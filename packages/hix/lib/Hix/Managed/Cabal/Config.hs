module Hix.Managed.Cabal.Config where

import Control.Exception (displayException)
import Control.Monad (foldM)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Exon (exon)
import qualified Path
import Path (SomeBase (..), toFilePath)
import qualified Path.IO as Path
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO.Error (tryIOError)
import qualified System.Process.Typed as Process

import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Data.Options (CabalOptions (..))
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (PackageName)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.Config (CabalConfig (..))
import Hix.Managed.Cabal.Data.ContextHackageRepo (
  ContextHackageLocation (..),
  ContextHackagePassword (..),
  ContextHackageRepo (..),
  ContextHackageSecret (..),
  ContextHackageToken (..),
  )
import qualified Hix.Managed.Cabal.Data.HackageLocation as HackageLocation
import Hix.Managed.Cabal.Data.HackageLocation (
  HackageAuth (..),
  HackageLocation (auth),
  HackagePassword (..),
  HackageSecret (..),
  HackageToken (..),
  HackageUser,
  )
import Hix.Managed.Cabal.Data.HackageRepo (HackageName, HackageRepo (..), centralName)
import Hix.Managed.Cabal.HackageLocation (parseLocation)
import Hix.Managed.Cabal.HackageRepo (hackageDescription)
import Hix.Managed.Data.Mutable (MutableDep, depName)
import Hix.Monad (appContextVerbose, clientError, eitherClient, noteClient, tryIOM)

nonReinstallableNames :: Set PackageName
nonReinstallableNames =
  [
    "base",
    "ghc-bignum",
    "ghc-prim",
    "ghc",
    "integer-gmp",
    "integer-simple",
    "template-haskell"
  ]

isNonReinstallable :: PackageName -> Bool
isNonReinstallable = flip Set.member nonReinstallableNames

isReinstallable :: PackageName -> Bool
isReinstallable = not . isNonReinstallable

isNonReinstallableId :: PackageId -> Bool
isNonReinstallableId package = isNonReinstallable package.name

isReinstallableId :: PackageId -> Bool
isReinstallableId package = isReinstallable package.name

isNonReinstallableDep :: MutableDep -> Bool
isNonReinstallableDep = isNonReinstallable . depName

resolveSecretEnvVar :: Text -> M HackageSecret
resolveSecretEnvVar name =
  lookup >>= \case
    [] -> clientError (message "is empty")
    value -> pure (HackageSecret (toText value))
  where
    lookup = noteClient (message "does not exist") =<< tryIOM (lookupEnv (toString name))

    message problem = [exon|The specified environment variable #{Color.cyan name} #{problem}|]

resolveSecretExec :: Text -> M HackageSecret
resolveSecretExec spec =
  noteClient (message "is not a valid path") (Path.parseSomeFile (toString spec)) >>= \case
    Abs path -> checkPath path
    Rel rel ->
      liftIO (tryIOError (Path.findExecutable rel)) >>= \case
        Right (Just path) -> checkPath path
        _ -> failure [exon|could not be found in #{Color.cyan ("$PATH" :: Text)}|]
  where
    checkPath path =
      liftIO (tryIOError (Path.getPermissions path)) >>= \case
        Right perms | Path.executable perms -> exec path
                    | otherwise -> failure "is not an executable file"
        Left _ -> failure "could not be read"

    exec path =
      liftIO (tryIOError (Process.readProcessStdout (Process.proc (toFilePath path) []))) >>= \case
        Right (ExitSuccess, output)
          | LByteString.null output -> failure "printed nothing on stdout"
          | [secret] <- Text.lines (decodeUtf8 output) -> pure (HackageSecret secret)
          | otherwise -> failure "printed multiple lines"
        Right (ExitFailure code, _) -> failure [exon|exited with code #{show @_ @Int code}|]
        Left err -> do
          Log.debug [exon|Subprocess error: #{toText (displayException err)}|]
          failure [exon|caused an exception (pass #{Color.cyan @Text "--debug"} to see it)|]

    failure = clientError . message

    message problem = [exon|The specified executable #{Color.path spec} #{problem}|]

resolveSecret :: ContextHackageSecret -> M HackageSecret
resolveSecret =
  appContextVerbose ctx . \case
    SecretUnobscured secret -> pure secret
    SecretPlain secret -> pure secret
    SecretEnvVar name -> resolveSecretEnvVar name
    SecretExec path -> resolveSecretExec path
  where
    ctx = "resolving the password"

withAuth ::
  HackageLocation ->
  Maybe HackageUser ->
  Maybe ContextHackagePassword ->
  Maybe ContextHackageToken ->
  M HackageLocation
withAuth location = \cases
  _ (Just _) (Just _) ->
    bothSorts
  (Just user) Nothing _ ->
    onlyOne [exon|user (##{user})|] "password"
  Nothing (Just _) Nothing ->
    onlyOne "password" "user"
  Nothing Nothing Nothing ->
    pure location
  (Just user) (Just passwordSpec) Nothing -> do
    secret <- resolveSecret passwordSpec.secret
    pure location {auth = Just (HackageAuthPassword {user, password = HackagePassword secret})}
  Nothing Nothing (Just tokenSpec) -> do
    secret <- resolveSecret tokenSpec.secret
    pure location {auth = Just (HackageAuthToken {token = HackageToken secret})}
  where
    onlyOne present absent = clientError [exon|Specified a #{present}, but no #{absent}|]

    bothSorts = clientError "Specified both password and auth token"

validateContextRepo :: ContextHackageRepo -> M HackageRepo
validateContextRepo ContextHackageRepo {location = location0, ..} = do
  appContextVerbose [exon|validating the Hackage config #{Color.yellow name}|] do
    location1 <- for location0 \ (ContextHackageLocation spec) ->
      eitherClient (first toText (parseLocation (toString spec)))
    location <- withAuth (fromMaybe HackageLocation.central location1) user password token
    pure HackageRepo {
      name,
      description = fromMaybe (hackageDescription location) description,
      enable = fromMaybe True enable,
      location,
      solver = fromMaybe True solver,
      publish = fromMaybe False publish,
      ..
    }

-- | Merge Hackage repos from different sources, prioritizing CLI arguments over flake config.
cabalConfig :: Map HackageName ContextHackageRepo -> CabalOptions -> M CabalConfig
cabalConfig contextRepos CabalOptions {hackage = cliOverrides} = do
  amended <- foldM applyOverride contextRepos cliOverrides
  valid <- traverse validateContextRepo amended
  pure CabalConfig {
    hackageMain = valid !? centralName,
    hackageExtra = Map.elems (Map.delete centralName valid)
  }
  where
    applyOverride repos (name :: HackageName, override) = do
      repo <- noteClient [exon|Invalid Hackage repo name: ##{name}|] (repos !? name)
      pure (Map.insert name (override repo) repos)
