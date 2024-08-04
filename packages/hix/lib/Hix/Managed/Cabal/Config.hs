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
  )
import qualified Hix.Managed.Cabal.Data.HackageLocation as HackageLocation
import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation (auth), HackagePassword (HackagePassword), HackageUser)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName, HackageRepo (..), centralName)
import Hix.Managed.Cabal.HackageLocation (parseLocation)
import Hix.Managed.Cabal.HackageRepo (hackageDescription)
import Hix.Managed.Data.Mutable (MutableDep, depName)
import Hix.Monad (appContext, clientError, eitherClient, noteClient, tryIOM)

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

resolvePasswordEnvVar :: Text -> M HackagePassword
resolvePasswordEnvVar name =
  lookup >>= \case
    [] -> clientError (message "is empty")
    value -> pure (HackagePassword (toText value))
  where
    lookup = noteClient (message "does not exist") =<< tryIOM (lookupEnv (toString name))

    message problem = [exon|The specified environment variable #{Color.cyan name} #{problem}|]

resolvePasswordExec :: Text -> M HackagePassword
resolvePasswordExec spec =
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
          | [pw] <- Text.lines (decodeUtf8 output) -> pure (HackagePassword pw)
          | otherwise -> failure "printed multiple lines"
        Right (ExitFailure code, _) -> failure [exon|exited with code #{show @_ @Int code}|]
        Left err -> do
          Log.debug [exon|Subprocess error: #{toText (displayException err)}|]
          failure [exon|caused an exception (pass #{Color.cyan @Text "--debug"} to see it)|]

    failure = clientError . message

    message problem = [exon|The specified executable #{Color.path spec} #{problem}|]

resolvePassword :: ContextHackagePassword -> M HackagePassword
resolvePassword =
  appContext ctx . \case
    PasswordPlain pw -> pure pw
    PasswordEnvVar name -> resolvePasswordEnvVar name
    PasswordExec path -> resolvePasswordExec path
  where
    ctx = "resolving the password"

withAuth ::
  HackageLocation ->
  Maybe HackageUser ->
  Maybe ContextHackagePassword ->
  M HackageLocation
withAuth location = \cases
  (Just user) Nothing ->
    onlyOne [exon|user (##{user})|] "password"
  Nothing (Just _) ->
    onlyOne "password" "user"
  Nothing Nothing ->
    pure location
  (Just user) (Just passwordSpec) -> do
    password <- resolvePassword passwordSpec
    pure location {auth = Just (user, password)}
  where
    onlyOne present absent = clientError [exon|Specified a #{present}, but no #{absent}|]

validateContextRepo :: ContextHackageRepo -> M HackageRepo
validateContextRepo ContextHackageRepo {location = location0, ..} = do
  appContext [exon|validating the Hackage config #{Color.yellow name}|] do
    location1 <- for location0 \ (ContextHackageLocation spec) ->
      eitherClient (first toText (parseLocation (toString spec)))
    location <- withAuth (fromMaybe HackageLocation.central location1) user password
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
