module Hix.Managed.Solve.Init where

import Distribution.Client.CmdUpdate (updateAction)
import Distribution.Client.Config (defaultCacheDir)
import qualified Distribution.Client.GlobalFlags
import Distribution.Client.GlobalFlags (GlobalFlags (..), defaultGlobalFlags)
import Distribution.Client.IndexUtils (indexBaseName)
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags)
import Distribution.Client.ProjectFlags (flagIgnoreProject)
import Distribution.Client.Setup (withRepoContext)
import Distribution.Client.Types (RemoteRepo (..), Repo (RepoSecure), RepoName (RepoName))
import qualified Distribution.Client.Types.Repo
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Program (defaultProgramDb)
import Distribution.Simple.Setup (ConfigFlags (..), defaultConfigFlags, maybeToFlag, toFlag)
import Distribution.Utils.NubList (toNubList)
import Exon (exon)
import Path (addExtension, parseAbsFile, reldir, relfile, toFilePath, (</>))
import Path.IO (doesFileExist)

import Hix.Data.Error (Error (Fatal))
import Hix.Error (pathText)
import qualified Hix.Log as Log
import qualified Hix.Managed.Solve.Config
import Hix.Managed.Solve.Config (GhcDb (GhcDb), HackageRepoName (HackageRepoName), SolveConfig (SolveConfig))
import Hix.Monad (M, eitherFatalShow, noteFatal, throwM, tryIOM, tryIOMWith)

data SolveFlags =
  SolveFlags {
    global :: GlobalFlags,
    main :: NixStyleFlags ()
  }

hackageRepo ::
  HackageRepoName ->
  M RemoteRepo
hackageRepo repoName =
  case eitherParsec [exon|##{repoName}:http://hackage.haskell.org/|] of
    Right repo -> pure repo {remoteRepoSecure = Just True}
    Left err -> throwM (Fatal [exon|Parse error: #{toText err}|])

globalFlags :: RemoteRepo -> FilePath -> GlobalFlags
globalFlags hackage cacheDir =
  defaultGlobalFlags {
    -- Cabal *always* reads ~/.cabal/config if no file is specified, and crashes if the file doesn't exist
    globalConfigFile = toFlag "/dev/null",
    globalCacheDir = toFlag cacheDir,
    globalRemoteRepos = toNubList [hackage]
  }

badCacheDir :: Text -> Error
badCacheDir err = Fatal [exon|Cannot access Cabal cache dir: #{err}|]

fullHackageRepo ::
  SolveConfig ->
  GlobalFlags ->
  IO (Maybe Repo)
fullHackageRepo SolveConfig {verbosity, hackageRepoName = HackageRepoName (toString -> hackage)} flags =
  withRepoContext verbosity flags \ ctx ->
    pure (find isHackage ctx.repoContextRepos)
  where
    isHackage = \case
      RepoSecure {repoRemote = RemoteRepo {remoteRepoName = RepoName name}} -> name == hackage
      _ -> False

indexExists :: SolveConfig -> GlobalFlags -> M Bool
indexExists conf flags = do
  repo <- noteFatal err =<< liftIO (fullHackageRepo conf flags)
  indexPath <- eitherFatalShow err do
    base <- parseAbsFile (indexBaseName repo)
    addExtension ".tar" base
  Log.debug [exon|Checking hackage snapshot at #{pathText indexPath}|]
  tryIOM (doesFileExist indexPath)
  where
    err = "Bad Hackage repo config"

mainFlags ::
  SolveConfig ->
  NixStyleFlags ()
mainFlags conf =
  basic {
    configFlags,
    projectFlags = basic.projectFlags {flagIgnoreProject = toFlag True}
  }
  where
    basic = defaultNixStyleFlags ()

    configFlags =
      (defaultConfigFlags defaultProgramDb) {
        configHcPath = pathFlag [relfile|ghc|],
        configHcPkg = pathFlag [relfile|ghc-pkg|]
      }

    pathFlag exe = maybeToFlag (ghcPath exe <$> conf.ghc)

    ghcPath exe (GhcDb dir) = toFilePath (dir </> [reldir|bin|] </> exe)

initialize ::
  SolveConfig ->
  M SolveFlags
initialize conf = do
  hackage <- hackageRepo conf.hackageRepoName
  cacheDir <- tryIOMWith badCacheDir defaultCacheDir
  let global = globalFlags hackage cacheDir
  let main = mainFlags conf
  unlessM (indexExists conf global) do
    Log.verbose "Hackage snapshot doesn't exist, fetching..."
    tryIOMWith (\ err -> Fatal [exon|Fetching hackage snapshot failed: #{err}|]) (updateAction main [] global)
  pure SolveFlags {global, main}
