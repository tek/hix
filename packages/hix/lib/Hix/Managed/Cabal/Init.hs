{-# language CPP #-}

module Hix.Managed.Cabal.Init where

import Distribution.Client.Config (defaultCacheDir)
import Distribution.Client.GlobalFlags (GlobalFlags (..), defaultGlobalFlags)
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags)
import Distribution.Client.ProjectFlags (flagIgnoreProject)
import Distribution.Client.Types (RemoteRepo (..), RepoName (RepoName))
import Distribution.Simple.Program (defaultProgramDb)
import Distribution.Simple.Setup (ConfigFlags (..), defaultConfigFlags, maybeToFlag, toFlag)
import Distribution.Utils.NubList (toNubList)
import Exon (exon)
import Path (reldir, relfile, toFilePath, (</>))

import Hix.Data.Error (ErrorMessage (Fatal))
import qualified Hix.Managed.Cabal.Data.Config
import Hix.Managed.Cabal.Data.Config (GhcPath (GhcPath), SolveConfig)
import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation (..), hackageTlsBool)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName (..), HackageRepo (..))
import Hix.Managed.Cabal.HackageLocation (hackageLocationUri)
import Hix.Managed.Cabal.Repo (ensureHackageIndex)
import Hix.Monad (M, tryIOMWith)

#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.Setup (CommonSetupFlags (..))
#endif

data SolveFlags =
  SolveFlags {
    global :: GlobalFlags,
    main :: NixStyleFlags ()
  }

emptySolveFlags :: SolveFlags
emptySolveFlags =
  SolveFlags {
    global = defaultGlobalFlags {globalConfigFile = toFlag "/dev/null"},
    main = defaultNixStyleFlags ()
  }

remoteRepo :: HackageRepo -> RemoteRepo
remoteRepo HackageRepo {name, location, secure, keys} =
  RemoteRepo {
    remoteRepoName = RepoName (toString @Text (coerce name)),
    remoteRepoURI = hackageLocationUri location,
    remoteRepoSecure = secure,
    remoteRepoRootKeys = foldMap (fmap toString . toList) keys,
    remoteRepoKeyThreshold = 0,
    remoteRepoShouldTryHttps = hackageTlsBool location.tls
  }

globalFlags :: NonEmpty RemoteRepo -> FilePath -> GlobalFlags
globalFlags repos cacheDir =
  defaultGlobalFlags {
    -- Cabal *always* reads ~/.cabal/config if no file is specified, and crashes if the file doesn't exist
    globalConfigFile = toFlag "/dev/null",
    globalCacheDir = toFlag cacheDir,
    globalRemoteRepos = toNubList (toList repos)
  }

badCacheDir :: Text -> ErrorMessage
badCacheDir err = Fatal [exon|Cannot access Cabal cache dir: #{err}|]

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


#if MIN_VERSION_Cabal(3,14,0)
    configFlags =
      configDef {
        configHcPath = pathFlag [relfile|ghc|],
        configHcPkg = pathFlag [relfile|ghc-pkg|],
        configCommonFlags = configDef.configCommonFlags {
          setupVerbosity = toFlag conf.verbosity
        }
      }

    configDef = defaultConfigFlags defaultProgramDb
#else
    configFlags =
      (defaultConfigFlags defaultProgramDb) {
        configHcPath = pathFlag [relfile|ghc|],
        configHcPkg = pathFlag [relfile|ghc-pkg|],
        configVerbosity = toFlag conf.verbosity
      }
#endif

    pathFlag exe = maybeToFlag (ghcPath exe <$> conf.ghc)

    ghcPath exe (GhcPath dir) = toFilePath (dir </> [reldir|bin|] </> exe)

solveFlags ::
  NonEmpty RemoteRepo ->
  SolveConfig ->
  M SolveFlags
solveFlags repos conf = do
  cacheDir <- tryIOMWith badCacheDir defaultCacheDir
  let global = globalFlags repos cacheDir
  let main = mainFlags conf
  pure SolveFlags {global, main}

initialize ::
  SolveConfig ->
  M SolveFlags
initialize conf = do
  flags <- solveFlags (remoteRepo <$> conf.hackageRepos) conf
  ensureHackageIndex conf flags.global flags.main
  pure flags
