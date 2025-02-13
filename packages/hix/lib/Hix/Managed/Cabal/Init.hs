{-# language CPP #-}

module Hix.Managed.Cabal.Init where

import Distribution.Client.Config (defaultCacheDir)
import Distribution.Client.GlobalFlags (GlobalFlags (..), defaultGlobalFlags)
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags)
import Distribution.Client.ProjectFlags (flagIgnoreProject)
import Distribution.Client.Types (RemoteRepo (..))
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Program (defaultProgramDb)
import Distribution.Simple.Setup (ConfigFlags (..), defaultConfigFlags, maybeToFlag, toFlag)
import Distribution.Utils.NubList (toNubList)
import Exon (exon)
import Path (reldir, relfile, toFilePath, (</>))

import Hix.Data.Error (Error (Fatal))
import qualified Hix.Managed.Cabal.Data.Config
import Hix.Managed.Cabal.Data.Config (GhcPath (GhcPath), HackageRepoName, SolveConfig)
import Hix.Managed.Cabal.Repo (ensureHackageIndex)
import Hix.Monad (M, throwM, tryIOMWith)

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

initialize ::
  SolveConfig ->
  M SolveFlags
initialize conf = do
  hackage <- hackageRepo conf.hackageRepoName
  cacheDir <- tryIOMWith badCacheDir defaultCacheDir
  let global = globalFlags hackage cacheDir
  let main = mainFlags conf
  ensureHackageIndex conf global main
  pure SolveFlags {global, main}
