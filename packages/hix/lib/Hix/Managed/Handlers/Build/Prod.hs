module Hix.Managed.Handlers.Build.Prod where

import Control.Monad.Catch (catch)
import Control.Monad.Trans.Reader (asks)
import Data.Foldable.Extra (allM)
import Exon (exon)
import Path (Abs, Dir, File, Path, parseAbsDir, toFilePath)
import Path.IO (copyDirRecur')
import System.IO.Error (IOError)
import System.Process.Typed (
  ExitCode (ExitFailure, ExitSuccess),
  ProcessConfig,
  inherit,
  nullStream,
  proc,
  readProcessStdout,
  runProcess,
  setStderr,
  setStdout,
  setWorkingDir,
  )

import Hix.Class.Map ((!!))
import Hix.Data.Deps (TargetDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Fatal))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (BuildOutputsPrefix (BuildOutputsPrefix), EnvsConfig, ManagedState)
import qualified Hix.Data.Monad (Env (verbose))
import Hix.Data.PackageName (LocalPackage)
import Hix.Error (pathText)
import qualified Hix.Log as Log
import Hix.Managed.Data.BuildState (BuildStatus, buildStatus)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Data.Targets (Targets (Targets))
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers (..), Builder (Builder), EnvBuilder (EnvBuilder))
import qualified Hix.Managed.Handlers.Hackage.Prod as HackageHandlers
import qualified Hix.Managed.Handlers.StateFile
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import qualified Hix.Managed.Handlers.StateFile.Prod as StateFileHandlers
import Hix.Managed.Path (rootOrCwd)
import Hix.Managed.Solve.Config (GhcDb (GhcDb))
import Hix.Managed.StateFile (writeBuildStateFor)
import Hix.Monad (M, noteFatal, throwM, tryIOM, withTempDir)

data BuilderResources =
  BuilderResources {
    handlers :: StateFileHandlers,
    stateFileConf :: StateFileConfig,
    envsConf :: EnvsConfig,
    buildOutputsPrefix :: Maybe BuildOutputsPrefix,
    root :: Path Abs Dir,
    stateFile :: Path Abs File
  }

data EnvBuilderResources =
  EnvBuilderResources {
    global :: BuilderResources,
    env :: EnvName,
    targets :: Targets,
    deps :: TargetDeps
  }

withTempProject ::
  Maybe (Path Abs Dir) ->
  (Path Abs Dir -> M a) ->
  M a
withTempProject rootOverride use = do
  projectRoot <- rootOrCwd rootOverride
  catch
    do
      withTempDir "managed-build" \ tmpRoot -> do
        copyDirRecur' projectRoot tmpRoot
        use tmpRoot
    \ (err :: IOError) -> throwM (Fatal (show err))

nixProc ::
  Path Abs Dir ->
  [Text] ->
  Text ->
  [Text] ->
  M (ProcessConfig () () ())
nixProc root cmd installable extra = do
  Log.debug [exon|Running nix at '#{pathText root}' with args #{show args}|]
  conf <$> asks (.verbose)
  where
    conf verbose = err verbose (setWorkingDir (toFilePath root) (proc "nix" args))

    err = \case
      True -> setStderr inherit
      False -> setStderr nullStream

    args = toString <$> cmd ++ [exon|path:#{".#"}#{installable}|] : extra

buildPackage ::
  Path Abs Dir ->
  EnvName ->
  LocalPackage ->
  M Bool
buildPackage root env target = do
  verbose <- asks (.verbose)
  conf <- nixProc root ["-L", "build"] [exon|env.##{env}.##{target}|] []
  tryIOM (runProcess (err verbose conf)) <&> \case
    ExitSuccess -> True
    ExitFailure _ -> False
  where
    err = \case
      True -> setStdout inherit
      False -> id

buildWithState ::
  EnvBuilderResources ->
  ManagedState ->
  M BuildStatus
buildWithState EnvBuilderResources {targets = Targets targets, ..} state = do
  writeBuildStateFor "current build" global.handlers deps env state global.stateFile
  buildStatus <$> allM (buildPackage global.root env) targets

ghcDb ::
  EnvBuilderResources ->
  M (Maybe GhcDb)
ghcDb EnvBuilderResources {global, env} = do
  conf <- nixProc global.root ["eval", "--raw"] [exon|#{prefix}.env.##{env}.ghc-local|] []
  tryIOM (readProcessStdout conf) >>= \case
    (ExitSuccess, decodeUtf8 -> out) -> Just . GhcDb <$> noteFatal (parseError out) (parseAbsDir out)
    (ExitFailure _, _) -> throwM (Fatal "Evaluation failed for vanilla GHC path")
  where
    BuildOutputsPrefix prefix = fromMaybe "build" global.buildOutputsPrefix
    parseError out = [exon|Parse error for vanilla GHC path: #{toText out}|]

-- | This writes the initial state separate from the individual builds because we want to load the GHC package db for
-- the Cabal solver at the start of @use@.
-- Since we allow multiple envs to be processed in sequence, the state will be updated between runs, which influences
-- the package db (due to local packages getting new dependency bounds).
--
-- TODO should EnvBuilder provide a solver constructor?
withEnvBuilder ::
  ∀ a .
  BuilderResources ->
  EnvName ->
  Targets ->
  TargetDeps ->
  ManagedState ->
  (EnvBuilder -> M a) ->
  M a
withEnvBuilder global env targets deps initialState use = do
  writeBuildStateFor "env initialization" global.handlers deps env initialState global.stateFile
  let resources = EnvBuilderResources {..}
  use EnvBuilder {buildWithState = buildWithState resources, ghcDb = db resources}
  where
    db _ = case (.ghc) =<< global.envsConf !! env of
      Just path -> pure (Just path)
      Nothing -> throwM (Fatal "No GHC package db in the config")

withBuilder ::
  StateFileHandlers ->
  StateFileConfig ->
  EnvsConfig ->
  Maybe BuildOutputsPrefix ->
  (Builder -> M a) ->
  M a
withBuilder handlers stateFileConf envsConf buildOutputsPrefix use =
  withTempProject stateFileConf.projectRoot \ root -> do
    stateFile <- handlers.initFile root stateFileConf.file
    let resources = BuilderResources {..}
    use Builder {withEnvBuilder = withEnvBuilder resources}

handlersProd ::
  StateFileConfig ->
  EnvsConfig ->
  Maybe BuildOutputsPrefix ->
  IO BuildHandlers
handlersProd stateFileConf envsConf buildOutputsPrefix = do
  hackage <- HackageHandlers.handlersProd
  let stateFile = StateFileHandlers.handlersProd
  pure BuildHandlers {
    stateFile,
    hackage,
    withBuilder = withBuilder stateFile stateFileConf envsConf buildOutputsPrefix
  }
