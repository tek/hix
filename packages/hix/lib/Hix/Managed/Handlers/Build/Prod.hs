module Hix.Managed.Handlers.Build.Prod where

import Control.Monad.Catch (catch)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (runStateT), gets, modify')
import qualified Data.ByteString.Char8 as ByteString
import Data.List.Extra (firstJust)
import qualified Data.Set as Set
import Exon (exon)
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (copyDirRecur')
import System.IO (BufferMode (LineBuffering), Handle, hSetBuffering)
import System.IO.Error (IOError, tryIOError)
import System.Process.Typed (
  ExitCode (ExitFailure, ExitSuccess),
  ProcessConfig,
  createPipe,
  getStderr,
  inherit,
  nullStream,
  proc,
  setStderr,
  setStdout,
  setWorkingDir,
  waitExitCode,
  withProcessTerm,
  )
import System.Timeout (timeout)

import Hix.Class.Map (nInsert, nMember)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error, ErrorMessage (Fatal))
import qualified Hix.Data.Monad
import Hix.Data.Monad (LogLevel (LogDebug), M, appRes)
import Hix.Data.Overrides (Overrides)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Versions)
import Hix.Error (pathText)
import Hix.Http (httpManager)
import qualified Hix.Log as Log
import Hix.Managed.Build.NixOutput (OutputResult (OutputResult), PackageDerivation (..), outputParse, runOutputState)
import Hix.Managed.Build.NixOutput.Analysis (analyzeLog)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForVersions), allHackages)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Data.StageState (BuildFailure (..), BuildResult (..), buildUnsuccessful)
import Hix.Managed.Data.Targets (firstMTargets)
import qualified Hix.Managed.Handlers.AvailableVersions.Prod as AvailableVersions
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers (..), Builder (Builder), EnvBuilder (EnvBuilder))
import Hix.Managed.Handlers.Cabal (CabalHandlers)
import qualified Hix.Managed.Handlers.Cabal.Prod as CabalHandlers
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import Hix.Managed.Handlers.SourceHash (SourceHashHandlers)
import qualified Hix.Managed.Handlers.SourceHash.Prod as SourceHashHandlers
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.Overrides (packageOverride, packageOverrides)
import Hix.Managed.StateFile (writeBuildStateFor, writeInitialEnvState)
import Hix.Monad (shouldLog, throwM, withLowerTry', withTempDir)
import Hix.Pretty (showP)

data BuilderResources =
  BuilderResources {
    hackage :: SourceHashHandlers,
    stateFileHandlers :: StateFileHandlers,
    envsConf :: Envs EnvConfig,
    root :: Path Abs Dir,
    buildConfig :: BuildConfig
  }

data EnvBuilderResources =
  EnvBuilderResources {
    global :: BuilderResources,
    context :: EnvContext
  }

withTempProject ::
  (Path Abs Dir -> M a) ->
  M a
withTempProject use = do
  projectRoot <- appRes.root
  catch
    do
      withTempDir "managed-build" \ tmpRoot -> do
        copyDirRecur' projectRoot tmpRoot
        use tmpRoot
    \ (err :: IOError) -> throwM (Fatal (show err))

data OutputConfig =
  OutputDebug
  |
  OutputParse
  |
  OutputIgnore

data NixProcResult =
  NixProcSuccess [Text]
  |
  NixProcFailure Text
  deriving stock (Eq, Show, Generic)

outputLines ::
  MonadIO m =>
  (ByteString -> m ()) ->
  Handle ->
  m (Maybe Text)
outputLines parse handle = do
  liftIO (hSetBuffering handle LineBuffering)
  spin mempty
  where
    spin buf = do
      liftIO (tryIOError (ByteString.hGet handle 4096)) >>= \case
        Left err -> pure (Just (show err))
        Right new ->
          join <$> for (completeLines buf new) \ (lns, newBuf) -> do
            for_ lns parse
            spin newBuf

    completeLines = \cases
      -- Empty output means the handle was closed, empty buffer means we can terminate
      "" "" ->
        Nothing
      -- First iteration after handle was closed, need to emit the last line
      buf "" ->
        Just ([buf], "")
      buf new ->
        Just (breakNl [] (buf <> new))

    breakNl acc s =
      case ByteString.break (== '\n') s of
        (rest, "") -> (reverse acc, rest)
        (ln, rest) -> breakNl (ln : acc) (ByteString.drop 1 rest)

nixProc ::
  OutputConfig ->
  Path Abs Dir ->
  [Text] ->
  Text ->
  [Text] ->
  M (ProcessConfig () () (Maybe Handle))
nixProc output root cmd installable extra = do
  Log.debug [exon|Running nix at '#{pathText root}' with args #{show args}|]
  pure conf
  where
    conf = err (setWorkingDir (toFilePath root) (proc "nix" args))

    err = case output of
      OutputParse -> setStderr (Just <$> createPipe)
      OutputDebug -> setStdout inherit . setStderr (Nothing <$ inherit)
      OutputIgnore -> setStderr (Nothing <$ nullStream)

    args = toString <$> cmd ++ [exon|path:#{".#"}#{installable}|] : extra ++ logArgs

    logArgs = case output of
      OutputParse -> ["--log-format", "internal-json"]
      OutputDebug -> ["-L"]
      OutputIgnore -> []

runProc ::
  BuildConfig ->
  (Handle -> M a) ->
  ProcessConfig () () (Maybe Handle) ->
  M (Maybe (Maybe (Either Error a), ExitCode))
runProc buildConf pipeHandler conf =
  withLowerTry' \ lower -> withProcessTerm conf \ prc -> do
    limit do
      output <- traverse (lower . pipeHandler) (getStderr prc)
      res <- waitExitCode prc
      pure (output, res)
  where
    limit | Just t <- buildConf.timeout
          , t > 0
          = timeout (coerce t * 1_000_000)
          | otherwise = fmap Just

outputResult ::
  Maybe (Either Error (Maybe Text, OutputResult)) ->
  ExitCode ->
  M BuildResult
outputResult result = \case
  ExitSuccess -> pure BuildSuccess
  ExitFailure _ -> pure (BuildFailure (maybe UnknownFailure PackageFailure failedPackage))
  where
    failedPackage =
      result >>= \case
        Right (_, OutputResult pkg) -> pkg
        Left _ -> Nothing

buildTarget ::
  BuildConfig ->
  Path Abs Dir ->
  EnvName ->
  LocalPackage ->
  M BuildResult
buildTarget buildConf root env target = do
  debug <- shouldLog LogDebug
  conf <- nixProc (outputHandler debug) root ["build", "--no-link"] [exon|env.##{env}.##{target}|] []
  runProc buildConf runOutput conf >>= \case
    Just (output, code) ->
      outputResult output code
    Nothing -> pure (BuildFailure (TimeoutFailure []))
  where
    runOutput handle =
      runOutputState (outputLines outputParse handle)

    outputHandler debug
      | buildConf.buildOutput || (buildConf.disableNixMonitor && debug)
      = OutputDebug
      | buildConf.disableNixMonitor
      = OutputIgnore
      | otherwise
      = OutputParse

buildAdaptive ::
  EnvBuilderResources ->
  Bool ->
  LocalPackage ->
  StateT (Overrides, Set PackageId) M BuildResult
buildAdaptive EnvBuilderResources {global, context} allowRevisions target = do
  build
  where
    build = do
      overrides <- gets fst
      result <- lift do
        writeBuildStateFor "current build" global.stateFileHandlers global.root context overrides
        buildTarget global.buildConfig global.root context.env target
      checkResult overrides result

    checkResult overrides result
      | allowRevisions
      , BuildFailure (PackageFailure pkgs) <- result
      , Just package <- firstJust logFailure (toList pkgs)
      , not (nMember package.name overrides)
      = retry package
      | otherwise
      = pure result

    logFailure PackageDerivation {package, log} =
      package <$ analyzeLog log

    retry broken = do
      lift $ Log.verbose [exon|Installed package failed with bounds error, retrying with override: #{showP broken}|]
      newOverride <- lift $ packageOverride global.hackage broken
      modify' \ (overrides, revisions) -> (nInsert broken.name newOverride overrides, Set.insert broken revisions)
      build

buildWithState ::
  EnvBuilderResources ->
  Bool ->
  Versions ->
  [PackageId] ->
  M (BuildResult, (Overrides, Set PackageId))
buildWithState builder allowRevisions _ overrideVersions = do
  overrides <- packageOverrides builder.global.hackage overrideVersions
  let build = buildAdaptive builder allowRevisions
      s0 = (overrides, Set.empty)
  runStateT (firstMTargets BuildSuccess buildUnsuccessful build builder.context.targets) s0

-- | This used to have the purpose of reading an updated GHC package db using the current managed state, but this has
-- become obsolete.
--
-- TODO Decide whether to keep this for abstraction purposes.
withEnvBuilder ::
  ∀ a .
  BuilderResources ->
  CabalHandlers ->
  EnvContext ->
  Initial EnvState ->
  (EnvBuilder -> M a) ->
  M a
withEnvBuilder global cabal context initialState use = do
  writeInitialEnvState global.stateFileHandlers global.root context initialState
  use builder
  where
    builder =
      EnvBuilder {
        cabal,
        buildWithState = buildWithState resources
      }

    resources = EnvBuilderResources {..}

withBuilder ::
  SourceHashHandlers ->
  StateFileHandlers ->
  Envs EnvConfig ->
  BuildConfig ->
  (Builder -> M a) ->
  M a
withBuilder hackage stateFileHandlers envsConf buildConfig use = do
  withTempProject \ root -> do
    let resources = BuilderResources {..}
    use Builder {withEnvBuilder = withEnvBuilder resources}

handlersProd ::
  ProjectHandlers ->
  Envs EnvConfig ->
  BuildConfig ->
  CabalConfig ->
  M BuildHandlers
handlersProd project envsConf buildConfig cabalConf = do
  manager <- httpManager
  hackage <- SourceHashHandlers.handlersProd (allHackages cabalConf)
  versionsHackages <- HackageClient.handlersProdFor (Just manager) ForVersions cabalConf
  pure BuildHandlers {
    project,
    cabal = CabalHandlers.handlersProd cabalConf False,
    withBuilder = withBuilder hackage project.stateFile envsConf buildConfig,
    versions = AvailableVersions.handlersProd versionsHackages
  }
