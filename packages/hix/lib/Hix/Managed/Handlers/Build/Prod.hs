module Hix.Managed.Handlers.Build.Prod where

import Control.Monad.Catch (catch)
import Control.Monad.Trans.Reader (ask, asks)
import qualified Data.ByteString.Char8 as ByteString
import Exon (exon)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
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

import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Fatal))
import qualified Hix.Data.Monad
import Hix.Data.Monad (M (M))
import Hix.Data.Overrides (Overrides)
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Versions)
import Hix.Error (pathText)
import Hix.Hackage (latestVersionHackage, versionsHackage)
import qualified Hix.Log as Log
import Hix.Managed.Build.NixOutput (OutputResult (OutputResult), outputParse, runOutputState)
import Hix.Managed.Cabal.Data.Config (CabalConfig)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext (EnvContext))
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Data.StageState (BuildFailure (..), BuildResult (..), buildUnsuccessful)
import qualified Hix.Managed.Data.StateFileConfig
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Data.Targets (firstMTargets)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers (..), BuildOutputsPrefix, Builder (Builder), EnvBuilder (EnvBuilder))
import Hix.Managed.Handlers.Cabal (CabalHandlers)
import qualified Hix.Managed.Handlers.Cabal.Prod as CabalHandlers
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Hackage.Prod as HackageHandlers
import qualified Hix.Managed.Handlers.Report.Prod as ReportHandlers
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import qualified Hix.Managed.Handlers.StateFile.Prod as StateFileHandlers
import Hix.Managed.Overrides (packageOverrides)
import Hix.Managed.Path (rootOrCwd)
import Hix.Managed.StateFile (writeBuildStateFor, writeInitialEnvState)
import Hix.Monad (runMUsing, throwM, tryIOM, withTempDir)

data BuilderResources =
  BuilderResources {
    hackage :: HackageHandlers,
    stateFileHandlers :: StateFileHandlers,
    envsConf :: Envs EnvConfig,
    buildOutputsPrefix :: Maybe BuildOutputsPrefix,
    root :: Path Abs Dir,
    buildConfig :: BuildConfig
  }

data EnvBuilderResources =
  EnvBuilderResources {
    global :: BuilderResources,
    context :: EnvContext
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
  (Handle -> IO a) ->
  ProcessConfig () () (Maybe Handle) ->
  M (Maybe (Maybe a, ExitCode))
runProc buildConf pipeHandler conf =
  tryIOM (withProcessTerm conf interact)
  where
    interact prc =
      limit do
        err <- traverse pipeHandler (getStderr prc)
        res <- waitExitCode prc
        pure (err, res)

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
  appRes <- M ask
  debug <- M (asks (.debug))
  conf <- nixProc (outputHandler debug) root ["build"] [exon|env.##{env}.##{target}|] []
  runProc buildConf (runOutput appRes) conf >>= \case
    Just (output, code) ->
      outputResult output code
    Nothing -> pure (BuildFailure (TimeoutFailure Nothing))
  where
    runOutput appRes handle =
      runMUsing appRes (runOutputState (outputLines outputParse handle))

    outputHandler debug
      | buildConf.buildOutput || (buildConf.disableNixMonitor && debug)
      = OutputDebug
      | buildConf.disableNixMonitor
      = OutputIgnore
      | otherwise
      = OutputParse

buildWithState ::
  EnvBuilderResources ->
  Versions ->
  [PackageId] ->
  M (Overrides, BuildResult)
buildWithState EnvBuilderResources {global, context = context@EnvContext {env, targets}} _ overrideVersions = do
  overrides <- packageOverrides global.hackage overrideVersions
  writeBuildStateFor "current build" global.stateFileHandlers global.root context overrides
  result <- firstMTargets BuildSuccess buildUnsuccessful (buildTarget global.buildConfig global.root env) targets
  pure (overrides, result)

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
  HackageHandlers ->
  StateFileHandlers ->
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  BuildConfig ->
  (Builder -> M a) ->
  M a
withBuilder hackage stateFileHandlers stateFileConf envsConf buildOutputsPrefix buildConfig use =
  withTempProject stateFileConf.projectRoot \ root -> do
    let resources = BuilderResources {..}
    use Builder {withEnvBuilder = withEnvBuilder resources}

handlersProd ::
  MonadIO m =>
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  BuildConfig ->
  CabalConfig ->
  Bool ->
  m BuildHandlers
handlersProd stateFileConf envsConf buildOutputsPrefix buildConfig cabalConf oldest = do
  manager <- liftIO (newManager tlsManagerSettings)
  hackage <- HackageHandlers.handlersProd
  let stateFile = StateFileHandlers.handlersProd stateFileConf
  pure BuildHandlers {
    stateFile,
    report = ReportHandlers.handlersProd,
    cabal = CabalHandlers.handlersProd cabalConf oldest,
    withBuilder = withBuilder hackage stateFile stateFileConf envsConf buildOutputsPrefix buildConfig,
    versions = versionsHackage manager,
    latestVersion = latestVersionHackage manager
  }
