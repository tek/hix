module Hix.Managed.Build.NixProcess where

import qualified Data.ByteString.Char8 as ByteString
import Exon (exon)
import Path (Abs, Dir, Path, toFilePath)
import System.IO (BufferMode (LineBuffering), Handle, hSetBuffering)
import System.IO.Error (tryIOError)
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
  withProcessTerm, getStdout, byteStringOutput,
  )
import System.Timeout (timeout)

import Hix.Data.Error (Error)
import Hix.Data.Monad (LogLevel (LogDebug), M)
import Hix.Error (pathText)
import qualified Hix.Log as Log
import Hix.Managed.Build.NixOutput (OutputResult (..), outputParse, runOutputState)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.StageState (BuildFailure (..), BuildResult (..))
import Hix.Monad (shouldLog, withLowerTry')
import Control.Concurrent.STM (atomically, STM)
import qualified Data.Text as Text

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
  M (ProcessConfig () (STM LByteString) (Maybe Handle))
nixProc output root cmd installable extra = do
  Log.debug [exon|Running nix at '#{pathText root}' with args #{show args}|]
  pure conf
  where
    conf =
      err $
      setStdout byteStringOutput $
      setWorkingDir (toFilePath root) $
      proc "nix" args

    err = case output of
      OutputParse -> setStderr (Just <$> createPipe)
      OutputDebug -> setStderr (Nothing <$ inherit)
      OutputIgnore -> setStderr (Nothing <$ nullStream)

    args = toString <$> cmd ++ [exon|path:#{".#"}#{installable}|] : extra ++ logArgs

    logArgs = case output of
      OutputParse -> ["--log-format", "internal-json"]
      OutputDebug -> ["-L"]
      OutputIgnore -> []

runProc ::
  BuildConfig ->
  (Handle -> M a) ->
  ProcessConfig () (STM LByteString) (Maybe Handle) ->
  M (Maybe (Maybe (Either Error a), [Text], ExitCode))
runProc buildConf pipeHandler conf =
  withLowerTry' \ lower -> withProcessTerm conf \ prc -> do
    limit do
      result <- traverse (lower . pipeHandler) (getStderr prc)
      res <- waitExitCode prc
      stdout <- atomically (getStdout prc)
      pure (result, Text.lines (decodeUtf8 stdout), res)
  where
    limit | Just t <- buildConf.timeout
          , t > 0
          = timeout (coerce t * 1_000_000)
          | otherwise = fmap Just

outputResult ::
  Maybe (Either Error (Maybe Text, OutputResult)) ->
  [Text] ->
  ExitCode ->
  BuildResult
outputResult result stdout = \case
  ExitSuccess -> BuildSuccess stdout
  ExitFailure _ -> BuildFailure (maybe UnknownFailure failure result)
  where
    failure = \case
      Right (unhandled, OutputResult {unknownMessages, failedPackages}) ->
        let alt = maybe UnknownFailure UnexpectedFailure (nonEmpty unknownMessages <> (pure <$> unhandled))
        in maybe alt PackageFailure failedPackages
      Left err ->
        AppFailure err

nixBuild ::
  BuildConfig ->
  Path Abs Dir ->
  Text ->
  M BuildResult
nixBuild buildConf root attr = do
  debug <- shouldLog LogDebug
  conf <- nixProc (outputHandler debug) root args [exon|__hix-internal__.#{attr}|] []
  runProc buildConf runOutput conf <&> \case
    Just (result, stdout, code) ->
      outputResult result stdout code
    Nothing -> BuildFailure (TimeoutFailure [])
  where
    args = ["build", "--no-link", "--show-trace", "--print-out-paths"]

    runOutput handle =
      runOutputState (outputLines outputParse handle)

    outputHandler debug
      | buildConf.buildOutput || (buildConf.disableNixMonitor && debug)
      = OutputDebug
      | buildConf.disableNixMonitor
      = OutputIgnore
      | otherwise
      = OutputParse
