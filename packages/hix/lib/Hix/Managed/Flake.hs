module Hix.Managed.Flake where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as ByteString
import Exon (exon)
import Path (Abs, Dir, Path, toFilePath)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (ProcessConfig, proc, readProcess, setWorkingDir)

import qualified Hix.Color as Color
import qualified Hix.Data.Monad as Monad
import Hix.Data.Monad (LogLevel (..), M, appRes)
import qualified Hix.Log as Log
import Hix.Maybe (fromMaybeA)
import Hix.Monad (eitherFatal, shouldLog)
import Hix.Error (pathText)

outLines :: ([ByteString] -> a) -> ByteString -> a
outLines f bs = f (ByteString.lines bs)

quiet3 :: [Text]
quiet3 = ["--quiet", "--quiet", "--quiet"]

data RunFlake a =
  RunFlake {
    processError :: ByteString -> Either Text a,
    cwd :: Maybe (Path Abs Dir),
    quiet :: Bool,
    confProc :: ProcessConfig () () () -> ProcessConfig () () ()
  }

runFlakeAt :: Path Abs Dir -> RunFlake a
runFlakeAt cwd = def {cwd = Just cwd}

flakeFailure :: ByteString -> Either Text a
flakeFailure stderr = Left [exon|stderr: #{decodeUtf8 stderr}|]

instance Default (RunFlake a) where
  def =
    RunFlake {
      processError = flakeFailure,
      cwd = Nothing,
      quiet = False,
      confProc = id
    }

runFlakeFatal ::
  ∀ a .
  Text ->
  [Text] ->
  RunFlake a ->
  (ByteString -> Either Text a) ->
  M a
runFlakeFatal desc args RunFlake {cwd = cwdSpec, ..} processOutput = do
  cwd <- fromMaybeA appRes.root cwdSpec
  verbose <- shouldLog LogVerbose
  Log.debug [exon|Running flake in #{Color.path (pathText cwd)} (#{desc}): #{Color.shellCommand (unwords args)}|]
  readProcess (conf cwd verbose) >>= \case
    (ExitSuccess, stdout, _) ->
      eitherFatal (first decodeError (processOutput (toStrict stdout)))
    (ExitFailure {}, _, stderr) ->
      eitherFatal (first failureError (processError (toStrict stderr)))
  where
    conf cwd verbose =
      confProc $
      setWorkingDir (toFilePath cwd) $
      proc "nix" (toString <$> (quietArgs ++ traceArg verbose ++ args))

    quietArgs = if quiet then quiet3 else []

    traceArg = \case
      True -> ["--show-trace"]
      False -> []

    decodeError msg = [exon|Flake command (#{desc}) produced invalid output: #{msg}|]
    failureError msg = [exon|Flake command (#{desc}) terminated with error: #{msg}|]

runFlake ::
  ∀ a .
  Text ->
  [Text] ->
  RunFlake a ->
  (ByteString -> Either Text a) ->
  M (Either Text a)
runFlake desc args conf processOutput =
  runFlakeFatal desc args conf {processError = pure . Left . decodeUtf8} (fmap Right . processOutput)

runFlakeJson ::
  ∀ a .
  FromJSON a =>
  Text ->
  [Text] ->
  M a
runFlakeJson desc args =
  runFlakeFatal desc args def \ stdout -> first toText (Aeson.eitherDecodeStrict' stdout)

runFlakeForSingleLine ::
  Text ->
  [Text] ->
  M ByteString
runFlakeForSingleLine desc args =
  runFlakeFatal desc args def \ stdout -> case ByteString.lines stdout of
    [ln] -> Right ln
    lns -> Left [exon|Expected a single line of output, got #{show (length lns)}|]

runFlakeSimple ::
  Text ->
  [Text] ->
  RunFlake () ->
  M ()
runFlakeSimple desc args conf =
  runFlakeFatal desc args conf {quiet = True} (const unit)

runFlakeLock :: RunFlake () -> M ()
runFlakeLock =
  runFlakeSimple "Create lock file" ["flake", "lock"]

runFlakeGen :: RunFlake () -> M ()
runFlakeGen =
  runFlakeSimple "Generate Cabal and overrides" ["run", ".#gen-quiet"]

runFlakeGenCabal :: RunFlake () -> M ()
runFlakeGenCabal =
  runFlakeSimple "Generate Cabal" ["run", ".#gen-cabal-quiet"]
