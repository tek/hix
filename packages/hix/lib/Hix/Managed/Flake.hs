module Hix.Managed.Flake where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as ByteString
import Exon (exon)
import Path (Abs, Dir, Path, toFilePath)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (ProcessConfig, proc, readProcess, setWorkingDir)

import Hix.Data.Monad (M)
import qualified Hix.Log as Log
import Hix.Monad (eitherFatal)

outLines :: ([ByteString] -> a) -> ByteString -> a
outLines f bs = f (ByteString.lines bs)

runFlakeFor ::
  ∀ a stdin stdout stderr .
  (ByteString -> Either Text a) ->
  (ByteString -> Either Text a) ->
  Text ->
  Path Abs Dir ->
  [Text] ->
  (ProcessConfig () () () -> ProcessConfig stdin stdout stderr) ->
  M a
runFlakeFor processOutput processError desc root args confProc = do
  Log.debug [exon|Running flake: #{unwords args}|]
  readProcess conf >>= \case
    (ExitSuccess, stdout, _) ->
      eitherFatal (first decodeError (processOutput (toStrict stdout)))
    (ExitFailure {}, _, stderr) ->
      eitherFatal (first failureError (processError (toStrict stderr)))
  where
    conf =
      confProc $
      setWorkingDir (toFilePath root) $
      proc "nix" (toString <$> args)

    decodeError msg = [exon|#{desc} produced invalid output: #{msg}|]
    failureError msg = [exon|#{desc} terminated with error: #{msg}|]

flakeFailure :: ByteString -> Either Text a
flakeFailure stderr = Left [exon|stderr: #{decodeUtf8 stderr}|]

runFlake ::
  ∀ a stdin stdout stderr .
  FromJSON a =>
  Text ->
  Path Abs Dir ->
  [Text] ->
  (ProcessConfig () () () -> ProcessConfig stdin stdout stderr) ->
  M a
runFlake =
  runFlakeFor success flakeFailure
  where
    success stdout = first toText (Aeson.eitherDecodeStrict' stdout)

runFlakeForSingleLine ::
  ∀ stdin stdout stderr .
  Text ->
  Path Abs Dir ->
  [Text] ->
  (ProcessConfig () () () -> ProcessConfig stdin stdout stderr) ->
  M ByteString
runFlakeForSingleLine =
  runFlakeFor success flakeFailure
  where
    success stdout = case ByteString.lines stdout of
      [ln] -> Right ln
      lns -> Left [exon|Expected a single line of output, got #{show (length lns)}|]

runFlakeRaw ::
  ∀ stdin stdout stderr .
  Text ->
  Path Abs Dir ->
  [Text] ->
  (ProcessConfig () () () -> ProcessConfig stdin stdout stderr) ->
  M ByteString
runFlakeRaw =
  runFlakeFor Right flakeFailure
