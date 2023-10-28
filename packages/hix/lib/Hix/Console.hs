module Hix.Console where

import qualified Data.Text.IO as Text
import Exon (exon)
import System.IO (Handle, stderr, stdout)

hPrint ::
  MonadIO m =>
  Handle ->
  Text ->
  m ()
hPrint h msg =
  liftIO (Text.hPutStrLn h msg)

out :: MonadIO m => Text -> m ()
out = hPrint stdout

err :: MonadIO m => Text -> m ()
err = hPrint stderr

hMessage ::
  MonadIO m =>
  Handle ->
  Text ->
  m ()
hMessage h msg =
  hPrint h [exon|#{esc}[5m>>>#{esc}[0m #{msg}|]
  where
    esc = "\ESC"

info :: MonadIO m => Text -> m ()
info = hMessage stdout

error :: MonadIO m => Text -> m ()
error = hMessage stderr
