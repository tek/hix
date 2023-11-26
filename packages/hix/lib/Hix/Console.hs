module Hix.Console where

import qualified Data.ByteString.Char8 as ByteString
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

hPrintBS ::
  MonadIO m =>
  Handle ->
  ByteString ->
  m ()
hPrintBS h msg =
  liftIO (ByteString.hPutStrLn h msg)

out :: MonadIO m => Text -> m ()
out = hPrint stdout

err :: MonadIO m => Text -> m ()
err = hPrint stderr

bytesOut :: MonadIO m => ByteString -> m ()
bytesOut = hPrintBS stdout

bytesErr :: MonadIO m => ByteString -> m ()
bytesErr = hPrintBS stderr

sgi :: Text -> Text
sgi param =
  [exon|#{esc}[#{param}m|]
  where
    esc = "\ESC"

sgis :: [Text] -> Text -> Text
sgis params chunk =
  [exon|#{seqs}#{chunk}#{sgi "0"}|]
  where
    seqs = mconcat (sgi <$> params)

color :: Int -> Text -> Text
color n =
  sgis [show (30 + n)]

withChevrons :: Int -> Text -> Text
withChevrons col msg =
  [exon|#{sgis [show (30 + col), "1"] ">>>"} #{msg}|]

errorMessage :: Text -> Text
errorMessage msg =
  withChevrons 1 [exon|Error: #{msg}|]
