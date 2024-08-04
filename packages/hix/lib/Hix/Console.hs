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

data ColorOffsets =
  ColorOffsets {
    black :: Int,
    red :: Int,
    green :: Int,
    yellow :: Int,
    blue :: Int,
    magenta :: Int,
    cyan :: Int,
    white :: Int
  }
  deriving stock (Eq, Show, Generic)

colors :: ColorOffsets
colors =
  ColorOffsets {
    black = 0,
    red = 1,
    green = 2,
    yellow = 3,
    blue = 4,
    magenta = 5,
    cyan = 6,
    white = 7
  }

withChevrons :: Int -> Text -> Text
withChevrons col msg =
  [exon|#{sgis [show (30 + col), "1"] ">>>"} #{msg}|]

withErrorChevrons :: Text -> Text
withErrorChevrons = withChevrons 1

errorMessage :: Text -> Text
errorMessage msg =
  withErrorChevrons [exon|Error: #{msg}|]
