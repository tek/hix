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
