module Hix.Log where

import Control.Monad.Trans.Reader (asks)
import Exon (exon)

import qualified Hix.Console as Console
import Hix.Console (color, sgis)
import qualified Hix.Data.Monad
import Hix.Data.Monad (M)

withChevrons :: Int -> Text -> Text
withChevrons col msg =
  [exon|#{sgis [show (30 + col), "1"] ">>>"} #{msg}|]

verbose :: Text -> M ()
verbose msg =
  whenM (asks (.verbose)) do
    Console.out (withChevrons 4 msg)

debug :: Text -> M ()
debug msg =
  whenM (asks (.debug)) do
    Console.err [exon|[#{color 6 "debug"}] #{msg}|]

info :: Text -> M ()
info msg =
  unlessM (asks (.quiet)) do
    Console.out (withChevrons 5 msg)

warn :: Text -> M ()
warn msg =
  unlessM (asks (.quiet)) do
    Console.out (withChevrons 3 msg)

infoCont :: Text -> M ()
infoCont msg =
  unlessM (asks (.quiet)) do
    Console.out [exon|    #{msg}|]

error ::
  MonadIO m =>
  Text ->
  m ()
error msg =
  Console.err (withChevrons 1 [exon|Error: #{msg}|])
