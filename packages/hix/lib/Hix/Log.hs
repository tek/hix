module Hix.Log where

import Control.Monad.Trans.Reader (ask)
import Exon (exon)
import Text.PrettyPrint (Doc)

import Hix.Console (color, withChevrons)
import Hix.Data.LogLevel (LogLevel (..))
import qualified Hix.Data.Monad
import Hix.Data.Monad (M (M))
import Hix.Handlers.Tui (TuiHandlers (..))

data LogWith r =
  LogWith {
    trace :: r,
    debug :: r,
    verbose :: r,
    info :: r,
    warn :: r,
    error :: r
  }

type LogFor a = LogWith (a -> M ())

logWith ::
  (LogLevel -> r) ->
  LogWith r
logWith f =
  LogWith {
    trace = f LogTrace,
    debug = f LogDebug,
    verbose = f LogVerbose,
    info = f LogInfo,
    warn = f LogWarn,
    error = f LogError
  }

logFor ::
  (LogLevel -> a -> M ()) ->
  LogFor a
logFor f =
  LogWith {
    trace = f LogTrace,
    debug = f LogDebug,
    verbose = f LogVerbose,
    info = f LogInfo,
    warn = f LogWarn,
    error = f LogError
  }

decorate :: Text -> LogLevel -> Text
decorate msg = \case
  LogTrace -> [exon|[#{color 3 "trace"}] #{msg}|]
  LogDebug -> [exon|[#{color 6 "debug"}] #{msg}|]
  LogVerbose -> withChevrons 4 msg
  LogInfo -> withChevrons 5 msg
  LogWarn -> withChevrons 3 msg
  LogError -> withChevrons 1 [exon|Error: #{msg}|]

log :: LogLevel -> Text -> M ()
log level msg = do
  env <- M ask
  env.tui.log level msg

plain :: LogFor Text
plain = logFor log

logDecorated :: LogLevel -> Text -> M ()
logDecorated level msg =
  log level (decorate msg level)

decorated :: LogFor Text
decorated = logFor logDecorated

logCont :: LogLevel -> Text -> M ()
logCont level msg =
  log level [exon|    #{msg}|]

cont :: LogFor Text
cont = logFor logCont

-- | Print the head with decorations and the tail with indent.
logHangL :: LogLevel -> NonEmpty Text -> M ()
logHangL level (h :| t) = do
  logDecorated level h
  traverse_ (logCont level) t

logHang :: LogLevel -> Text -> [Text] -> M ()
logHang level h t = logHangL level (h :| t)

hang :: LogWith (Text -> [Text] -> M ())
hang = logWith logHang

trace :: Text -> M ()
trace = decorated.trace

traceP :: Doc -> M ()
traceP = trace . show

debug :: Text -> M ()
debug = decorated.debug

debugP :: Doc -> M ()
debugP = debug . show

verbose :: Text -> M ()
verbose = decorated.verbose

info :: Text -> M ()
info = decorated.info

infoP :: Doc -> M ()
infoP = info . show

warn :: Text -> M ()
warn = decorated.warn

error :: Text -> M ()
error = decorated.error
