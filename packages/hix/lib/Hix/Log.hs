module Hix.Log where

import Control.Monad.Trans.Reader (ask)
import Exon (exon)
import Text.PrettyPrint (Doc)

import Hix.Console (color, withChevrons)
import Hix.Data.LogLevel (LogLevel (..))
import qualified Hix.Data.Monad
import Hix.Data.Monad (M (M))

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
  env.logger level msg

logDecorated :: LogLevel -> Text -> M ()
logDecorated level msg =
  log level (decorate msg level)

trace :: Text -> M ()
trace = logDecorated LogTrace

traceP :: Doc -> M ()
traceP = trace . show

debug :: Text -> M ()
debug = logDecorated LogDebug

debugP :: Doc -> M ()
debugP = debug . show

verbose :: Text -> M ()
verbose = logDecorated LogVerbose

info :: Text -> M ()
info = logDecorated LogInfo

infoP :: Doc -> M ()
infoP = info . show

infoPlain :: Text -> M ()
infoPlain msg =
  log LogInfo msg

infoCont :: Text -> M ()
infoCont msg =
  log LogInfo [exon|    #{msg}|]

warn :: Text -> M ()
warn = logDecorated LogWarn

error :: Text -> M ()
error = logDecorated LogError

logWith :: (LogLevel -> Text -> IO ()) -> LogLevel -> Text -> M ()
logWith handler level msg = do
  env <- M ask
  let
    minDebug = env.logLevel >= LogDebug
    minVerbose = env.logLevel >= LogVerbose
    minInfo = env.logLevel >= LogInfo
  case level of
    LogTrace | env.logLevel >= LogTrace -> accept
    LogDebug | minDebug -> accept
    LogVerbose | minVerbose -> accept
    LogInfo | minInfo -> accept
    LogWarn | minInfo -> accept
    LogError -> accept
    _ -> unit
  where
    accept = liftIO (handler level msg)
