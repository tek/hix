module Hix.Log where

import Control.Monad.Trans.Reader (ask)
import Exon (exon)
import Text.PrettyPrint (Doc)

import Hix.Console (color, withChevrons)
import qualified Hix.Data.Monad
import Hix.Data.Monad (LogLevel (..), M (M))

log :: LogLevel -> Text -> M ()
log level msg = do
  env <- M ask
  env.logger level msg

trace :: Text -> M ()
trace msg =
  log LogTrace [exon|[#{color 3 "trace"}] #{msg}|]

traceP :: Doc -> M ()
traceP = trace . show

debug :: Text -> M ()
debug msg =
  log LogDebug [exon|[#{color 6 "debug"}] #{msg}|]

debugP :: Doc -> M ()
debugP = debug . show

verbose :: Text -> M ()
verbose msg =
  log LogVerbose (withChevrons 4 msg)

info :: Text -> M ()
info msg =
  log LogInfo (withChevrons 5 msg)

infoP :: Doc -> M ()
infoP = info . show

infoPlain :: Text -> M ()
infoPlain msg =
  log LogInfo msg

warn :: Text -> M ()
warn msg =
  log LogWarn (withChevrons 3 msg)

infoCont :: Text -> M ()
infoCont msg =
  log LogInfo [exon|    #{msg}|]

error :: Text -> M ()
error msg =
  log LogError (withChevrons 1 [exon|Error: #{msg}|])

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
