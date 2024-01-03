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

verbose :: Text -> M ()
verbose msg =
  log LogVerbose (withChevrons 4 msg)

debug :: Text -> M ()
debug msg =
  log LogDebug [exon|[#{color 6 "debug"}] #{msg}|]

debugP :: Doc -> M ()
debugP = debug . show

info :: Text -> M ()
info msg =
  log LogInfo (withChevrons 5 msg)

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
    minVerbose = env.verbose || env.debug
    minInfo = minVerbose || not env.quiet
  case level of
    LogDebug | env.debug -> accept
    LogVerbose | minVerbose -> accept
    LogInfo | minInfo -> accept
    LogWarn | minInfo -> accept
    LogError -> accept
    _ -> unit
  where
    accept = liftIO (handler level msg)
