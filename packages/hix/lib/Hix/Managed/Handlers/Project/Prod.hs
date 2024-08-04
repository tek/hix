module Hix.Managed.Handlers.Project.Prod where

import Distribution.Pretty (pretty)
import Text.PrettyPrint (hang, vcat)

import Hix.Data.Monad (M)
import qualified Hix.Log as Log
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import qualified Hix.Managed.Handlers.Report.Prod as Report
import qualified Hix.Managed.Handlers.StateFile.Prod as StateFile

handlersProd ::
  StateFileConfig ->
  M ProjectHandlers
handlersProd stateFileConf = do
  traceInputs
  pure ProjectHandlers {
    stateFile = StateFile.handlersProd stateFileConf,
    report = Report.handlersProd
  }
  where
    traceInputs =
      Log.traceP (hang "Creating project handlers from config:" 2 (vcat [pretty stateFileConf]))
