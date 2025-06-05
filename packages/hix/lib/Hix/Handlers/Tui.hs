module Hix.Handlers.Tui where

import Brick (App)
import Brick.BChan (BChan)

import Hix.Data.LogLevel (LogLevel)

newtype RunBrick m =
  RunBrick (∀ n e s . Ord n => App s e n -> Maybe (BChan e) -> s -> m s)

data TuiHandlers m =
  TuiHandlers {
    log :: LogLevel -> Text -> m (),
    runBrick :: RunBrick m
  }
  deriving stock (Generic)

handlersNull ::
  Applicative m =>
  TuiHandlers m
handlersNull =
  TuiHandlers {
    log = \ _ _ -> unit,
    runBrick = RunBrick \ _ _ s -> pure s
  }
