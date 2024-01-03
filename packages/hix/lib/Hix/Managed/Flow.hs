module Hix.Managed.Flow (
  Flow,
  liftM,
  runStage,
  execStage,
  evalStage,
  runStage_,
  execStatelessStage,
  stageState,
  runFlow,
  execFlow,
  evalFlow,
  stageError,
) where

import Control.Lens ((%=))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, gets, runStateT)
import qualified Data.List.NonEmpty as NonEmpty

import Hix.Data.Error (Error (Fatal))
import Hix.Data.Monad (M)
import qualified Hix.Managed.Data.EnvContext
import qualified Hix.Managed.Data.EnvRequest
import Hix.Managed.Data.EnvRequest (EnvRequest (EnvRequest))
import qualified Hix.Managed.Data.EnvResult
import Hix.Managed.Data.EnvResult (EnvResult (EnvResult))
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.StageContext (StageContext)
import Hix.Managed.Data.StageResult (StageFailure (FailedPrecondition), StageResult (..), StageSummary (StageFailure))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Managed.StageContext (stageContext)
import Hix.Managed.UpdateState (stateWithMutations)
import Hix.Monad (throwM)

data FlowState =
  FlowState {
    env :: EnvRequest,
    current :: EnvState,
    summaries :: [StageSummary]
  }
  deriving stock (Generic)

type FlowM = StateT FlowState M

newtype Flow x =
  Flow (FlowM x)
  deriving newtype (Functor, Applicative, Monad)

liftM :: M a -> Flow a
liftM = Flow . lift

addResult ::
  StageResult ->
  Flow BuildStatus
addResult StageResult {state, summary} =
  Flow do
    #summaries %= (summary :)
    #current %= maybe id stateWithMutations state
    pure status
  where
    status = case summary of
      StageFailure _ -> Failure
      _ -> Success

newContext :: Flow StageContext
newContext = do
  FlowState {env, current} <- Flow get
  pure (stageContext env current)

runStage ::
  ∀ o .
  (StageContext -> M (StageResult, o)) ->
  Flow (BuildStatus, o)
runStage stage = do
  context <- newContext
  (result, o) <- liftM (stage context)
  status <- addResult result
  pure (status, o)

execStage ::
  (StageContext -> M StageResult) ->
  Flow BuildStatus
execStage stage =
  fst <$> runStage (fmap (, ()) . stage)

evalStage ::
  (StageContext -> M (StageResult, o)) ->
  Flow o
evalStage stage =
  snd <$> runStage stage

runStage_ ::
  (StageContext -> M StageResult) ->
  Flow ()
runStage_ stage =
  void (execStage stage)

execStatelessStage ::
  (StageContext -> M StageSummary) ->
  Flow BuildStatus
execStatelessStage f = do
  execStage \ context -> do
    summary <- f context
    pure StageResult {state = Nothing, summary}

stageError :: Text -> Flow ()
stageError msg =
  void (execStatelessStage (const (pure (StageFailure (FailedPrecondition [msg])))))

stageState :: (EnvState -> a) -> Flow a
stageState f = Flow (gets (f . (.current)))

runFlow :: EnvRequest -> Flow o -> M (o, EnvResult)
runFlow request@EnvRequest {context, state = (Initial initState)} (Flow m) =
  runStateT m FlowState {env = request, current = initState, summaries = []} >>= traverse \case
    FlowState {summaries = h : t, current = finalState} ->
      pure EnvResult {env = context.env, state = Just finalState, summaries = NonEmpty.reverse (h :| t)}
    FlowState {summaries = []} ->
      throwM (Fatal "Executed flow with no stages")

execFlow :: EnvRequest -> Flow () -> M EnvResult
execFlow env m =
  snd <$> runFlow env m

evalFlow :: EnvRequest -> Flow o -> M o
evalFlow env m =
  fst <$> runFlow env m
