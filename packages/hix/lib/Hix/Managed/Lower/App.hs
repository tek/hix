module Hix.Managed.Lower.App where

import Hix.Data.Bounds (TargetBound (TargetLower))
import Hix.Data.Error (Error (Client))
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig, LowerInitConfig (LowerInitConfig), lowerConfig)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (BuildOutputsPrefix, EnvsConfig)
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (LowerInitOptions, LowerOptions)
import Hix.Json (jsonConfigE)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build.Mutation (MutationResult (MutationFailed, MutationKeep))
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpLowerInit, OpLowerOptimize, OpLowerStabilize))
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers, SpecialLowerHandlers (TestLowerHandlers))
import qualified Hix.Managed.Handlers.Lower.Prod as Lower
import qualified Hix.Managed.Handlers.Lower.Test as Lower
import Hix.Managed.Lower.Data.Lower (Lower)
import Hix.Managed.Lower.Init (lowerInit)
import Hix.Managed.Lower.Optimize (lowerOptimize)
import Hix.Managed.Lower.Stabilize (lowerStabilize)

chooseHandlers ::
  LowerOptions ->
  EnvsConfig ->
  LowerConfig ->
  Maybe BuildOutputsPrefix ->
  M LowerHandlers
chooseHandlers opts envsConf conf buildOutputsPrefix =
  case opts.handlers of
    Just TestLowerHandlers -> Lower.handlersTest stateFileConf envsConf buildOutputsPrefix oldest
    Nothing -> Lower.handlersProd stateFileConf envsConf buildOutputsPrefix oldest
  where
    stateFileConf = opts.managed.stateFile
    oldest = conf.oldest

-- TODO move all the fields from LowerConfig here that have a static mapping from the modes
-- TODO move this out of ManagedApp so it can be set from @lowerInit@ etc. to avoid duplication in tests
data LowerMode =
  LowerMode {
    operation :: ManagedOp,
    targetBound :: TargetBound
  }
  deriving stock (Eq, Show, Generic)

lowerCli ::
  LowerMode ->
  LowerOptions ->
  LowerConfig ->
  (LowerHandlers -> ManagedApp -> M (BuildResults Lower)) ->
  M ()
lowerCli mode opts lowerConf main = do
  env <- jsonConfigE Client opts.env
  handlers <- chooseHandlers opts env.envs lowerConf env.buildOutputsPrefix
  runManagedApp handlers.build handlers.report env opts.managed mode.operation \ app ->
    Right <$> main handlers app

lowerInitCli :: LowerInitOptions -> M ()
lowerInitCli opts =
  lowerCli LowerMode {operation = OpLowerInit, targetBound = TargetLower} opts.common lowerConf \ handlers app ->
    lowerInit handlers lowerConf LowerInitConfig {reset = opts.reset} app
  where
    lowerConf = lowerConfig opts.common True MutationFailed

lowerOptimizeCli :: LowerOptions -> M ()
lowerOptimizeCli opts =
  lowerCli LowerMode {operation = OpLowerOptimize, targetBound = TargetLower} opts lowerConf \ handlers app ->
    lowerOptimize handlers lowerConf app
  where
    lowerConf = lowerConfig opts False MutationKeep

lowerStabilizeCli :: LowerOptions -> M ()
lowerStabilizeCli opts =
  lowerCli LowerMode {operation = OpLowerStabilize, targetBound = TargetLower} opts lowerConf \ handlers app ->
    lowerStabilize handlers lowerConf app
  where
    lowerConf = lowerConfig opts True MutationFailed
