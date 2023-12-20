module Hix.Managed.Lower.App where

import Hix.Data.Error (Error (Client))
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig, lowerConfig)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (BuildOutputsPrefix, EnvsConfig)
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (LowerOptions)
import Hix.Json (jsonConfigE)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedConfig
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers, SpecialLowerHandlers (TestLowerHandlers))
import qualified Hix.Managed.Handlers.Lower.Prod as Lower
import qualified Hix.Managed.Handlers.Lower.Test as Lower
import Hix.Managed.Lower.Data.Lower (Lower)
import qualified Hix.Managed.Lower.Data.LowerMode
import Hix.Managed.Lower.Data.LowerMode (LowerMode, lowerInitMode, lowerOptimizeMode, lowerStabilizeMode)
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

lowerCli ::
  LowerMode ->
  (LowerHandlers -> LowerConfig -> ManagedApp -> M (BuildResults Lower)) ->
  LowerOptions ->
  M ()
lowerCli mode main opts = do
  env <- jsonConfigE Client opts.env
  handlers <- chooseHandlers opts env.envs conf env.buildOutputsPrefix
  runManagedApp handlers.build handlers.report env opts.managed mode.operation \ app ->
    Right <$> main handlers conf app
  where
    conf = lowerConfig opts

lowerInitCli :: LowerOptions -> M ()
lowerInitCli = lowerCli lowerInitMode lowerInit

lowerOptimizeCli :: LowerOptions -> M ()
lowerOptimizeCli = lowerCli lowerOptimizeMode lowerOptimize

lowerStabilizeCli :: LowerOptions -> M ()
lowerStabilizeCli = lowerCli lowerStabilizeMode lowerStabilize
