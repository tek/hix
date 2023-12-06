module Hix.Managed.Lower.App where


import Hix.Data.Bounds (TargetBound)
import Hix.Data.Error (Error (Client))
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig, LowerInitConfig (LowerInitConfig), lowerConfig)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (BuildOutputsPrefix)
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (LowerInitOptions, LowerOptions)
import Hix.Json (jsonConfigE)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build.Mutation (MutationResult (MutationFailed, MutationKeep))
import Hix.Managed.Data.Build (BuildResults)
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedConfig
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
  LowerConfig ->
  Maybe BuildOutputsPrefix ->
  M LowerHandlers
chooseHandlers opts conf buildOutputsPrefix =
  case opts.handlers of
    Just TestLowerHandlers -> Lower.handlersTest stateFileConf buildOutputsPrefix oldest
    Nothing -> Lower.handlersProd stateFileConf buildOutputsPrefix oldest
  where
    stateFileConf = opts.managed.stateFile
    oldest = conf.oldest

-- TODO move all the specific config from LowerConfig here
data LowerMode =
  LowerMode {
    targetBound :: TargetBound
  }
  deriving stock (Eq, Show, Generic)

lowerCli ::
  LowerMode ->
  LowerOptions ->
  LowerConfig ->
  (LowerHandlers -> ManagedApp -> M (BuildResults Lower)) ->
  M ()
lowerCli _ opts lowerConf main = do
  env <- jsonConfigE Client opts.env
  handlers <- chooseHandlers opts lowerConf env.buildOutputsPrefix
  runManagedApp handlers.build handlers.report env opts.managed \ app ->
    Right <$> main handlers app

lowerInitCli :: LowerInitOptions -> M ()
lowerInitCli opts =
  lowerCli undefined opts.common lowerConf \ handlers app ->
    lowerInit handlers lowerConf LowerInitConfig {reset = opts.reset} app
  where
    lowerConf = lowerConfig opts.common True MutationFailed

lowerOptimizeCli :: LowerOptions -> M ()
lowerOptimizeCli opts =
  lowerCli undefined opts lowerConf \ handlers app ->
    lowerOptimize handlers lowerConf app
  where
    lowerConf = lowerConfig opts False MutationKeep

lowerStabilizeCli :: LowerOptions -> M ()
lowerStabilizeCli opts =
  lowerCli undefined opts lowerConf \ handlers app ->
    lowerStabilize handlers lowerConf app
  where
    lowerConf = lowerConfig opts True MutationFailed
