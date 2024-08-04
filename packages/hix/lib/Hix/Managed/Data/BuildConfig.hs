module Hix.Managed.Data.BuildConfig where

import Data.Aeson (FromJSON)

newtype BuildTimeout =
  BuildTimeout Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, FromJSON)

data SpecialBuildHandlers =
  BuildHandlersTestBump
  |
  BuildHandlersTestMaint
  deriving stock (Eq, Show)

data BuildConfig =
  BuildConfig {
    maxIterations :: Natural,
    maxFailedPre :: Natural,
    maxFailedPost :: Natural,
    lookup :: Bool,
    validate :: Bool,
    buildOutput :: Bool,
    toposortMutations :: Bool,
    timeout :: Maybe BuildTimeout,
    disableNixMonitor :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default BuildConfig where
  def = BuildConfig {
    maxIterations = 3,
    maxFailedPre = 99,
    maxFailedPost = 0,
    lookup = False,
    validate = False,
    buildOutput = False,
    toposortMutations = True,
    timeout = Nothing,
    disableNixMonitor = False
  }
