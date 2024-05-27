module Hix.Managed.Data.BuildConfig where

data BuildConfig =
  BuildConfig {
    maxIterations :: Natural,
    maxFailedPre :: Natural,
    maxFailedPost :: Natural,
    lookup :: Bool,
    validate :: Bool,
    buildOutput :: Bool,
    toposortMutations :: Bool
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
    toposortMutations = True
  }
