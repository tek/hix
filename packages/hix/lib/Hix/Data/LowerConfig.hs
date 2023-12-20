module Hix.Data.LowerConfig where

import qualified Hix.Data.Options
import Hix.Data.Options (LowerOptions (LowerOptions))

data LowerConfig =
  LowerConfig {
    initOnly :: Bool,
    reset :: Bool,
    maxFailedPre :: Natural,
    maxFailedPost :: Natural,
    maxIterations :: Natural,
    oldest :: Bool
  }
  deriving stock (Eq, Show, Generic)

lowerConfig :: LowerOptions -> LowerConfig
lowerConfig LowerOptions {..} =
  LowerConfig {oldest = False, ..}

instance Default LowerConfig where
  def =
    LowerConfig {
      initOnly = False,
      reset = False,
      maxFailedPre = 99,
      maxFailedPost = 0,
      maxIterations = 3,
      oldest = False
    }
