module Hix.Data.LowerConfig where

import qualified Hix.Data.Options
import Hix.Data.Options (LowerOptions (LowerOptions))

data LowerConfig =
  LowerConfig {
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
      maxFailedPre = 99,
      maxFailedPost = 0,
      maxIterations = 3,
      oldest = False
    }

data LowerInitConfig =
  LowerInitConfig {
    reset :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default LowerInitConfig where
  def = LowerInitConfig {reset = False}
