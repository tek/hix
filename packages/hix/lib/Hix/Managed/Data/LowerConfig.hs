module Hix.Managed.Data.LowerConfig where

import qualified Hix.Data.Options
import Hix.Data.Options (LowerOptions (LowerOptions))

data LowerConfig =
  LowerConfig {
    initOnly :: Bool,
    reset :: Bool,
    stabilize :: Bool
  }
  deriving stock (Eq, Show, Generic)

lowerConfig :: LowerOptions -> LowerConfig
lowerConfig LowerOptions {..} =
  LowerConfig {..}

instance Default LowerConfig where
  def =
    LowerConfig {
      initOnly = False,
      reset = False,
      stabilize = False
    }
