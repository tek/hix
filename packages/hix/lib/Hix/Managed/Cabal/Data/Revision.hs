module Hix.Managed.Cabal.Data.Revision where

import Data.Aeson (FromJSON)
import Data.Time (UTCTime)

import Hix.Managed.Cabal.Data.HackageRepo (HackageDescription)

data Revision =
  Revision {
    number :: Word,
    sha256 :: Text,
    time :: UTCTime,
    user :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data RevisionPublished =
  RevisionPublished {
    revision :: Revision,
    destination :: HackageDescription
  }
  deriving stock (Eq, Show)
