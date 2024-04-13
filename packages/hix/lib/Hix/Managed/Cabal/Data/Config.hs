module Hix.Managed.Cabal.Data.Config where

import Data.Aeson (FromJSON (parseJSON))
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Verbosity (Verbosity, verbose)
import Path (Abs, Dir, Path)

import Hix.Managed.Cabal.Data.Packages (GhcPackages)

newtype HackageRepoName =
  HackageRepoName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

instance Default HackageRepoName where
  def = "hackage.haskell.org"

newtype GhcPath =
  GhcPath (Path Abs Dir)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON)

data GhcDb =
  GhcDbSystem (Maybe GhcPath)
  |
  GhcDbSynthetic GhcPackages
  deriving stock (Eq, Show, Generic)

instance FromJSON GhcDb where
  parseJSON = fmap GhcDbSystem . parseJSON

newtype HackageIndexState =
  HackageIndexState Timestamp
  deriving stock (Eq, Show, Generic)

data CabalConfig =
  CabalConfig {
    indexState :: Maybe HackageIndexState
  }
  deriving stock (Eq, Show, Generic)

instance Default CabalConfig where
  def =
    CabalConfig {indexState = Nothing}

data SolveConfig =
  SolveConfig {
    hackageRepoName :: HackageRepoName,
    verbosity :: Verbosity,
    ghc :: Maybe GhcPath,
    allowBoot :: Bool,
    cabal :: CabalConfig
  }
  deriving stock (Eq, Show, Generic)

instance Default SolveConfig where
  def =
    SolveConfig {
      hackageRepoName = def,
      verbosity = verbose,
      ghc = Nothing,
      allowBoot = False,
      cabal = def
    }
