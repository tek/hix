module Hix.Data.NewProjectConfig where

import Path (Dir)

import Hix.Data.PathSpec (PathSpec)

newtype ProjectName =
  ProjectName { unProjectName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

newtype HixUrl =
  HixUrl { unHixUrl :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

instance Default HixUrl where
  def = "github:tek/hix"

newtype Author =
  Author { unAuthor :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

newtype ProjectDirectory =
  ProjectDirectory { unProjectDirectory :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data CreateProjectConfig =
  CreateProjectConfig {
    packages :: Bool,
    hixUrl :: HixUrl,
    author :: Author,
    noInitGitAndFlake :: Bool,
    devCli :: Bool
  }
  deriving stock (Eq, Show, Generic)

data InitProjectConfig =
  InitProjectConfig {
    name :: ProjectName,
    config :: CreateProjectConfig
  }
  deriving stock (Eq, Show, Generic)

data NewProjectConfig =
  NewProjectConfig {
    directory :: PathSpec Dir,
    name :: Maybe ProjectName,
    printDirectory :: Bool,
    config :: CreateProjectConfig
  }
  deriving stock (Eq, Show, Generic)
