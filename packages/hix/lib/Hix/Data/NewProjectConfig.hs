module Hix.Data.NewProjectConfig where

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

data NewProjectConfig =
  NewProjectConfig {
    name :: ProjectName,
    packages :: Bool,
    hixUrl :: HixUrl,
    author :: Author
  }
  deriving stock (Eq, Show, Generic)
