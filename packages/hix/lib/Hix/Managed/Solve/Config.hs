module Hix.Managed.Solve.Config where

import Distribution.Verbosity (Verbosity, verbose)
import Path (Abs, Dir, Path)

newtype HackageRepoName =
  HackageRepoName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

instance Default HackageRepoName where
  def = "hackage.haskell.org"

data SolveConfig =
  SolveConfig {
    hackageRepoName :: HackageRepoName,
    verbosity :: Verbosity,
    ghc :: Maybe (Path Abs Dir),
    allowBoot :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default SolveConfig where
  def =
    SolveConfig {
      hackageRepoName = def,
      verbosity = verbose,
      ghc = Nothing,
      allowBoot = False
    }
