module Hix.Managed.Data.NixOutput where

import Distribution.Pretty (Pretty (..))

import Hix.Data.PackageId (PackageId)

data Derivation =
  Derivation {
    path :: Text,
    log :: (Seq Text, Seq Text)
  }
  deriving stock (Eq, Show, Generic)

data PackageDerivation =
  PackageDerivation {
    package :: PackageId,
    success :: Bool,
    log :: [Text]
  }
  deriving stock (Eq, Show, Generic)

instance Pretty PackageDerivation where
  pretty PackageDerivation {package} = pretty package

data BuildsState =
  BuildsState {
    id :: Integer,
    done :: Int,
    failed :: Int,
    unassigned :: [Bool]
  }
  deriving stock (Eq, Show, Generic)

data OutputState =
  OutputState {
    builds :: Maybe BuildsState,
    running :: Map Integer Derivation,
    finished :: [PackageDerivation],
    messages :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

data OutputResult =
  OutputResult {
    failedPackages :: Maybe (NonEmpty PackageDerivation),
    unknownMessages :: [Text]
  }
  deriving stock (Eq, Show, Generic)
