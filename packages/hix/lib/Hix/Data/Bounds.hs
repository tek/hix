module Hix.Data.Bounds where

import Data.Aeson (FromJSON (parseJSON))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version, VersionRange, hasLowerBound, hasUpperBound)
import GHC.Exts (IsList)

import Hix.Class.Map (LookupMaybe, LookupMonoid, NtMap, ntInsert, ntMap, ntPretty, ntPretty1, via)
import qualified Hix.Data.Dep
import Hix.Data.Deps (TargetDeps)
import Hix.Data.Json (jsonParsec)
import Hix.Data.Package (LocalPackage, PackageName)

newtype Bounds =
  Bounds (Map PackageName VersionRange)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NtMap Bounds PackageName VersionRange LookupMaybe where

instance Pretty Bounds where
  pretty = ntPretty

instance FromJSON Bounds where
  parseJSON v =
    Bounds . fmap jsonParsec <$> parseJSON v

newtype TargetBounds =
  TargetBounds (Map LocalPackage Bounds)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList)

instance NtMap TargetBounds LocalPackage Bounds LookupMonoid where

instance Pretty TargetBounds where
  pretty = ntPretty1

updateBounds :: LocalPackage -> PackageName -> VersionRange -> TargetBounds -> TargetBounds
updateBounds target package range =
  via $ Map.adjust (ntInsert package range) target

data BoundExtension =
  LowerBoundExtension Version
  |
  UpperBoundExtension Version
  deriving stock (Eq, Show, Generic)

newtype BoundExtensions =
  BoundExtensions (Map PackageName BoundExtension)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NtMap BoundExtensions PackageName BoundExtension LookupMaybe where

data TargetBound =
  TargetLower
  |
  TargetUpper
  deriving stock (Eq, Show, Generic)

instance Pretty TargetBound where
  pretty = \case
    TargetLower -> "lower"
    TargetUpper -> "upper"

data UninitializedBounds =
  UninitializedBounds {
    targetBound :: TargetBound,
    deps :: Map LocalPackage (Set PackageName)
  }
  deriving stock (Eq, Show, Generic)

filterUninitialized :: TargetBound -> TargetDeps -> UninitializedBounds
filterUninitialized targetBound targetDeps =
  UninitializedBounds {
    targetBound,
    deps = Set.fromList . mapMaybe hasNoTargetBound . toList . ntMap <$> ntMap targetDeps
  }
  where
    hasNoTargetBound dep | predicate dep.version = Nothing
                         | otherwise = Just dep.package
    predicate | TargetUpper <- targetBound = hasUpperBound
              | otherwise = hasLowerBound
