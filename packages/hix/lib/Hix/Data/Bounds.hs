module Hix.Data.Bounds where

import Data.Aeson (FromJSON (parseJSON))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version, VersionRange, hasLowerBound, hasUpperBound)
import GHC.Exts (IsList)

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Class.Map (LookupMaybe, LookupMonoid, NtMap, convert1, ntAmend1, ntInsert, ntPretty, ntPretty1, via)
import qualified Hix.Data.Dep
import Hix.Data.Deps (TargetRemoteDeps)
import Hix.Data.Json (jsonParsec)
import Hix.Data.PackageName (LocalPackage, PackageName)

newtype Bounds =
  Bounds (Map PackageName VersionRange)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList, EncodeNix)

instance NtMap Bounds PackageName VersionRange LookupMaybe where

instance Pretty Bounds where
  pretty = ntPretty

instance FromJSON Bounds where
  parseJSON v =
    Bounds . fmap jsonParsec <$> parseJSON v

newtype TargetBounds =
  TargetBounds (Map LocalPackage Bounds)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

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

-- | The dependencies for which the flake specifies a bound corresponding to 'targetBound', but the preexisting managed
-- state doesn't.
--
-- This is used to inform the user for which dependencies it is safe to remove the bounds after a run.
-- We could examine only the specified bounds, but that would mean that we would make that suggestion after every run if
-- the user decided to keep the bounds in the flake anyway.
data RemovableBounds =
  RemovableBounds {
    targetBound :: TargetBound,
    deps :: Map LocalPackage (Set PackageName)
  }
  deriving stock (Eq, Show, Generic)

removableBounds :: TargetBound -> TargetRemoteDeps -> TargetBounds -> RemovableBounds
removableBounds targetBound configDeps managedBounds =
  RemovableBounds {
    targetBound,
    deps = matchingNames <$> ntAmend1 unmanaged managedBounds specified
  }
  where
    matchingNames = Set.fromList . Map.keys . Map.filter id

    specified = convert1 (predicate . (.version)) configDeps

    unmanaged range spec = not (predicate range) && spec

    predicate | TargetUpper <- targetBound = hasUpperBound
              | otherwise = hasLowerBound
