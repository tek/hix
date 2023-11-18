module Hix.Managed.Data.SolverBounds where

import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version, VersionRange, orEarlierVersion)
import GHC.Exts (IsList)

import qualified Hix.Class.Map as NtMap
import Hix.Class.Map (LookupMaybe, NtMap, ntFromList, ntPretty)
import Hix.Data.Bounds (Bounds (Bounds))
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Package (PackageName)
import Hix.Version (lowerBound)

data SolverBound =
  ExtendedBound Version
  |
  SpecifiedBounds VersionRange
  |
  NoBounds
  deriving stock (Eq, Show, Generic)

instance Pretty SolverBound where
  pretty = \case
    ExtendedBound version -> pretty version <> "[ext]"
    SpecifiedBounds range -> pretty range
    NoBounds -> "[no bounds]"

newtype SolverBounds =
  SolverBounds (Map PackageName SolverBound)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NtMap SolverBounds PackageName SolverBound LookupMaybe where

instance Pretty SolverBounds where
  pretty = ntPretty

fromConfig :: Bounds -> SolverBounds
fromConfig = NtMap.convert SpecifiedBounds

solverRanges :: SolverBounds -> Bounds
solverRanges (SolverBounds bounds) =
  Bounds (Map.mapMaybe convert bounds)
  where
    convert = \case
      ExtendedBound version -> Just (orEarlierVersion version)
      SpecifiedBounds range -> Just range
      NoBounds -> Nothing

noBounds :: [Dep] -> SolverBounds
noBounds =
  ntFromList . fmap \ dep -> (dep.package, NoBounds)

optimizeBounds :: [Dep] -> SolverBounds
optimizeBounds =
  ntFromList . fmap \ dep -> (dep.package, range dep)
  where
    range dep | Just bound <- lowerBound dep.version = ExtendedBound bound
              | otherwise = NoBounds
