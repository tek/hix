module Hix.Managed.Data.SolverParams where

import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (
  Version,
  VersionRange,
  anyVersion,
  intersectVersionRanges,
  orEarlierVersion,
  orLaterVersion,
  )
import GHC.Exts (IsList)
import Text.PrettyPrint (parens, (<+>))

import qualified Hix.Class.Map as NtMap
import Hix.Class.Map (LookupMonoid, NtMap, convert, ntFromList, ntPretty, (!!))
import Hix.Data.Bounds (Bounds)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Package (PackageName)
import Hix.Data.Version (Versions)
import Hix.Managed.Data.ManagedConfig (ManagedOp (..))
import Hix.Version (lowerBound, upperVersion)

-- | Left-biased semigroup op.
data BoundMutation =
  ExtendedBound Version
  |
  RetractedBound Version
  |
  NoBounds
  deriving stock (Eq, Show, Generic)

instance Semigroup BoundMutation where
  NoBounds <> r = r
  l <> NoBounds = l
  ExtendedBound _ <> r = r
  l <> _ = l

instance Pretty BoundMutation where
  pretty = \case
    ExtendedBound version -> pretty version <> "[ext]"
    RetractedBound version -> pretty version <> "[ret]"
    NoBounds -> "[original]"

-- | Left-biased semigroup op, but 'oldest' is combined by disjunction, since 'False' is the default.
--
-- TODO Should 'oldest' be 'Maybe' and be left-biased as well?
data PackageParams =
  PackageParams {
    oldest :: Bool,
    mutation :: BoundMutation,
    bounds :: Maybe VersionRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty PackageParams where
  pretty PackageParams {oldest, mutation, bounds} =
    pretty mutation <+> parens oldnew <+> foldMap spec bounds
    where
      oldnew = case oldest of
        True -> "oldest"
        False -> "newest"

      spec v = "+" <+> pretty v

instance Semigroup PackageParams where
  l <> r =
    PackageParams {
      oldest = l.oldest || r.oldest,
      mutation = l.mutation <> l.mutation,
      bounds = l.bounds <|> r.bounds
    }

instance Monoid PackageParams where
  mempty =
    PackageParams {
      oldest = False,
      mutation = NoBounds,
      bounds = Nothing
    }

newtype SolverParams =
  SolverParams (Map PackageName PackageParams)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Monoid, IsList)

instance Semigroup SolverParams where
  SolverParams l <> SolverParams r = SolverParams (Map.unionWith (<>) l r)

instance NtMap SolverParams PackageName PackageParams LookupMonoid where

instance Pretty SolverParams where
  pretty = ntPretty

fromConfig :: Bounds -> SolverParams
fromConfig = NtMap.convert \ range -> mempty {bounds = Just range}

originalBounds :: [Dep] -> SolverParams
originalBounds =
  ntFromList . fmap \ dep -> (dep.package, mempty)

keepBounds :: Versions -> [Dep] -> SolverParams
keepBounds keep =
  ntFromList . fmap \ dep -> (dep.package, mempty {mutation = mutation dep})
  where
    mutation dep = maybe NoBounds ExtendedBound (keep !! dep.package)

depBounds :: (VersionRange -> Maybe Version) -> [Dep] -> SolverParams
depBounds get =
  ntFromList . fmap \ dep -> (dep.package, mempty {mutation = range dep})
  where
    range dep | Just bound <- get dep.version = ExtendedBound bound
              | otherwise = NoBounds

lowerBounds :: [Dep] -> SolverParams
lowerBounds = depBounds lowerBound

upperBounds :: [Dep] -> SolverParams
upperBounds = depBounds upperVersion

packageParamsRange :: ManagedOp -> PackageParams -> VersionRange
packageParamsRange op PackageParams {mutation, bounds} =
  intersectVersionRanges (mutationRange mutation) userRange
  where
    mutationRange = \case
      ExtendedBound v -> extended op v
      RetractedBound v -> retracted op v
      NoBounds -> anyVersion

    userRange | Just r <- bounds = r
              | otherwise = anyVersion

    extended = \case
      OpBump -> orLaterVersion
      OpLowerStabilize -> orEarlierVersion
      OpLowerInit -> orEarlierVersion
      OpLowerOptimize -> orEarlierVersion

    retracted = \case
      OpBump -> orEarlierVersion
      OpLowerStabilize -> orLaterVersion
      OpLowerInit -> orLaterVersion
      OpLowerOptimize -> orLaterVersion

toBounds :: ManagedOp -> SolverParams -> Bounds
toBounds op = convert (packageParamsRange op)
