module Hix.Managed.Data.Mutable (
  MutableDep (MutableDep),
  MutableDeps,
  unsafeMutableDep,
  depName,
  isMutableDep,
  validate,
  MutableVersions,
  MutableBounds,
  MutableRanges,
  LocalRanges,
  mutRestrictKeys,
  mutLookup,
  mutReplace,
  mutReplaceTargets,
  mutUpdate,
  mutUpdateTargets,
  classifyPackageDep,
  mutRelax,
  mutUpdatePartial,
  mutReplacePartial,
  addBuildVersions,
  mutFromSet,
  mutRelaxVersions,
  mutNonTargets,
  forTargets,
) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Set as Set
import Distribution.Pretty (Pretty (pretty))
import Exon (ToSegment (toSegment))
import GHC.Exts (IsList)

import Hix.Class.EncodeNix (EncodeNix (encodeNix), EncodeNixKey)
import Hix.Class.Map (
  LookupMaybe,
  NLookup,
  NMap,
  nAmend,
  nFromKeys,
  nMap,
  nMapKeys,
  nPartitionByKey,
  nPretty,
  nPrettyWith,
  nRestrictKeys,
  nTransformMaybe,
  (!!),
  )
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep (Dep))
import Hix.Data.PackageName (LocalPackage (LocalPackage), PackageName)
import Hix.Data.Version (Version, VersionRange, Versions)
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Managed.Data.Packages (Deps, Packages)
import Hix.Managed.Data.Targets (Targets)
import qualified Hix.Managed.Targets as Targets
import Hix.Managed.Targets (onlyTargets, overTargets)
import Hix.Maybe (justIf)

newtype MutableDep =
  UnsafeMutableDep PackageName
  deriving stock (Eq, Show)
  deriving newtype (Ord, ToJSON, ToJSONKey, Pretty, EncodeNixKey, FromJSON, FromJSONKey)

pattern MutableDep :: PackageName -> MutableDep
pattern MutableDep name <- UnsafeMutableDep name

{-# complete MutableDep #-}

unsafeMutableDep :: PackageName -> MutableDep
unsafeMutableDep = UnsafeMutableDep

depName :: MutableDep -> PackageName
depName (UnsafeMutableDep name) = name

isMutableDep :: Set MutableDep -> PackageName -> Bool
isMutableDep mut p = Set.member (UnsafeMutableDep p) mut

validate :: Set MutableDep -> PackageName -> Maybe MutableDep
validate pool candidate =
  justIf (isMutableDep pool candidate) (UnsafeMutableDep candidate)

instance IsString a => ToSegment MutableDep a where
  toSegment (UnsafeMutableDep name) = toSegment name

newtype MutableDeps a =
  MutableDeps (Map MutableDep a)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, EncodeNix)

instance NMap (MutableDeps a) MutableDep a LookupMaybe where

instance Pretty a => Pretty (MutableDeps a) where
  pretty = nPretty

newtype MutableVersions =
  MutableVersions (Map MutableDep (Maybe Version))
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap MutableVersions MutableDep (Maybe Version) LookupMaybe where

instance Pretty MutableVersions where
  pretty = nPrettyWith (maybe "[missing]" pretty)

instance EncodeNix MutableVersions where
  encodeNix = encodeNix . mutRelaxVersions

newtype MutableBounds =
  MutableBounds (Map MutableDep VersionBounds)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList, EncodeNix, ToJSON)

instance NMap MutableBounds MutableDep VersionBounds LookupMaybe where

instance Pretty MutableBounds where
  pretty = nPretty

newtype MutableRanges =
  MutableRanges (Map MutableDep VersionRange)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap MutableRanges MutableDep VersionRange LookupMaybe where

instance Pretty MutableRanges where
  pretty = nPretty

newtype LocalRanges =
  LocalRanges (Map LocalPackage VersionRange)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap LocalRanges LocalPackage VersionRange LookupMaybe where

instance Pretty LocalRanges where
  pretty = nPretty

mutLookup ::
  NMap map MutableDep v sort =>
  NLookup sort MutableDep v l =>
  PackageName ->
  map ->
  l
mutLookup k m =
  m !! UnsafeMutableDep k

mutRestrictKeys ::
  NMap map MutableDep v sort =>
  Set PackageName ->
  map ->
  map
mutRestrictKeys keys =
  nRestrictKeys (Set.fromList (UnsafeMutableDep <$> Set.toList keys))

mutReplace ::
  NMap map MutableDep v sort =>
  map ->
  map ->
  map
mutReplace =
  nAmend \ new _ -> new

mutReplaceTargets ::
  NMap map MutableDep v sort =>
  Targets ->
  map ->
  Packages map ->
  Packages map
mutReplaceTargets targets new =
  overTargets targets (mutReplace new)

mutUpdate ::
  NMap map1 MutableDep v1 s1 =>
  NMap map2 MutableDep v2 s2 =>
  (v1 -> v2 -> v2) ->
  map1 ->
  map2 ->
  map2
mutUpdate =
  nAmend

mutUpdateTargets ::
  NMap map1 MutableDep v1 s1 =>
  NMap map2 MutableDep v2 s2 =>
  Targets ->
  (v1 -> v2 -> v2) ->
  map1 ->
  Packages map2 ->
  Packages map2
mutUpdateTargets targets f new =
  overTargets targets (mutUpdate f new)

mutRelax ::
  NMap map1 MutableDep v s1 =>
  NMap map2 PackageName v s2 =>
  map1 ->
  map2
mutRelax =
  nMapKeys depName

mutRelaxVersions ::
  MutableVersions ->
  Versions
mutRelaxVersions =
  nTransformMaybe \ dep -> fmap (depName dep,)

-- | Update a MutableDep map with a PackageName map, only where the original is defined.
mutUpdatePartial ::
  ∀ map1 map2 v1 v2 s1 s2 .
  NMap map1 PackageName v1 s1 =>
  NMap map2 MutableDep v2 s2 =>
  (v1 -> v2 -> v2) ->
  map1 ->
  map2 ->
  map2
mutUpdatePartial f map1 map2 =
  nAmend f (nMapKeys UnsafeMutableDep map1 :: MutableDeps v1) map2

-- | Replace entries in a MutableDep map with those from a PackageName map, only where the original is defined.
mutReplacePartial ::
  ∀ map1 map2 v1 s1 s2 .
  NMap map1 PackageName v1 s1 =>
  NMap map2 MutableDep v1 s2 =>
  map1 ->
  map2 ->
  map2
mutReplacePartial =
  mutUpdatePartial const

addBuildVersions :: Versions -> MutableVersions -> MutableVersions
addBuildVersions versions =
  mutReplacePartial (nMap Just versions :: Deps (Maybe Version))

classifyPackageDep :: Set LocalPackage -> Dep -> Either (LocalPackage, VersionRange) (MutableDep, VersionRange)
classifyPackageDep locals Dep {package, version}
  | Set.member localName locals
  = Left (localName, version)
  | otherwise
  = Right (UnsafeMutableDep package, version)
  where
    localName = LocalPackage package

mutFromSet ::
  NMap map MutableDep v sort =>
  Set MutableDep ->
  (MutableDep -> v) ->
  map
mutFromSet =
  nFromKeys

-- TODO remove
mutNonTargets ::
  Targets ->
  LocalRanges ->
  (MutableRanges, LocalRanges)
mutNonTargets targets =
  nPartitionByKey \case
    package | Targets.member package targets -> Right package
    LocalPackage package -> Left (UnsafeMutableDep package)

forTargets ::
  NMap map1 PackageName a sort1 =>
  NMap map2 MutableDep a sort2 =>
  Targets ->
  Packages map1 ->
  Packages map2
forTargets targets =
  onlyTargets targets . nMap transformOne
  where
    transformOne =
      nTransformMaybe \ package a ->
        justIf (not (Targets.member (LocalPackage package) targets)) (UnsafeMutableDep package, a)
