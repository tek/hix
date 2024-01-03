module Hix.Managed.ProjectStateProto where

import Data.Map.Merge.Strict (mapMissing, traverseMaybeMissing, traverseMissing, zipWithAMatched, zipWithMatched)
import Exon (exon)

import Hix.Class.Map (NMap, nBy, nElems, nFromKeys, nMap, nMergeA, nTransform)
import Hix.Data.Bounds (Bounds)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (ProjectOptions)
import Hix.Data.PackageName (LocalPackage, PackageName)
import Hix.Data.Version (Version, VersionRange, Versions)
import Hix.Data.VersionBounds (anyBounds, withUpper)
import qualified Hix.Log as Log
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvDeps (EnvDeps))
import Hix.Managed.Data.Envs (Envs)
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage (ManagedPackage))
import Hix.Managed.Data.Mutable (MutableBounds, MutableDep, MutableRanges, depName)
import Hix.Managed.Data.Packages (Deps, Packages)
import qualified Hix.Managed.Data.ProjectState
import Hix.Managed.Data.ProjectState (ProjectState (ProjectState))
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import Hix.Version (upperVersion)

invalidDep ::
  ∀ a b .
  LocalPackage ->
  PackageName ->
  a ->
  M (Maybe b)
invalidDep package dep _ =
  Nothing <$ Log.warn [exon|Discarding bound for invalid dep '##{dep}' of package '##{package}'|]

packageDepsForMerge :: MutableRanges -> Deps (MutableDep, VersionRange)
packageDepsForMerge =
  nTransform \ name range -> (depName name, (name, range))

envDepsForMerge :: Envs EnvDeps -> Envs (Deps MutableDep)
envDepsForMerge =
  nMap \ EnvDeps {mutable} -> nBy mutable depName

toMutable ::
  NMap map MutableDep a sort =>
  Deps (MutableDep, a) ->
  map
toMutable = nTransform \ _ -> id

validateBounds ::
  Bool ->
  LocalPackage ->
  ManagedPackage ->
  Bounds ->
  M MutableBounds
validateBounds readUpper package ManagedPackage {mutable} bounds =
  toMutable <$> nMergeA boundMissing depMissing convertBound deps bounds
  where
    deps = packageDepsForMerge mutable

    boundMissing = mapMissing \ _ (name, range) -> (name, handleUpper range anyBounds)

    depMissing = traverseMaybeMissing (invalidDep package)

    convertBound = zipWithMatched \ _ (name, range) bound -> (name, handleUpper range bound)

    handleUpper range | readUpper, Just u <- upperVersion range = withUpper u
                      | otherwise = id

invalidBoundsPackage :: LocalPackage -> a -> M (Maybe b)
invalidBoundsPackage package _ =
  Nothing <$ Log.warn [exon|Discarding bounds for unknown local package '##{package}'|]

validateProjectBounds ::
  Bool ->
  Packages ManagedPackage ->
  Packages Bounds ->
  M (Packages MutableBounds)
validateProjectBounds readUpper =
  nMergeA boundsMissing (traverseMaybeMissing invalidBoundsPackage) (zipWithAMatched (validateBounds readUpper))
  where
    boundsMissing = traverseMissing \ name package -> validateBounds readUpper name package mempty

invalidInitDep :: Text -> EnvName -> PackageName -> a -> M (Maybe b)
invalidInitDep desc env package _ =
  Nothing <$ Log.warn [exon|Discarding #{desc} for unknown dep '##{package}' of env '##{env}'|]

validateVersions ::
  NMap map MutableDep (Maybe Version) sort =>
  Text ->
  EnvName ->
  Deps MutableDep ->
  Versions ->
  M map
validateVersions desc env deps bounds =
  toMutable <$> nMergeA boundMissing envMissing matching deps bounds
  where
    boundMissing = mapMissing \ _ dep -> (dep, Nothing)
    envMissing = traverseMaybeMissing (invalidInitDep desc env)
    matching = zipWithMatched \ _ dep version -> (dep, Just version)

-- TODO ugly
emptyVersions ::
  NMap map MutableDep (Maybe Version) sort =>
  Deps MutableDep ->
  map
emptyVersions deps =
  nFromKeys (nElems deps) (const Nothing)

invalidVersions :: Text -> EnvName -> a -> M (Maybe b)
invalidVersions desc env _ =
  Nothing <$ Log.warn [exon|Discarding #{desc} for unknown env '##{env}'|]

validateProjectVersions ::
  NMap map MutableDep (Maybe Version) sort =>
  Text ->
  Envs (Deps MutableDep) ->
  Envs Versions ->
  M (Envs map)
validateProjectVersions desc =
  nMergeA initMissing envMissing matching
  where
    initMissing = mapMissing (const emptyVersions)
    envMissing = traverseMaybeMissing (invalidVersions desc)
    matching = zipWithAMatched (validateVersions desc)

validateProjectState ::
  ProjectOptions ->
  Packages ManagedPackage ->
  Envs EnvDeps ->
  ProjectStateProto ->
  M ProjectState
validateProjectState opts packages envDeps proto = do
  bounds <- validateProjectBounds opts.readUpperBounds packages proto.bounds
  versions <- validateProjectVersions "bound versions" (envDepsForMerge envDeps) proto.versions
  initial <- validateProjectVersions "initial versions" (envDepsForMerge envDeps) proto.initial
  pure ProjectState {overrides = proto.overrides, resolving = False, ..}
