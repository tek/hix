module Hix.Managed.ProjectStateProto where

import Data.Map.Merge.Strict (mapMissing, traverseMaybeMissing, traverseMissing, zipWithAMatched, zipWithMatched)
import Exon (exon)

import Hix.Class.Map (NMap, nBy, nElems, nFromKeys, nMap, nMergeA, nTransform)
import Hix.Data.Bounds (Bounds, Ranges)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.Options (ProjectOptions (..))
import Hix.Data.PackageName (LocalPackage, PackageName)
import Hix.Data.Version (Version, Versions)
import Hix.Data.VersionBounds (amendUpper, anyBounds)
import qualified Hix.Log as Log
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvDeps (EnvDeps))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Mutable (MutableDep, depName)
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

envDepsForMerge :: Envs EnvDeps -> Envs (Deps MutableDep)
envDepsForMerge =
  nMap \ EnvDeps {mutable} -> nBy mutable depName

toMutable ::
  NMap map MutableDep a sort =>
  Deps (MutableDep, a) ->
  map
toMutable = nTransform \ _ -> id

invalidBoundsPackage :: LocalPackage -> a -> M (Maybe b)
invalidBoundsPackage package _ =
  Nothing <$ Log.warn [exon|Discarding bounds for unknown local package '##{package}'|]

validateBounds ::
  Bool ->
  LocalPackage ->
  Ranges ->
  Bounds ->
  M Bounds
validateBounds readUpper package ranges stateBounds =
  nMergeA stateMissing depMissing convertBound ranges stateBounds
  where
    stateMissing = mapMissing \ _ range -> handleUpper range anyBounds

    depMissing = traverseMaybeMissing (invalidDep package)

    convertBound = zipWithMatched (const handleUpper)

    handleUpper range | readUpper, Just u <- upperVersion range = amendUpper u
                      | otherwise = id

validateProjectBounds ::
  Bool ->
  Packages Ranges ->
  Packages Bounds ->
  M (Packages Bounds)
validateProjectBounds readUpper =
  nMergeA boundsMissing (traverseMaybeMissing invalidBoundsPackage) (zipWithAMatched (validateBounds readUpper))
  where
    boundsMissing = traverseMissing \ name package -> validateBounds readUpper name package mempty

invalidStateDep :: Text -> EnvName -> PackageName -> a -> M (Maybe b)
invalidStateDep desc env package _ =
  Nothing <$ Log.warn [exon|Discarding #{desc} for unknown dep '##{package}' of env '##{env}'|]

validateVersions ::
  NMap map MutableDep (Maybe Version) sort =>
  Text ->
  EnvName ->
  Deps MutableDep ->
  Versions ->
  M map
validateVersions desc env deps bounds =
  toMutable <$> nMergeA stateMissing envMissing matching deps bounds
  where
    stateMissing = mapMissing \ _ dep -> (dep, Nothing)
    envMissing = traverseMaybeMissing (invalidStateDep desc env)
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
  nMergeA stateMissing envMissing matching
  where
    stateMissing = mapMissing (const emptyVersions)
    envMissing = traverseMaybeMissing (invalidVersions desc)
    matching = zipWithAMatched (validateVersions desc)

validateProjectState ::
  ProjectOptions ->
  Packages Ranges ->
  Envs EnvDeps ->
  ProjectStateProto ->
  M ProjectState
validateProjectState opts ranges envDeps proto = do
  bounds <- validateProjectBounds opts.readUpperBounds ranges proto.bounds
  versions <- validateProjectVersions "bound versions" depSets proto.versions
  initial <- validateProjectVersions "initial versions" depSets proto.initial
  pure ProjectState {overrides = proto.overrides, solver = proto.solver, resolving = False, ..}
  where
    depSets = envDepsForMerge envDeps
