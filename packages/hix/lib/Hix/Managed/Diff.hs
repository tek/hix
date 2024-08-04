module Hix.Managed.Diff where

import Hix.Class.Map (NMap, nAmend, nMap)
import Hix.Data.Version (Version)
import qualified Hix.Data.VersionBounds
import Hix.Data.VersionBounds (VersionBounds, anyBounds, updateWithCorrection)
import Hix.Managed.Data.Diff (BoundsChange, BoundsDiffDetail (..), Change (..), Diff (..), VersionChange)
import Hix.Managed.Data.Mutable (MutableBounds, MutableDep, MutableDeps, MutableVersions)
import Hix.These (maybeThese)

diffOriginal :: Diff d a -> Maybe a
diffOriginal = \case
  DiffChanged original _ _ -> Just original
  _ -> Nothing

changeOriginal :: Change d a -> Maybe a
changeOriginal = \case
  Unchanged original -> original
  Changed d -> diffOriginal d

reifyChangeWith :: b -> (a -> b) -> Change d a -> b
reifyChangeWith absent f = \case
  Unchanged Nothing -> absent
  Unchanged (Just a) -> f a
  Changed (DiffAdded a) -> f a
  Changed (DiffChanged _ a _) -> f a

reifyChange :: a -> Change d a -> a
reifyChange absent = reifyChangeWith absent id

reifyChangeMaybe :: Change d a -> Maybe a
reifyChangeMaybe = reifyChangeWith Nothing Just

reifyBoundsChangeMaybe :: BoundsChange -> Maybe VersionBounds
reifyBoundsChangeMaybe = reifyChangeMaybe

reifyBoundsChange :: BoundsChange -> VersionBounds
reifyBoundsChange = reifyChange anyBounds

reifyBoundsChanges :: MutableDeps BoundsChange -> MutableBounds
reifyBoundsChanges = nMap reifyBoundsChange

reifyVersionChange :: VersionChange -> Maybe Version
reifyVersionChange = reifyChangeMaybe

reifyVersionChanges :: MutableDeps VersionChange -> MutableVersions
reifyVersionChanges = nMap reifyVersionChange

applyVersionChange :: VersionChange -> Maybe Version -> Maybe Version
applyVersionChange d _ = reifyVersionChange d

applyBoundsChange :: BoundsChange -> VersionBounds -> VersionBounds
applyBoundsChange d old =
  fromMaybe old (reifyBoundsChangeMaybe d)

diff ::
  (a -> a -> Maybe d) ->
  a ->
  a ->
  Change d a
diff mkDetail original new =
  case mkDetail original new of
    Just detail -> Changed (DiffChanged original new detail)
    Nothing -> Unchanged (Just original)

diffMaybe ::
  (a -> a -> Maybe d) ->
  Maybe a ->
  Maybe a ->
  Change d a
diffMaybe mkDetail original new =
  case (original, new) of
    (Just o, Just n) -> diff mkDetail o n
    (Just o, Nothing) -> Unchanged (Just o)
    (Nothing, Just n) -> Changed (DiffAdded n)
    (Nothing, Nothing) -> Unchanged Nothing

versionDiffDetail :: Version -> Version -> Maybe ()
versionDiffDetail original new
  | original == new
  = Nothing
  | otherwise
  = Just ()

versionChange :: Maybe Version -> Maybe Version -> VersionChange
versionChange = diffMaybe versionDiffDetail

versionDiff :: Maybe Version -> Maybe Version -> Maybe (Diff () Version)
versionDiff original new =
  case versionChange original new of
    Changed d -> Just d
    Unchanged _ -> Nothing

boundsDiffDetail :: VersionBounds -> VersionBounds -> Maybe BoundsDiffDetail
boundsDiffDetail original new =
  BoundsDiffDetail <$> maybeThese diffL diffU
  where
    diffL = versionDiff original.lower combined.lower
    diffU = versionDiff original.upper combined.upper
    combined = updateWithCorrection new original

boundsChange :: VersionBounds -> VersionBounds -> BoundsChange
boundsChange = diff boundsDiffDetail

updateVersionChange :: Version -> VersionChange -> VersionChange
updateVersionChange new d =
  diffMaybe versionDiffDetail (changeOriginal d) (Just new)

-- | We never want to remove any bounds, but mutations only return the bound they have targeted, so we need to fall back
-- to the original bounds when updating.
updateBoundsChange :: VersionBounds -> BoundsChange -> BoundsChange
updateBoundsChange new d =
  diffMaybe boundsDiffDetail original (Just combined)
  where
    (original, combined) = case changeOriginal d of
      Just o -> (Just o, updateWithCorrection new o)
      Nothing -> (Nothing, new)

updateVersionChanges ::
  NMap vmap MutableDep (Maybe Version) s1 =>
  NMap map MutableDep VersionChange s2 =>
  vmap ->
  map ->
  map
updateVersionChanges =
  nAmend (maybe id updateVersionChange)

updateBoundsChanges ::
  NMap vmap MutableDep VersionBounds s1 =>
  NMap map MutableDep BoundsChange s2 =>
  vmap ->
  map ->
  map
updateBoundsChanges =
  nAmend updateBoundsChange

initChanges ::
  NMap map1 k (Maybe v1) s1 =>
  NMap map2 k (Change d v1) s2 =>
  map1 ->
  map2
initChanges =
  nMap Unchanged
