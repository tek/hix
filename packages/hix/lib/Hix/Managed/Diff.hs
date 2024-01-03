module Hix.Managed.Diff where

import Hix.Class.Map (NMap, nAmend, nMap)
import Hix.Data.Version (Version)
import qualified Hix.Data.VersionBounds
import Hix.Data.VersionBounds (VersionBounds (VersionBounds), anyBounds)
import Hix.Managed.Data.Diff (BoundsChange, BoundsDiffDetail (..), Change (..), Diff (..), VersionChange, VersionDiff)
import Hix.Managed.Data.Mutable (MutableBounds, MutableDep, MutableDeps, MutableVersions)
import Hix.These (maybeThese)

diff ::
  Eq a =>
  (a -> a -> d) ->
  Maybe a ->
  Maybe a ->
  Change d a
diff mkDetail original new =
  case (original, new) of
    (Just o, Just n)
      | o == n
      -> Unchanged (Just o)
      | otherwise
      -> Changed (DiffChanged o n (mkDetail o n))
    (Just o, Nothing) -> Unchanged (Just o)
    (Nothing, Just n) -> Changed (DiffAdded n)
    (Nothing, Nothing) -> Unchanged Nothing

versionDiffDetail :: Version -> Version -> Maybe ()
versionDiffDetail _ _ = Just ()

diffVersions :: Maybe Version -> Maybe Version -> VersionChange
diffVersions original new =
  diff (\ _ _ -> ()) original new

versionDiff :: Maybe Version -> Maybe Version -> Maybe (Diff () Version)
versionDiff original new =
  case diffVersions original new of
    Changed d -> Just d
    Unchanged _ -> Nothing

diffBounds :: VersionBounds -> VersionBounds -> BoundsChange
diffBounds original new
  | Just detail <- BoundsDiffDetail <$> maybeThese diffL diffU
  = Changed (DiffChanged {..})
  | otherwise
  = Unchanged (Just original)
  where
    diffL = versionDiff original.lower new.lower
    diffU = versionDiff original.upper new.upper

updateDiff ::
  Eq a =>
  (a -> Maybe (a, d)) ->
  a ->
  Diff d a ->
  Maybe (Diff d a)
updateDiff mkDiff newFresh = \case
  DiffAdded _ -> Just (DiffAdded newFresh)
  DiffChanged original _ _ -> changed original
  where
    changed original =
      case mkDiff original of
        Just (new, detail) | original == new -> Nothing
                           | otherwise -> Just (DiffChanged {original, new, detail})
        Nothing -> Nothing

updateChange ::
  Eq a =>
  (a -> Maybe (a, d)) ->
  a ->
  Change d a ->
  Change d a
updateChange mkDiff newFresh = \case
  Unchanged Nothing -> Changed (DiffAdded newFresh)
  Unchanged (Just original)
    | original == newFresh
    -> Unchanged (Just original)
    | otherwise
    -> changed original
  Changed pre -> case updateDiff mkDiff newFresh pre of
    Just new -> Changed new
    Nothing -> Unchanged (Just newFresh)
  where
    changed original =
      case mkDiff original of
        Just (new, detail) | original == new -> Unchanged (Just original)
                           | otherwise -> Changed (DiffChanged {original, new, detail})
        Nothing -> Unchanged (Just original)

replaceChange ::
  Eq a =>
  a ->
  (a -> a -> Maybe d) ->
  Change d a ->
  Change d a
replaceChange new mkDetail =
  updateChange mkDetail' new
  where
    mkDetail' original = (new,) <$> mkDetail original new

updateVersionChange :: Version -> VersionChange -> VersionChange
updateVersionChange new d =
  replaceChange new versionDiffDetail d

-- TODO change to reify + diffBounds
-- Need to track whether the original value was @current@ so that the new value will be as well
updateBoundsChange :: VersionBounds -> BoundsChange -> BoundsChange
updateBoundsChange new d =
  updateChange mkDetail new d
  where
    mkDetail original@VersionBounds {lower = lo, upper = uo} =
      maybeThese diffL diffU <&> \ detail -> (updated, BoundsDiffDetail detail)
      where
        diffL = versionDiff lo ln
        diffU = versionDiff uo un
        updated@VersionBounds {lower = ln, upper = un} = new <> original

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

reifyChangeWith :: b -> (a -> b) -> Change d a -> b
reifyChangeWith absent f = \case
  Unchanged Nothing -> absent
  Unchanged (Just a) -> f a
  Changed (DiffAdded a) -> f a
  Changed (DiffChanged _ a _) -> f a

reifyChange :: a -> Change d a -> a
reifyChange absent =
  reifyChangeWith absent id

reifyChangeMaybe :: Change d a -> Maybe a
reifyChangeMaybe =
  reifyChangeWith Nothing Just

reifyBoundsChangeMaybe :: BoundsChange -> Maybe VersionBounds
reifyBoundsChangeMaybe d =
  reifyChangeMaybe d

reifyBoundsChange :: BoundsChange -> VersionBounds
reifyBoundsChange d =
  reifyChange anyBounds d

reifyBoundsChanges :: MutableDeps BoundsChange -> MutableBounds
reifyBoundsChanges = nMap reifyBoundsChange

reifyVersionChange :: VersionChange -> Maybe Version
reifyVersionChange d =
  reifyChangeMaybe d

reifyVersionChanges :: MutableDeps VersionChange -> MutableVersions
reifyVersionChanges = nMap reifyVersionChange

applyVersionChange :: VersionChange -> Maybe Version -> Maybe Version
applyVersionChange d _ =
  reifyVersionChange d

applyBoundsChange :: BoundsChange -> VersionBounds -> VersionBounds
applyBoundsChange d old =
  fromMaybe old (reifyBoundsChangeMaybe d)

diffChangedOriginal :: Diff d a -> Maybe a
diffChangedOriginal = \case
  DiffChanged original _ _ -> Just original
  _ -> Nothing

versionDiffChangedOriginal :: VersionDiff -> Maybe Version
versionDiffChangedOriginal d = diffChangedOriginal d

initChanges ::
  NMap map1 k (Maybe v1) s1 =>
  NMap map2 k (Change d v1) s2 =>
  map1 ->
  map2
initChanges =
  nMap Unchanged
