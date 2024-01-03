module Hix.Managed.Data.Targets (
  Targets (Targets),
  unsafeTargets,
  getTargets,
  targetsSet,
  singleTarget,
  sortTargets,
  allMTargets,
  overTargets,
) where

import Data.Aeson (FromJSON, FromJSONKey)
import Data.Foldable.Extra (allM)
import Data.Graph (Graph, Vertex, graphFromEdges, reverseTopSort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Pretty (Pretty)

import Hix.Class.EncodeNix (EncodeNixKey)
import Hix.Class.Map (nGet, nMap, nMapWithKey, nRestrictKeys)
import Hix.Data.PackageName (LocalPackage (LocalPackage))
import Hix.Managed.Data.Packages (Packages)

newtype EnvMember =
  EnvMember LocalPackage
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey, Pretty, EncodeNixKey)

newtype Targets =
  UnsafeTargets [LocalPackage]
  deriving stock (Eq, Show)

pattern Targets :: [LocalPackage] -> Targets
pattern Targets pkgs <- UnsafeTargets pkgs

{-# complete Targets #-}

unsafeTargets :: [LocalPackage] -> Targets
unsafeTargets = UnsafeTargets

singleTarget :: LocalPackage -> Targets
singleTarget = UnsafeTargets . pure

instance IsString Targets where
  fromString = singleTarget . fromString

getTargets :: Targets -> [LocalPackage]
getTargets (Targets pkgs) = pkgs

targetsSet :: Targets -> Set LocalPackage
targetsSet (Targets targets) =
  Set.fromList targets

graph ::
  Map LocalPackage [LocalPackage] ->
  (Graph, Vertex -> (LocalPackage, LocalPackage, [LocalPackage]), LocalPackage -> Maybe Vertex)
graph deps =
  graphFromEdges keyAssoc
  where
    keyAssoc = Map.toList deps <&> \ (p, ds) -> (p, p, ds)

onlyFrom :: Set LocalPackage -> [LocalPackage] -> [LocalPackage]
onlyFrom targets deps =
  mapMaybe isTarget deps
  where
    isTarget = \case
      dep | Set.member dep targets -> Just dep
      _ -> Nothing

sortTargets ::
  Packages [LocalPackage] ->
  [LocalPackage] ->
  Targets
sortTargets deps targets =
  UnsafeTargets (reverseTopSort g <&> \ v -> let (n, _, _) = get v in n)
  where
    (g, get, _) = graph (nGet simple)
    simple :: Packages [LocalPackage]
    simple = nMap (onlyFrom targetSet) (nRestrictKeys targetSet deps)
    targetSet = Set.fromList targets

allMTargets ::
  Monad m =>
  (LocalPackage -> m Bool) ->
  Targets ->
  m Bool
allMTargets f (Targets targets) =
  allM f targets

overTargets :: Targets -> (a -> a) -> Packages a -> Packages a
overTargets (targetsSet -> targets) f =
  nMapWithKey checked
  where
    checked package a
      | Set.member package targets
      = f a
      | otherwise
      = a
