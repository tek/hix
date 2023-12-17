module Hix.Managed.Data.Targets (
  Targets (Targets),
  getTargets,
  singleTarget,
  sortTargets,
) where

import Data.Graph (Graph, Vertex, graphFromEdges, reverseTopSort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Hix.Data.Dep
import Hix.Data.Deps (Deps (Deps), ProjectDep (LocalDep), TargetDeps (TargetDeps))
import Hix.Data.PackageName (LocalPackage (LocalPackage), isLocalPackage)

newtype Targets =
  UnsafeTargets [LocalPackage]
  deriving stock (Eq, Show)

pattern Targets :: [LocalPackage] -> Targets
pattern Targets pkgs <- UnsafeTargets pkgs

{-# complete Targets #-}

singleTarget :: LocalPackage -> Targets
singleTarget = UnsafeTargets . pure

instance IsString Targets where
  fromString = singleTarget . fromString

getTargets :: Targets -> [LocalPackage]
getTargets (Targets pkgs) = pkgs

onlyFrom :: Set LocalPackage -> Deps -> [LocalPackage]
onlyFrom targets (Deps deps) =
  LocalPackage <$> mapMaybe isTarget (Map.elems deps)
  where
    isTarget = \case
      LocalDep dep | isLocalPackage targets dep.package -> Just dep.package
      _ -> Nothing

graph ::
  Map LocalPackage [LocalPackage] ->
  (Graph, Vertex -> (LocalPackage, LocalPackage, [LocalPackage]), LocalPackage -> Maybe Vertex)
graph deps =
  graphFromEdges keyAssoc
  where
    keyAssoc = Map.toList deps <&> \ (p, ds) -> (p, p, ds)

sortTargets ::
  TargetDeps ->
  [LocalPackage] ->
  Targets
sortTargets (TargetDeps deps) targets =
  UnsafeTargets (reverseTopSort g <&> \ v -> let (n, _, _) = get v in n)
  where
    (g, get, _) = graph simple
    simple = onlyFrom targetSet <$> Map.restrictKeys deps targetSet
    targetSet = Set.fromList targets
