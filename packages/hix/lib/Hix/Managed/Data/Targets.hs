module Hix.Managed.Data.Targets (
  Targets (Targets),
  unsafeTargets,
  singleTarget,
  unsortedTargets,
  sortTargets,
  firstMTargets,
) where

import Data.Graph (Graph, Vertex, graphFromEdges, reverseTopSort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Pretty (Pretty (pretty))

import Hix.Class.Map (nGet, nMap, nRestrictKeys)
import Hix.Data.PackageName (LocalPackage, PackageName, toLocalPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Pretty (prettyL)

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

instance Pretty Targets where
  pretty (Targets ps) = prettyL ps

unsortedTargets :: Targets -> Set LocalPackage
unsortedTargets (Targets targets) = Set.fromList targets

graph ::
  Map LocalPackage [LocalPackage] ->
  (Graph, Vertex -> (LocalPackage, LocalPackage, [LocalPackage]), LocalPackage -> Maybe Vertex)
graph deps =
  graphFromEdges [(p, p, ds) | (p, ds) <- Map.toList deps]

onlyFrom :: Set LocalPackage -> [PackageName] -> [LocalPackage]
onlyFrom targets = mapMaybe (toLocalPackage targets)

sortTargets ::
  Packages [PackageName] ->
  [LocalPackage] ->
  Targets
sortTargets deps targets =
  UnsafeTargets (reverseTopSort g <&> \ v -> let (n, _, _) = get v in n)
  where
    (g, get, _) = graph (nGet simple)

    simple :: Packages [LocalPackage]
    simple = nMap (onlyFrom targetSet) (nRestrictKeys targetSet deps)

    targetSet = Set.fromList targets

-- | Run the computation for each target in topological order, aborting and returning the computation's result if it
-- meets the error condition.
-- If all computations succeed, return the success value.
firstMTargets ::
  Monad m =>
  -- | Success value.
  a ->
  -- | Error condition.
  (a -> Bool) ->
  -- | Computation.
  (LocalPackage -> m a) ->
  -- | Iteration sequence targets.
  Targets ->
  m a
firstMTargets success cond f (Targets targets) =
  foldr chain (pure success) targets
  where
    chain a z = do
      res <- f a
      if cond res
      then pure res
      else z
