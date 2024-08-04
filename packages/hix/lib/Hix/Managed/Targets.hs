module Hix.Managed.Targets where

import qualified Data.Set as Set

import Hix.Class.Map (nMapWithKey, nRestrictKeys)
import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.Targets (Targets (..), unsortedTargets)

onlyTargets :: Targets -> Packages a -> Packages a
onlyTargets targets =
  nRestrictKeys (unsortedTargets targets)

overTargets :: Targets -> (a -> a) -> Packages a -> Packages a
overTargets (unsortedTargets -> targets) f =
  nMapWithKey checked
  where
    checked package a
      | Set.member package targets
      = f a
      | otherwise
      = a

member :: LocalPackage -> Targets -> Bool
member package (unsortedTargets -> targets) = package `elem` targets
