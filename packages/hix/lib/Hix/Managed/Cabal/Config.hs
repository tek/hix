module Hix.Managed.Cabal.Config where

import qualified Data.Set as Set

import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (PackageName)
import Hix.Managed.Data.Mutable (MutableDep, depName)

nonReinstallableNames :: Set PackageName
nonReinstallableNames =
  [
    "base",
    "ghc-bignum",
    "ghc-prim",
    "ghc",
    "integer-gmp",
    "integer-simple",
    "template-haskell"
  ]

isNonReinstallable :: PackageName -> Bool
isNonReinstallable = flip Set.member nonReinstallableNames

isReinstallable :: PackageName -> Bool
isReinstallable = not . isNonReinstallable

isNonReinstallableId :: PackageId -> Bool
isNonReinstallableId package = isNonReinstallable package.name

isReinstallableId :: PackageId -> Bool
isReinstallableId package = isReinstallable package.name

isNonReinstallableDep :: MutableDep -> Bool
isNonReinstallableDep = isNonReinstallable . depName
