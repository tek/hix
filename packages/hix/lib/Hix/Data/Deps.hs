module Hix.Data.Deps where

import Distribution.Pretty (Pretty (pretty))

import Hix.Class.Map (LookupMaybe, LookupMonoid, NtMap, ntPretty, ntPretty1)
import Hix.Data.Dep (Dep)
import Hix.Data.Package (LocalPackage, PackageName)

newtype Deps =
  Deps (Map PackageName Dep)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance NtMap Deps PackageName Dep LookupMaybe

instance Pretty Deps where
  pretty = ntPretty

newtype TargetDeps =
  TargetDeps (Map LocalPackage Deps)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance NtMap TargetDeps LocalPackage Deps LookupMonoid where

instance Pretty TargetDeps where
  pretty = ntPretty1
