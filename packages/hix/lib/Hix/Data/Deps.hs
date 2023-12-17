module Hix.Data.Deps where

import Distribution.Pretty (Pretty (pretty))

import Hix.Class.Map (LookupMaybe, LookupMonoid, NtMap, convertMaybe1, ntPretty, ntPretty1)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep)
import Hix.Data.PackageName (LocalPackage, PackageName)

data ProjectDep =
  LocalDep Dep
  |
  RemoteDep Dep
  deriving stock (Eq, Show, Generic)

instance Pretty ProjectDep where
  pretty = \case
    LocalDep dep -> pretty dep <> "[local]"
    RemoteDep dep -> pretty dep <> "[remote]"

overDep :: (Dep -> Dep) -> ProjectDep -> ProjectDep
overDep f = \case
  LocalDep dep -> LocalDep (f dep)
  RemoteDep dep -> RemoteDep (f dep)

projectDep :: ProjectDep -> Dep
projectDep = \case
  LocalDep dep -> dep
  RemoteDep dep -> dep

projectDepPackage :: ProjectDep -> PackageName
projectDepPackage dep =
  (projectDep dep).package

projectDepLocal :: ProjectDep -> Bool
projectDepLocal = \case
  LocalDep _ -> True
  RemoteDep _ -> False

newtype Deps =
  Deps (Map PackageName ProjectDep)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance NtMap Deps PackageName ProjectDep LookupMaybe

instance Pretty Deps where
  pretty = ntPretty

-- TODO Maybe create a wrapper or alternative @Target@ that is abstract and use it here.
newtype TargetDeps =
  TargetDeps (Map LocalPackage Deps)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance NtMap TargetDeps LocalPackage Deps LookupMonoid where

instance Pretty TargetDeps where
  pretty = ntPretty1

newtype RemoteDeps =
  RemoteDeps (Map PackageName Dep)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance NtMap RemoteDeps PackageName Dep LookupMaybe

instance Pretty RemoteDeps where
  pretty = ntPretty

newtype TargetRemoteDeps =
  TargetRemoteDeps (Map LocalPackage RemoteDeps)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance NtMap TargetRemoteDeps LocalPackage RemoteDeps LookupMonoid where

instance Pretty TargetRemoteDeps where
  pretty = ntPretty1

targetRemoteDeps :: TargetDeps -> TargetRemoteDeps
targetRemoteDeps =
  convertMaybe1 \case
    LocalDep _ -> Nothing
    RemoteDep dep -> Just dep
