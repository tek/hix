module Hix.Test.Managed.ReleaseMaintenance.Case where

import Distribution.Pretty (Pretty (..))
import Distribution.Version (Version)
import Text.PrettyPrint (text, ($$), (<+>))

import Hix.Data.PackageName (LocalPackage (..), PackageName)
import Hix.Managed.Data.BuildOutput (ModifiedId (..))
import Hix.Managed.Data.Mutable (MutableDep, depName)
import Hix.Managed.Data.Packages (Deps, Packages)
import Hix.Pretty (HPretty (..), field, prettyMap, showP)

data TestDep =
  UnmodifiedDep MutableDep
  |
  ModifiedDep ModifiedId
  |
  FailedDep MutableDep
  deriving stock (Eq, Show)

instance Pretty TestDep where
  pretty = \case
    UnmodifiedDep mid -> pretty mid
    ModifiedDep mid -> pretty mid
    FailedDep mid -> pretty mid

bumpedMutableDep :: TestDep -> MutableDep
bumpedMutableDep = \case
  UnmodifiedDep mid -> mid
  ModifiedDep ModifiedId {package} -> package
  FailedDep mid -> mid

testDepName :: TestDep -> PackageName
testDepName = depName . bumpedMutableDep

isModifiedDep :: TestDep -> Bool
isModifiedDep = \case
  ModifiedDep _ -> True
  UnmodifiedDep _ -> False
  FailedDep _ -> False

partitionTestDeps :: [TestDep] -> ([MutableDep], [ModifiedId], [MutableDep])
partitionTestDeps =
  foldr step mempty
  where
    step dep (u, m, f) = case dep of
      UnmodifiedDep mid -> (mid : u, m, f)
      ModifiedDep mid -> (u, mid : m, f)
      FailedDep mid -> (u, m, mid : f)

data PackageConf =
  PackageConf {
    revision :: Word,
    deps :: Deps TestDep
  }
  deriving stock (Eq, Show)

instance Pretty PackageConf where
  pretty PackageConf {..} =
    text (show revision) $$ pretty deps

data PackageMeta =
  PackageMeta {
    package :: LocalPackage,
    version :: Version,
    revision :: Word,
    deps :: Deps TestDep,
    modified :: Bool,
    bumped :: Bool,
    envModified :: Bool,
    released :: Bool,
    shared :: Bool
  }
  deriving stock (Eq, Show)

instance HPretty PackageMeta where
  hpretty PackageMeta {..} =
    prettyMap (showP package) [
      field "deps" deps,
      field "version" version,
      field "revision" (fromIntegral @_ @Int revision),
      field "modified" modified,
      field "bumped" bumped,
      field "envModified" envModified,
      field "released" released,
      field "shared" shared
    ]

data EnvStyle =
  EnvAll
  |
  EnvEach
  deriving stock (Eq, Show)

instance Pretty EnvStyle where
  pretty = \case
    EnvAll -> text "all"
    EnvEach -> text "each"

data ProjectHistoryEvent =
  Release { package :: LocalPackage, major :: Bool, revision :: Word }
  |
  ReleaseShared { major :: Bool, revision :: Word }
  deriving stock (Eq, Show)

instance Pretty ProjectHistoryEvent where
  pretty = \case
    Release {..} -> text "release" <+> pretty package <+> prettyMajor major <+> prettyRevision revision
    ReleaseShared {..} -> text "release" <+> prettyMajor major <+> prettyRevision revision
    where
      prettyMajor major = if major then "major" else "minor"
      prettyRevision revision = text (show revision)

data MaintTestCase =
  MaintTestCase {
    packages :: Packages PackageConf,
    history :: [ProjectHistoryEvent],
    envStyle :: EnvStyle,
    anyDepModified :: Bool
  }
  deriving stock (Eq, Show)

instance HPretty MaintTestCase where
  hpretty MaintTestCase {..} =
    prettyMap "maint test" [
      field "packages" packages,
      field "history" history,
      field "style" envStyle
    ]

instance Default MaintTestCase where
  def =
    MaintTestCase {
      packages = [],
      history = [],
      envStyle = EnvAll,
      anyDepModified = False
    }
