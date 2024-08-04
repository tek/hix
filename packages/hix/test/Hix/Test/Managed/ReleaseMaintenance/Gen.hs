module Hix.Test.Managed.ReleaseMaintenance.Gen where

import Data.List (cycle)
import Distribution.Version (Version)
import Exon (exon)
import Hedgehog (Gen, GenT)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hix.Class.Map (nElems, nForKeys, nKeysSet, nRestrictKeys, nConcat)
import Hix.Data.PackageName (LocalPackage (..), PackageName (..))
import Hix.Data.Version (VersionRange)
import Hix.Data.VersionBounds (VersionBounds, majorRange)
import Hix.Managed.Data.BuildOutput (ModifiedId (..))
import Hix.Managed.Data.Mutable (unsafeMutableDep)
import Hix.Managed.Data.Packages (Deps)
import Hix.Test.Managed.ReleaseMaintenance.Case (
  EnvStyle (EnvAll, EnvEach),
  MaintTestCase (..),
  PackageConf (..),
  ProjectHistoryEvent (..),
  TestDep (..),
  isModifiedDep,
  )
import Hix.Test.Managed.UnsafeIsString ()

initialDepVersion :: Version
initialDepVersion = [1, 0]

oldDepBounds :: VersionBounds
oldDepBounds = [[1, 0], [1, 1]]

oldDepRange :: VersionRange
oldDepRange = majorRange oldDepBounds

bumpedDepBounds :: VersionBounds
bumpedDepBounds = [[1, 0], [1, 2]]

bumpedDepRange :: VersionRange
bumpedDepRange = majorRange bumpedDepBounds

bumpedMinorVersion :: Version
bumpedMinorVersion = [1, 0, 1]

bumpedMajorVersion :: Version
bumpedMajorVersion = [1, 1]

genBumpedDep ::
  Monad m =>
  PackageName ->
  GenT m TestDep
genBumpedDep name =
  Gen.element @[] [
    UnmodifiedDep package,
    ModifiedDep ModifiedId {package, version = bumpedMinorVersion, range = Nothing},
    ModifiedDep ModifiedId {package, version = bumpedMajorVersion, range = Just bumpedDepBounds},
    FailedDep package
  ]
  where
    package = unsafeMutableDep name

-- TODO Properties we want to test:
-- * If a package has changes:
--   * The revision number is one greater than when the run started
--   * The release branch exists, has the proper branch style, and corresponds to the latest version (name and base tag)
-- * If a package has no changes:
--   * The revision number is equal to when the run started
--   * If a release branch for the latest version exists, the revision number must be greater than 0
genEvents ::
  NonEmpty LocalPackage ->
  Bool ->
  Gen [ProjectHistoryEvent]
genEvents packages shared
  | shared
  = Gen.list (Range.constant 1 5) (ReleaseShared <$> Gen.bool <*> Gen.word (Range.constant 0 2))
  | otherwise
  = Gen.list (Range.constant 1 10) do
    package <- Gen.element packages
    major <- Gen.bool
    revision <- Gen.word (Range.constant 0 2)
    pure Release {..}

genPackage :: Deps TestDep -> Gen PackageConf
genPackage depsAvail = do
  revision <- Gen.word (Range.constant 0 2)
  depNames <- Gen.subset (nKeysSet depsAvail)
  pure PackageConf {deps = nRestrictKeys depNames depsAvail, ..}

numbered :: IsString a => String -> Int -> [a]
numbered prefix to =
  [fromString [exon|#{prefix}#{show i}|] | i <- [1 .. to]]

genMaintTestCase :: Gen MaintTestCase
genMaintTestCase = do
  depCount <- Gen.int (Range.constant 0 10)
  packageCount <- Gen.int (Range.constant 1 3)
  envStyle <- Gen.element @[] [EnvAll, EnvEach]
  startStyle <- Gen.int (Range.constant 0 1)
  alts <- Gen.int (Range.constant 1 4)
  let packageNames = [fromString [exon|local#{show i}|] | i <- [1 .. packageCount]]
      segments = (take alts (drop startStyle (cycle [True, False])))
  depsBumped <- nForKeys (numbered "dep" depCount) genBumpedDep
  packages <- nForKeys (toList packageNames) (const (genPackage depsBumped))
  history <- concat <$> traverse (genEvents packageNames) segments
  let usedDeps = nConcat packages (const (.deps))
      anyDepModified = any isModifiedDep (nElems usedDeps)
  pure MaintTestCase {..}
