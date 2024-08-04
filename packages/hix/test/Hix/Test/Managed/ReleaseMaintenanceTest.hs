module Hix.Test.Managed.ReleaseMaintenanceTest where

import Hedgehog (Property, TestT, forAllWith, property, test, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Class.Map (nElems, nMap, nTo)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (localPackageName)
import Hix.Managed.Data.BuildOutput (DepChanges (..))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Git (MaintBranch (..), Tag (..))
import Hix.Managed.Maint.Data.MaintResult (
  MaintChanged (..),
  MaintResult (..),
  NoPublishReason (..),
  UnchangedReason (..),
  )
import Hix.Managed.Maint.Git (releaseBranchName)
import Hix.Managed.ReleaseMaintenance (releaseMaintenance)
import Hix.Maybe (justIf)
import Hix.Pretty (showHP, showP)
import Hix.Test.Managed.Maint.Handlers (
  EnvOutput (..),
  GitState (..),
  MaintEvent (..),
  MaintPremise (..),
  MaintState (..),
  buildOutputs,
  dummyRevision,
  evolveHistory,
  maintTestHandlers,
  )
import Hix.Test.Managed.ReleaseMaintenance.Case (MaintTestCase (..), PackageMeta (..))
import Hix.Test.Managed.ReleaseMaintenance.Gen (genMaintTestCase)
import Hix.Test.Utils (runMTest)

config :: MaintConfig
config =
  MaintConfig {
    noFailures = False,
    commit = True,
    push = False,
    revision = True,
    targets = Nothing,
    ci = False,
    pr = False
  }

data MaintTestOutput =
  MaintTestOutput {
    results :: Packages MaintResult,
    events :: [MaintEvent],
    git :: GitState,
    packages :: Packages PackageMeta,
    outputs :: Envs EnvOutput
  }
  deriving stock (Eq, Show)

runMaintTest ::
  MaintTestCase ->
  TestT IO MaintTestOutput
runMaintTest testCase = do
  runMTest False do
    (handlers, context, state) <- maintTestHandlers outputs premise
    results <- releaseMaintenance handlers config context
    MaintState {events, git} <- liftIO $ readMVar state
    pure MaintTestOutput {packages = premise.packages, ..}
  where
    outputs = buildOutputs premise.packages testCase.envStyle
    premise = evolveHistory testCase

resultProps ::
  Packages PackageMeta ->
  Packages MaintResult
resultProps =
  nMap \ meta -> result (mkChanged (mkBranch meta)) meta
  where
    result changed = \case
      PackageMeta {modified, bumped, released, revision, envModified}
        | bumped
        , released
        -> changed (Published (dummyRevision (revision + 1)))

        | modified
        , released
        -> changed (Modified NoRangeUpdates)

        | released
        , envModified
        -> changed (Modified NoDirectDepUpdates)

        | released
        -> Unchanged NoUpdates

        | otherwise
        -> Unchanged NoTags

    mkChanged branch resolution = Changed {baseBranch = branch, ..}

    mkBranch PackageMeta {package, version} =
      releaseBranchName MaintBranch {..}

eventProps ::
  MaintTestOutput ->
  [MaintEvent]
eventProps MaintTestOutput {packages, outputs} =
  mconcat $ reverse $ nTo packages $ const \case
    PackageMeta {package, version, released, bumped}
      | released ->
        fold (justIf bumped (publishEvents PackageId {name = localPackageName package, version})) ++
        maybeToList (gitEvents package version)
      | otherwise ->
        []
  where
    publishEvents packageId =
      [
        EventFetchRevisions,
        EventPublishRevision {
          packageId,
          form = ("pkgid", showP packageId)
        },
        EventFetchRevisions
      ]

    gitEvents package version = do
      output <- find (isTarget package) (nElems outputs)
      modified <- nonEmpty output.changes.modified
      pure GitCommitted {package, branch = MaintBranch {package, version}, modified}

    isTarget target EnvOutput {targets} = flip any targets \ PackageMeta {package} -> package == target

gitProps :: Packages PackageMeta -> GitState
gitProps packages =
  GitState {branches, current}
  where
    branches =
      reverse $ catMaybes $ nTo packages $ const \case
        PackageMeta {package, version, revision, released, shared}
          | released
          , revision == 0
          , let maintBranch = MaintBranch {package, version}
          -> Just (maintBranch, releaseBranchName maintBranch, Tag {package = justIf (not shared) package, version})
          | otherwise
          -> Nothing

    current =
      last $ catMaybes $ nTo packages $ const \case
        PackageMeta {package, version, released}
          | released
          -> Just MaintBranch {package, version}
          | otherwise
          -> Nothing


prop_maint :: Property
prop_maint =
  property do
    testCase <- forAllWith showHP genMaintTestCase
    output <- test $ runMaintTest testCase
    resultProps output.packages === output.results
    eventProps output === output.events
    gitProps output.packages === output.git

test_maint :: TestTree
test_maint =
  testGroup "managed bounds maintenance" [
    testProperty "maint logic" prop_maint
  ]
