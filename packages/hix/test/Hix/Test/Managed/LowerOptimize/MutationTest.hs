module Hix.Test.Managed.LowerOptimize.MutationTest where

import Data.Aeson (eitherDecodeStrict')
import Data.IORef (IORef, readIORef)
import Distribution.Pretty (pretty)
import Distribution.Version (Version, VersionRange, orLaterVersion)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe)
import Path (Abs, Dir, Path, absdir, relfile)

import Hix.Data.Bounds (TargetBound (TargetLower), TargetBounds)
import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.Error (Error (Client, Fatal))
import Hix.Data.LowerConfig (lowerConfigOptimize)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  EnvConfig (EnvConfig),
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import Hix.Data.NixExpr (Expr)
import Hix.Data.Overrides (EnvOverrides)
import Hix.Data.Package (PackageName (PackageName))
import Hix.Data.Version (SourceHash (SourceHash), Versions)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.BuildState (BuildStatus (Failure, Success))
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (
  ManagedConfig (ManagedConfig),
  ManagedOp (OpLowerOptimize),
  StateFileConfig (StateFileConfig),
  )
import Hix.Managed.Handlers.Build (BuildHandlers (..), versionsBuilder)
import qualified Hix.Managed.Handlers.Build.Test as BuildHandlers
import Hix.Managed.Handlers.Hackage (fetchHash)
import Hix.Managed.Handlers.Lower (LowerHandlers (..), versions)
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import qualified Hix.Managed.Handlers.Solve
import Hix.Managed.Handlers.Solve (SolveHandlers (SolveHandlers))
import Hix.Managed.Lower.Optimize (lowerOptimize)
import Hix.Managed.Lower.Data.Lower (Lower)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import qualified Hix.Test.Managed.Solver
import Hix.Test.Managed.Solver (TestDeps (TestDeps), testSolver)
import Hix.Test.Utils (UnitTest, runMTest, testRoot)

tmpRoot :: Path Abs Dir
tmpRoot = [absdir|/tmp/project|]

fetchVersions :: PackageName -> M [Version]
fetchVersions = \case
  "direct1" -> pure basic
  "direct2" -> pure [[1, 7, 1], [1, 8, 1], [1, 9, 1], [1, 9, 2], [2, 0, 1]]
  "transitive1" -> pure [[1, 0, 1]]
  package -> throwM (Client [exon|No such package: ##{package}|])
  where
    basic = [[1, 8, 1], [1, 9, 1], [2, 0, 1]]

fetchHash :: PackageName -> Version -> M SourceHash
fetchHash (PackageName name) v =
  pure (SourceHash [exon|#{name}-#{show (pretty v)}|])

td :: Int -> (PackageName, VersionRange)
td n =
  ([exon|transitive#{show n}|], orLaterVersion [1, 0])

byPackage :: Map PackageName [(PackageName, VersionRange)]
byPackage =
  [
    ("direct1", [td 1])
  ]

testDeps :: TestDeps
testDeps = TestDeps {fetchVersions, byPackage, byVersion = []}

-- TODO make missing initial lower bounds fatal
buildWithState :: Versions -> M BuildStatus
buildWithState = \case
  [("direct1", [1, 9, 1]), ("direct2", [1, 9, 2]), ("transitive1", [1, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 9, 2]), ("transitive1", [1, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 9, 1]), ("transitive1", [1, 0, 1])] -> pure Failure
  [("direct1", [1, 8, 1]), ("direct2", [1, 8, 1]), ("transitive1", [1, 0, 1])] -> pure Failure
  versions -> throwM (Fatal [exon|Unexpected overrides: #{showP versions}|])

handlersTest :: IO (LowerHandlers, IORef [Expr], IORef [DepMutation Lower])
handlersTest = do
  (build, stateFileRef) <- BuildHandlers.handlersUnitTest
  (handlers, bumpsRef) <- LowerHandlers.handlersUnitTest
  let handlers' = handlers {
    solve = \ _ -> pure SolveHandlers {solveForVersion = testSolver testDeps},
    build = build {
      withBuilder = versionsBuilder buildWithState,
      hackage = build.hackage {fetchHash}
    },
    versions = fetchVersions
  }
  pure (handlers', stateFileRef, bumpsRef)

depsConfig :: Either String ConfigDeps
depsConfig =
  eitherDecodeStrict' [exon|{
    "local1": {
      "library": {
        "dependencies": [
          "direct1",
          "direct2"
        ]
      }
    }
  }|]

managedBoundsFile :: Either String TargetBounds
managedBoundsFile =
  eitherDecodeStrict' [exon|{
    "local1": {
      "direct1": ">=2.0 && <2.1",
      "direct2": ">=2.0 && <2.1"
    }
  }|]

managedOverridesFile :: Either String EnvOverrides
managedOverridesFile =
  eitherDecodeStrict' [exon|{
    "lower": {
      "direct1": {
        "version": "2.0.1",
        "hash": "direct1-2.0.1"
      },
      "direct2": {
        "version": "2.0.1",
        "hash": "direct2-2.0.1"
      }
    }
  }|]

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = ">=1.8.1 && <2.1";
      direct2 = ">=1.9.2 && <2.1";
    };
  };
  overrides = {
    lower = {
      direct1 = {
        version = "1.8.1";
        hash = "direct1-1.8.1";
      };
      direct2 = {
        version = "1.9.2";
        hash = "direct2-1.9.2";
      };
      transitive1 = {
        version = "1.0.1";
        hash = "transitive1-1.0.1";
      };
    };
  };
  lowerInit = {};
  resolving = false;
}
|]

-- | Goals for these deps:
--
-- - @direct1@ has two lower majors and all of them succeed, resulting in the lowest major (1.8.1) to become the bound.
--
-- - @direct2@ has three lower majors, the second of which fails, resulting in the highest major 1.9 to be the last one
--   tested.
--   The first version 1.9.1 also fails, resulting in 1.9.2 to become the bound.
test_lowerOptimizeMutation :: UnitTest
test_lowerOptimizeMutation = do
  deps <- leftA fail depsConfig
  managedBounds <- leftA fail managedBoundsFile
  managedOverrides <- leftA fail managedOverridesFile
  (handlers, stateFileRef, _) <- liftIO handlersTest
  let
    env =
      ManagedEnv {
        deps,
        state = ManagedEnvState {bounds = managedBounds, overrides = managedOverrides, lowerInit = [], resolving = False},
        lower = ManagedLowerEnv {solverBounds = mempty},
        envs = [("lower", EnvConfig {targets = ["local1"]})],
        buildOutputsPrefix = Nothing
      }
    conf =
      ManagedConfig {
        operation = OpLowerOptimize,
        ghc = Nothing,
        stateFile = StateFileConfig {
          file = [relfile|ops/managed.nix|],
          updateProject = True,
          projectRoot = Just testRoot
        },
        envs = ["lower"],
        targetBound = TargetLower
      }
  evalEither =<< liftIO do
    runMTest False $ runManagedApp handlers.build handlers.report env conf \ app ->
      Right <$> lowerOptimize handlers lowerConfigOptimize app
  stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr stateFile)
