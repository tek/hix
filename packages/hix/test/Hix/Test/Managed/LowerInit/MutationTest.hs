module Hix.Test.Managed.LowerInit.MutationTest where

import Data.Aeson (eitherDecodeStrict')
import Data.IORef (IORef, readIORef)
import qualified Data.Text as Text
import Distribution.Pretty (pretty)
import Distribution.Version (Version, VersionRange, orLaterVersion, thisVersion)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe, (===))
import Path (relfile)

import Hix.Data.Bounds (TargetBound (TargetLower))
import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.Error (Error (Client, Fatal))
import Hix.Data.LowerConfig (lowerConfigInit)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  EnvConfig (EnvConfig),
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import Hix.Data.NixExpr (Expr)
import qualified Hix.Data.Overrides
import Hix.Data.Overrides (Override (Override))
import Hix.Data.Package (PackageName (PackageName))
import Hix.Data.Version (SourceHash (SourceHash), Versions)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.Build (BuildStatus (Failure, Success), buildStatus)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (
  ManagedConfig (ManagedConfig),
  ManagedOp (OpLowerInit),
  StateFileConfig (StateFileConfig),
  )
import Hix.Managed.Handlers.Build (BuildHandlers (..), versionsBuilder)
import qualified Hix.Managed.Handlers.Build.Test as BuildHandlers
import Hix.Managed.Handlers.Hackage (fetchHash)
import Hix.Managed.Handlers.Lower (LowerHandlers (..), versions)
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import qualified Hix.Managed.Handlers.Report.Prod as ReportHandlers
import qualified Hix.Managed.Handlers.Solve
import Hix.Managed.Handlers.Solve (SolveHandlers (SolveHandlers))
import Hix.Managed.Lower.Data.Lower (Lower)
import Hix.Managed.Lower.Init (lowerInit)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import qualified Hix.Test.Managed.Solver
import Hix.Test.Managed.Solver (TestDeps (TestDeps), testSolver)
import Hix.Test.Utils (UnitTest, runMLogTest, testRoot)

depsConfig :: Either String ConfigDeps
depsConfig =
  eitherDecodeStrict' [exon|{
    "local1": {
      "library": {
        "dependencies": [
          "direct1",
          "direct2 ^>=5.0",
          "direct3 >=1.0 && < 1.5",
          "direct4"
        ]
      }
    },
    "local2": {
      "library": {
        "dependencies": [
          "local1",
          "local3"
        ]
      }
    },
    "local3": {
      "library": {
        "dependencies": [
          "direct1",
          "local1"
        ]
      }
    },
    "local4": {
      "library": {
        "dependencies": [
          "direct4"
        ]
      }
    },
    "local5": {
      "library": {
        "dependencies": [
          "direct5"
        ]
      }
    }
  }|]

fetchVersions :: PackageName -> M [Version]
fetchVersions = \case
  "direct1" -> pure [[1, 0, 3], [1, 0, 4], [1, 0, 5]]
  "direct2" -> pure [[5, 0], [5, 0, 5]]
  "direct3" -> pure [[0, 8], [1, 0, 1], [1, 3], [1, 4], [1, 5]]
  "direct4" -> pure [[1, 0, 1], [1, 0, 2], [1, 0, 3], [1, 0, 4]]
  "transitive1" -> pure [[1, 0, 1]]
  "transitive2" -> pure [[1, 0, 1]]
  "transitive3" -> pure [[1, 0, 1]]
  "transitive4" -> pure [[1, 0, 1]]
  "transitive5" -> pure [[1, 0, 1]]
  "transitive6" -> pure [[1, 0, 1]]
  "transitive7" -> pure [[1, 0, 1]]
  package -> throwM (Client [exon|No such package: ##{package}|])

fetchHashTest :: PackageName -> Version -> M SourceHash
fetchHashTest (PackageName name) v =
  pure (SourceHash [exon|#{name}-#{show (pretty v)}|])

td :: Int -> (PackageName, VersionRange)
td n =
  ([exon|transitive#{show n}|], orLaterVersion [1, 0])

direct2AsDep :: (PackageName, VersionRange)
direct2AsDep =
  ("direct2", thisVersion [5, 0, 5])

byPackage :: Map PackageName [(PackageName, VersionRange)]
byPackage =
  [
    ("direct1", [td 3]),
    ("direct2", [td 5]),
    ("direct4", [direct2AsDep, td 6])
  ]

byVersion :: Map (PackageName, Version) [(PackageName, VersionRange)]
byVersion =
  [
    (("direct1", [1, 0, 5]), [td 2])
  ]

testDeps :: TestDeps
testDeps =
  TestDeps {
    fetchVersions,
    byPackage,
    byVersion
  }

buildWithState :: Versions -> M BuildStatus
buildWithState = \case
  [("direct1", [1, 0, 3]), ("direct3", [1, 0, 1]), ("transitive3", [1, 0, 1])] -> pure Failure
  [("direct1", [1, 0, 4]), ("direct3", [1, 0, 1]), ("transitive3", [1, 0, 1])] -> pure Failure
  [("direct1", [1, 0, 5]), ("direct3", [1, 0, 1]), ("transitive2", [1, 0, 1])] -> pure Success
  [("direct1", [1, 0, 5]), ("direct2", [5, 0]), ("direct3", [1, 0, 1]), ("transitive2", [1, 0, 1]), ("transitive5", [1, 0, 1])] -> pure Success
  [("direct1", [1, 0, 5]), ("direct2", [5, 0, 5]), ("direct3", [1, 0, 1]), ("direct4", [1, 0, n]), ("transitive2", [1, 0, 1]), ("transitive5", [1, 0, 1]), ("transitive6", [1, 0, 1])] -> pure (buildStatus (n == 3))
  versions -> throwM (Fatal [exon|Unexpected overrides: #{showP versions}|])

handlersTest :: IO (LowerHandlers, IORef [Expr], IORef [DepMutation Lower])
handlersTest = do
  (build, stateFileRef) <- BuildHandlers.handlersUnitTest
  (handlers, bumpsRef) <- LowerHandlers.handlersUnitTest
  let handlers' = handlers {
    solve = \ _ -> pure SolveHandlers {solveForVersion = testSolver testDeps},
    build = build {
      withBuilder = versionsBuilder buildWithState,
      hackage = build.hackage {fetchHash = fetchHashTest}
    },
    versions = fetchVersions
  }
  pure (handlers', stateFileRef, bumpsRef)

initialState :: ManagedEnvState
initialState =
  ManagedEnvState {
    bounds = [
      ("local1", [("direct3", [[1, 0, 1], [1, 5]])]),
      ("local5", [("direct5", [[1, 5], [1, 6]])])
    ],
    overrides = [
      ("latest", [("direct2", Override {version = [5, 0], hash = SourceHash "direct2-5.0"})]),
      ("lower-main", [("direct3", Override {version = [1, 0, 1], hash = SourceHash "direct3-1.0.1"})])
    ],
    lowerInit = [("lower-main", [("direct3", [1, 0, 1])])],
    resolving = False
  }

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = ">=1.0.5";
      direct2 = ">=5.0.5 && <5.1";
      direct3 = ">=1.0.1 && <1.5";
      direct4 = ">=1.0.3";
    };
    local2 = {};
    local3 = {
      direct1 = ">=1.0.5";
    };
    local4 = {
      direct4 = ">=1.0.3";
    };
    local5 = {
      direct5 = ">=1.5 && <1.6";
    };
  };
  overrides = {
    latest = {
      direct2 = {
        version = "5.0";
        hash = "direct2-5.0";
      };
    };
    lower-main = {
      direct1 = {
        version = "1.0.5";
        hash = "direct1-1.0.5";
      };
      direct2 = {
        version = "5.0.5";
        hash = "direct2-5.0.5";
      };
      direct3 = {
        version = "1.0.1";
        hash = "direct3-1.0.1";
      };
      direct4 = {
        version = "1.0.3";
        hash = "direct4-1.0.3";
      };
      transitive2 = {
        version = "1.0.1";
        hash = "transitive2-1.0.1";
      };
      transitive5 = {
        version = "1.0.1";
        hash = "transitive5-1.0.1";
      };
      transitive6 = {
        version = "1.0.1";
        hash = "transitive6-1.0.1";
      };
    };
  };
  lowerInit = {
    lower-main = {
      direct1 = "1.0.5";
      direct2 = "5.0.5";
      direct3 = "1.0.1";
      direct4 = "1.0.3";
    };
  };
  resolving = false;
}
|]

logTarget :: [Text]
logTarget =
  Text.lines [exon|[35m[1m>>>[0m Processed environment 'lower-main':
[35m[1m>>>[0m Updated dependency versions:
    📦 direct1 1.0.5 [>=1.0.5]
    📦 direct2 5.0 [>=5.0 && <5.1]
    📦 direct4 1.0.3 [>=1.0.3]
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local1':
    📦 direct1
    📦 direct2
    📦 direct4
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local3':
    📦 direct1
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local4':
    📦 direct4
|]

-- | Goals for these deps:
--
-- - @direct1@ will have three available versions and build successfully for the third one, 1.0.5, which will be added
--   to the overrides. Its bound will be @>=1.0.5@.
--
-- - @direct2@ builds successfully with its lower bound version 5.0, but @direct3@ restricts its version to 5.0.5, which
--   will be in the final state as lower bound and override.
--   Its upper bound from the user config will be retained.
--   It also has a preexisting entry in the overrides of another env, @latest@, which will be retained.
--
-- - @direct3@ has a preexisting entry in `lowerInit`, so it will be ignored completely, since we want to be able to run
--   @lower.init@ after @lower.optimize@ without resetting all lower bounds to the initial states.
--
-- - @direct4@ is a dependency of two targets that have no dependency on each other, but one of them has a stricter
--   version requirement, so that the build fails for @<= direct4-1.0.2@ in @local1@ but only for @direct4-1.0.1@ in
--   @local4@.
--
-- - @direct5@ is a dependency of @local5@, which is not part of the target set.
--   The managed bounds for @local5@ in the initial file should be preserved and unchanged (save for version range
--   normalization), and @direct5@ should never appear int the solver and build deps.
test_lowerInitMutation :: UnitTest
test_lowerInitMutation = do
  deps <- leftA fail depsConfig
  (handlers, stateFileRef, _) <- liftIO handlersTest
  let
    env =
      ManagedEnv {
        deps,
        state = initialState,
        lower = ManagedLowerEnv {solverBounds = mempty},
        envs = [("lower-main", EnvConfig {targets = ["local1", "local2", "local3", "local4"]})],
        buildOutputsPrefix = Nothing
      }
    conf =
      ManagedConfig {
        operation = OpLowerInit,
        ghc = Nothing,
        stateFile = StateFileConfig {
          file = [relfile|ops/managed.nix|],
          updateProject = True,
          projectRoot = Just testRoot
        },
        envs = ["lower-main"],
        targetBound = TargetLower
      }
  (log, result) <- liftIO do
    runMLogTest False $ runManagedApp handlers.build ReportHandlers.handlersProd env conf \ app ->
      Right <$> lowerInit handlers lowerConfigInit def app
  evalEither result
  stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr stateFile)
  logTarget === drop 7 (reverse log)
