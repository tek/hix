module Hix.Test.Managed.LowerInit.MutationTest where

import Data.Aeson (eitherDecodeStrict')
import Data.IORef (IORef, readIORef)
import qualified Data.Text as Text
import Distribution.Pretty (pretty)
import Distribution.Version (Version, VersionRange, orLaterVersion, thisVersion)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe, (===))
import Path (Abs, Dir, Path, absdir, relfile)

import Hix.Data.Bounds (TargetBound (TargetLower), TargetBounds)
import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Client, Fatal))
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerInitConfig (LowerInitConfig))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import Hix.Data.NixExpr (Expr)
import Hix.Data.OutputFormat (OutputFormat (OutputNone))
import Hix.Data.Overrides (EnvOverrides)
import Hix.Data.Package (LocalPackage, PackageName (PackageName))
import Hix.Data.Version (NewVersion (..), SourceHash (SourceHash))
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedConfig (ManagedConfig), StateFileConfig (StateFileConfig))
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import qualified Hix.Managed.Handlers.Build.Test as BuildHandlers
import Hix.Managed.Handlers.Hackage (fetchHash)
import Hix.Managed.Handlers.LowerInit (LowerInitHandlers (..), versions)
import qualified Hix.Managed.Handlers.LowerInit.Test as LowerInitHandlers
import qualified Hix.Managed.Handlers.Report.Prod as ReportHandlers
import qualified Hix.Managed.Handlers.Solve
import Hix.Managed.Handlers.Solve (SolveHandlers (SolveHandlers))
import Hix.Managed.Lower.App (lowerInit)
import Hix.Managed.Lower.Data.LowerInit (LowerInit)
import Hix.Monad (M, runMLog, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Test.Hedgehog (eqLines)
import qualified Hix.Test.Managed.Solver
import Hix.Test.Managed.Solver (TestDeps (TestDeps), testSolver)
import Hix.Test.Utils (UnitTest)

root :: Path Abs Dir
root = [absdir|/project|]

tmpRoot :: Path Abs Dir
tmpRoot = [absdir|/tmp/project|]

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
    ("direct3", [direct2AsDep]),
    ("direct4", [td 6])
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

buildProjectTest :: Path Abs Dir -> EnvName -> LocalPackage -> NewVersion -> M Bool
buildProjectTest _ _ target newVersion =
  case newVersion.package of
    "direct1" -> case newVersion.version of
      [1, 0, 5] -> pure True
      _ -> pure False
    "direct2" -> pure True
    "direct3" -> pure True
    "direct4" | target == "local1" -> case newVersion.version of
      [1, 0, 3] -> pure True
      _ -> pure False
    "direct4" | target == "local4" -> case newVersion.version of
      [1, 0, 1] -> pure False
      _ -> pure True
    "direct4" -> pure True
    pkg -> throwM (Fatal [exon|Unexpected dep for building ##{target}: ##{pkg}|])

handlersTest :: IO (LowerInitHandlers, IORef [Expr], IORef [DepMutation LowerInit])
handlersTest = do
  (build, stateFileRef) <- BuildHandlers.handlersUnitTest tmpRoot
  (handlers, bumpsRef) <- LowerInitHandlers.handlersUnitTest
  let handlers' = handlers {
    build = build {
      buildProject = buildProjectTest,
      solve = SolveHandlers {solveForVersion = testSolver testDeps},
      hackage = build.hackage {
        fetchHash = fetchHashTest
      }
    },
    versions = fetchVersions
  }
  pure (handlers', stateFileRef, bumpsRef)

managedBoundsFile :: Either String TargetBounds
managedBoundsFile =
  eitherDecodeStrict' [exon|{
    "local1": {
    },
    "local5": {
      "direct5": "^>= 1.5"
    }
  }|]

managedOverridesFile :: Either String EnvOverrides
managedOverridesFile =
  eitherDecodeStrict' [exon|{
    "latest": {
      "direct2": {
        "version": "5.0",
        "hash": "direct2-5.0"
      }
    }
  }|]

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
  resolving = false;
}
|]

logTarget :: [Text]
logTarget =
  Text.lines [exon|[35m[1m>>>[0m Updated dependency versions:
    📦 direct1 1.0.5 [>=1.0.5]
    📦 direct2 5.0 [>=5.0 && <5.1]
    📦 direct3 1.0.1 [>=1.0.1 && <1.5]
    📦 direct4 1.0.3 [>=1.0.3]
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local1':
    📦 direct1
    📦 direct2
    📦 direct3
    📦 direct4
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local3':
    📦 direct1
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local4':
    📦 direct4
|]

failedMutationsTarget :: [DepMutation LowerInit]
failedMutationsTarget =
  [
  ]

-- | Goals for these deps:
--
-- - @direct1@ will have three available versions and build successfully for the third one, 1.0.5, which will be added
--   to the overrides. Its bound will be @>=1.0.5@.
--
-- - @direct2@ builds successfully with its lower bound version 5.0, but @direct3@ restricts its version to 5.0.5, which
--   will be in the final state as lower bound and override.
--   It also has a preexisting entry in the overrides of another env, @latest@, which will be retained.
--
-- - @direct3@ has a preexisting lower bound, so the first version greater than that, 1.0.1, will be added to the
--   overrides.
--   Its bounds will be adapted to @>=1.0.1 && <1.5@, retaining the upper bound.
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
  managedBounds <- leftA fail managedBoundsFile
  managedOverrides <- leftA fail managedOverridesFile
  (handlers, stateFileRef, _) <- liftIO handlersTest
  let
    env =
      ManagedEnv {
        deps,
        state = ManagedEnvState {bounds = managedBounds, overrides = managedOverrides, resolving = False},
        lower = ManagedLowerEnv {solverBounds = mempty},
        targets = ["local1", "local2", "local3", "local4"]
      }
    conf =
      ManagedConfig {
        ghc = Nothing,
        stateFile = StateFileConfig {
          file = [relfile|ops/managed.nix|],
          updateProject = True,
          projectRoot = Just root,
          latestOverrides = True
        },
        env = "lower-main",
        targetBound = TargetLower,
        batchLog = Nothing
      }
    lowerConf = LowerInitConfig {stabilize = True, lowerMajor = False, oldest = False, initialBounds = []}
  (log, result) <- liftIO do
    runMLog False False False OutputNone root $ runManagedApp handlers.build ReportHandlers.handlersProd env conf \ app ->
      Right <$> lowerInit handlers lowerConf app
  evalEither result
  stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr stateFile)
  logTarget === reverse log
