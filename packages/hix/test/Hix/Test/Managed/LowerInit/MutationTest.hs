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
import Hix.Data.Overrides (EnvOverrides)
import Hix.Data.Package (LocalPackage, PackageName (PackageName))
import Hix.Data.Version (NewVersion (..), SourceHash (SourceHash))
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build.Mutation (DepMutation (DepMutation))
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
import Hix.Managed.Lower (lowerInit)
import qualified Hix.Managed.Lower.Data.LowerInit
import Hix.Managed.Lower.Data.LowerInit (LowerInit (LowerInit))
import Hix.Monad (M, runMLog, throwM)
import Hix.NixExpr (Expr, renderRootExpr)
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
          "direct1 ^>= 2.0",
          {
            "name": "direct2",
            "version": "< 1.5"
          },
          "direct3",
          "direct4:{internal,external}",
          "direct5 ^>=5.0",
          "direct6 >=1.0 && < 1.5",
          "direct7"
        ]
      }
    },
    "local2": {
      "library": {
        "dependencies": [
          "direct1 ^>= 2.0",
          "local1",
          "local3"
        ]
      }
    },
    "local3": {
      "library": {
        "dependencies": [
          "direct3",
          "local1"
        ]
      }
    },
    "local4": {
      "library": {
        "dependencies": [
          "direct7"
        ]
      }
    },
    "local5": {
      "library": {
        "dependencies": [
          "direct8"
        ]
      }
    }
  }|]

fetchVersions :: PackageName -> M [Version]
fetchVersions = \case
  "direct1" -> pure [[1, 9], [2, 2, 0, 5]]
  "direct2" -> pure [[1, 3, 1], [1, 7, 14]]
  "direct3" -> pure [[1, 0, 3], [1, 0, 4], [1, 0, 5]]
  "direct4" -> pure [[1, 9], [2, 0, 1]]
  "direct5" -> pure [[5, 0], [5, 0, 5]]
  "direct6" -> pure [[0, 8], [1, 0, 1], [1, 3], [1, 4], [1, 5]]
  "direct7" -> pure [[1, 0, 1], [1, 0, 2], [1, 0, 3], [1, 0, 4]]
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

direct5AsDep :: (PackageName, VersionRange)
direct5AsDep =
  ("direct5", thisVersion [5, 0, 5])

byPackage :: Map PackageName [(PackageName, VersionRange)]
byPackage =
  [
    ("direct1", [td 1]),
    ("direct2", []),
    ("direct3", [td 3]),
    ("direct4", [td 4]),
    ("direct5", [td 5]),
    ("direct6", [direct5AsDep]),
    ("direct7", [td 6])
  ]

byVersion :: Map (PackageName, Version) [(PackageName, VersionRange)]
byVersion =
  [
    (("direct3", [1, 0, 5]), [td 2])
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
    "direct2" -> pure True
    "direct3" -> case newVersion.version of
      [1, 0, 5] -> pure True
      _ -> pure False
    "direct4" -> case newVersion.version of
      [2, 0, 1] -> pure False
      _ -> pure True
    "direct5" -> pure True
    "direct6" -> pure True
    "direct7" | target == "local1" -> case newVersion.version of
      [1, 0, 3] -> pure True
      _ -> pure False
    "direct7" | target == "local4" -> case newVersion.version of
      [1, 0, 1] -> pure False
      _ -> pure True
    "direct7" -> pure True
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
      "direct4": ">= 2.0"
    },
    "local5": {
      "direct8": "^>= 1.5"
    }
  }|]

managedOverridesFile :: Either String EnvOverrides
managedOverridesFile =
  eitherDecodeStrict' [exon|{
    "latest": {
      "direct5": {
        "version": "5.0",
        "hash": "direct5-5.0"
      }
    }
  }|]

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = ">=2.0 && <2.1";
      direct2 = ">=1.3.1 && <1.5";
      direct3 = ">=1.0.5";
      direct4 = ">=2.0";
      direct5 = ">=5.0.5 && <5.1";
      direct6 = ">=1.0.1 && <1.5";
      direct7 = ">=1.0.3";
    };
    local2 = {
      direct1 = ">=2.0 && <2.1";
    };
    local3 = {
      direct3 = ">=1.0.5";
    };
    local4 = {
      direct7 = ">=1.0.3";
    };
    local5 = {
      direct8 = ">=1.5 && <1.6";
    };
  };
  overrides = {
    latest = {
      direct5 = {
        version = "5.0";
        hash = "direct5-5.0";
      };
    };
    lower-main = {
      direct2 = {
        version = "1.3.1";
        hash = "direct2-1.3.1";
      };
      direct3 = {
        version = "1.0.5";
        hash = "direct3-1.0.5";
      };
      direct5 = {
        version = "5.0.5";
        hash = "direct5-5.0.5";
      };
      direct6 = {
        version = "1.0.1";
        hash = "direct6-1.0.1";
      };
      direct7 = {
        version = "1.0.3";
        hash = "direct7-1.0.3";
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
}
|]

logTarget :: [Text]
logTarget =
  Text.lines [exon|[33m[1m>>>[0m No suitable version found for 'direct1'.
[35m[1m>>>[0m Updated dependency versions:
    📦 direct2 1.3.1 [>=1.3.1 && <1.5]
    📦 direct3 1.0.5 [>=1.0.5]
    📦 direct5 5.0 [>=5.0 && <5.1]
    📦 direct6 1.0.1 [>=1.0.1 && <1.5]
    📦 direct7 1.0.3 [>=1.0.3]
[35m[1m>>>[0m Failed to find working versions for some dependencies:
    📦 direct4 2.0.1 [>=2.0]
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local1':
    📦 direct3
    📦 direct5
    📦 direct6
    📦 direct7
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local3':
    📦 direct3
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local4':
    📦 direct7
|]

failedMutationsTarget :: [DepMutation LowerInit]
failedMutationsTarget =
  [
    DepMutation "direct4" LowerInit {versions = pure [2, 0, 1], range = orLaterVersion [2, 0]}
  ]

-- | Goals for these deps:
--
-- - @direct1@ will be skipped because there are no versions that match the existing bounds.
--
-- - @direct2@ has only an upper bound, 1.5, which means that the second major before that will be considered.
--   As it doesn't have any versions in 1.4, and only one version in 1.3, we use 1.3 anyway.
--   Its bound will be adapted to ">=1.3.1 && <1.5".
--
-- - @direct3@ will have three available versions and build successfully for the third one, 1.0.5, which will be added
--   to the overrides. Its bound will be @>=1.0.5@.
--
-- - @direct4@ has a preexisting lower bound in the managed state, but it will fail to build.
--   The original bound will be retained.
--
-- - @direct5@ builds successfully with its lower bound version 5.0, but @direct6@ restricts its version to 5.0.5, which
--   will be in the final state as lower bound and override.
--   It also has a preexisting entry in the overrides of another env, @latest@, which will be retained.
--
-- - @direct6@ has a preexisting lower bound, so the first version greater than that, 1.0.1, will be added to the
--   overrides.
--   Its bounds will be adapted to @>=1.0.1 && <1.5@, retaining the upper bound.
--
-- - @direct7@ is a dependency of two targets that have no dependency on each other, but one of them has a stricter
--   version requirement, so that the build fails for @<= direct7-1.0.2@ in @local1@ but only for @direct7-1.0.1@ in
--   @local4@.
--
-- - @direct8@ is a dependency of @local5@, which is not part of the target set.
--   The managed bounds for @local5@ in the initial file should be preserved and unchanged (save for version range
--   normalization), and @direct8@ should never appear int the solver and build deps.
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
        state = ManagedEnvState {bounds = managedBounds, overrides = managedOverrides},
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
        targetBound = TargetLower
      }
    lowerConf = LowerInitConfig {stabilize = True, lowerMajor = False, oldest = False, initialBounds = []}
  (log, result) <- liftIO do
    runMLog False False False root $ runManagedApp handlers.build ReportHandlers.handlersProd env conf \ app ->
      Right <$> lowerInit handlers lowerConf app
  evalEither result
  stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr stateFile)
  logTarget === reverse log
