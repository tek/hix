module Hix.Test.Managed.LowerInit.MutationTest where

import Data.IORef (readIORef)
import qualified Data.Text as Text
import Exon (exon)
import Hedgehog (evalEither, evalMaybe, (===))

import Hix.Class.Map ((!!))
import Hix.Data.Error (Error (Fatal))
import Hix.Data.LowerConfig (lowerConfigInit)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  EnvConfig (EnvConfig),
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import qualified Hix.Data.Overrides
import Hix.Data.Overrides (Override (Override))
import Hix.Data.Version (SourceHash (SourceHash), Versions)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Data.BuildState (BuildStatus (Failure, Success))
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedConfig (ManagedConfig), ManagedOp (OpLowerInit))
import Hix.Managed.Data.ManagedPackage (ManagedPackages, managedPackages)
import Hix.Managed.Handlers.Lower (LowerHandlers (..))
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import qualified Hix.Managed.Handlers.Report.Prod as ReportHandlers
import Hix.Managed.Lower.Init (lowerInit)
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages, allDep, allDeps)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Config (stateFileConfig)
import Hix.Test.Utils (UnitTest, runMLogTest)

packages :: ManagedPackages
packages =
  managedPackages [
    (("local1", "1.0"), [
      "direct1",
      "direct2 ^>=5.0",
      "direct3 >=1.0 && < 1.5",
      "direct4"
    ]),
    (("local2", "1.0"), ["local1", "local3"]),
    (("local3", "1.0"), ["local1", "direct1"]),
    (("local4", "1.0"), ["direct4"]),
    (("local5", "1.0"), ["direct5"]),
    (("local6", "1.0"), ["direct3"]),
    (("local7", "1.0"), [
      "local6 <2",
      "direct2"
    ])
  ]

packageDb :: SourcePackages
packageDb =
  [
    ("direct1", [
      ([1, 0, 3], ["transitive1 >=1"]),
      ([1, 0, 4], ["transitive1 >=1"]),
      ([1, 0, 5], ["transitive2 >=1"])
    ]),
    ("direct2", allDep "transitive3 >=1" [
      ([5, 0], []),
      ([5, 0, 5], [])
    ]),
    ("direct3", [
      ([0, 8], []),
      ([1, 0, 1], []),
      ([1, 3], []),
      ([1, 4], []),
      ([1, 5], [])
    ]),
    ("direct4", allDeps ["direct2 ==5.0.5", "transitive4 >=1"] [
      ([1, 0, 1], []),
      ([1, 0, 2], []),
      ([1, 0, 3], []),
      ([1, 0, 4], [])
    ]),
    ("transitive1", [([1, 0, 1], [])]),
    ("transitive2", [([1, 0, 1], [])]),
    ("transitive3", [([1, 0, 1], [])]),
    ("transitive4", [([1, 0, 1], [])])
  ]

buildVersions :: Versions -> M BuildStatus
buildVersions = \case
  versions
    | Just [1, 0, n] <- versions !! "direct1"
    , n /= 5
    -> pure Failure
  versions
    | Just [1, 0, n] <- versions !! "direct4"
    , n /= 3
    -> pure Failure
  [
    ("direct1", [1, 0, 5]),
    ("direct2", [5, 0, 5]),
    ("direct3", [1, 0, 1]),
    ("direct4", [1, 0, 3]),
    ("transitive2", [1, 0, 1]),
    ("transitive3", [1, 0, 1]),
    ("transitive4", [1, 0, 1])
    ] -> pure Success
  [
    ("direct2", [5, 0]),
    ("direct3", [1, 5]),
    ("transitive3", [1, 0, 1])
    ] -> pure Success
  [
    ("direct2", [5, 0]),
    ("transitive3", [1, 0, 1])
    ] -> pure Success
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

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
    local6 = {
      direct3 = ">=1.0.1 && <1.5";
    };
    local7 = {
      direct2 = ">=5.0";
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
      transitive3 = {
        version = "1.0.1";
        hash = "transitive3-1.0.1";
      };
      transitive4 = {
        version = "1.0.1";
        hash = "transitive4-1.0.1";
      };
    };
    lower-special = {
      direct2 = {
        version = "5.0";
        hash = "direct2-5.0";
      };
      direct3 = {
        version = "1.5";
        hash = "direct3-1.5";
      };
      transitive3 = {
        version = "1.0.1";
        hash = "transitive3-1.0.1";
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
    lower-special = {
      direct2 = "5.0";
    };
  };
  resolving = false;
}
|]

logTarget :: [Text]
logTarget =
  Text.lines [exon|[35m[1m>>>[0m Result for 'lower-main': All dependencies were processed successfully.
[35m[1m>>>[0m Updated dependency versions:
    📦 direct1 1.0.5 [>=1.0.5]
    📦 direct2 5.0.5 [>=5.0.5 && <5.1]
    📦 direct4 1.0.3 [>=1.0.3]
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local1':
    📦 direct1
    📦 direct2
    📦 direct4
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local3':
    📦 direct1
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local4':
    📦 direct4
[35m[1m>>>[0m Result for 'lower-special': All dependencies were processed successfully.
[35m[1m>>>[0m Updated dependency versions:
    📦 direct2 5.0 [>=5.0]
[35m[1m>>>[0m You can remove the lower bounds from these deps of 'local7':
    📦 direct2
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
--
-- - @local6@ is a cross-env dependency of @local7@ and should be omitted from the overrides.
test_lowerInitMutation :: UnitTest
test_lowerInitMutation = do
  (handlers, stateFileRef, _) <- liftIO (LowerHandlers.handlersUnitTest buildVersions packages [] packageDb)
  let
    env =
      ManagedEnv {
        packages,
        state = initialState,
        lower = ManagedLowerEnv {solverBounds = mempty},
        envs = [
          ("lower-main", EnvConfig {targets = ["local1", "local2", "local3", "local4", "local6"], ghc = Nothing}),
          ("lower-special", EnvConfig {targets = ["local7"], ghc = Nothing})
        ],
        buildOutputsPrefix = Nothing
      }
    conf =
      ManagedConfig {
        stateFile = stateFileConfig,
        envs = ["lower-main", "lower-special"]
      }
  (log, result) <- liftIO do
    runMLogTest False True $ runManagedApp handlers.build ReportHandlers.handlersProd env conf OpLowerInit \ app ->
      Right <$> lowerInit handlers lowerConfigInit def app
  evalEither result
  stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr stateFile)
  logTarget === drop 24 (reverse log)
