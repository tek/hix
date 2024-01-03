module Hix.Test.Managed.LowerInit.MutationTest where

import Data.IORef (readIORef)
import Data.List.Extra (zipWithLongest)
import qualified Data.Text as Text
import Exon (exon)
import Hedgehog (evalEither, evalMaybe, (===))

import Hix.Class.Map ((!!))
import Hix.Data.Error (Error (Fatal))
import Hix.Data.Options (envs, readUpperBounds)
import qualified Hix.Data.Overrides
import Hix.Data.Overrides (Override (Override))
import Hix.Data.Version (SourceHash (SourceHash), Versions)
import qualified Hix.Managed.Cabal.Changes
import Hix.Managed.Cabal.Changes (SolverPlan (SolverPlan))
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSynthetic))
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Cabal.Mock.SourcePackage (allDep, allDeps)
import Hix.Managed.Data.Constraints (EnvConstraints)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto, managedPackages)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (ProjectContextProto))
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Managed.Handlers.Build (report)
import qualified Hix.Managed.Handlers.Lower as LowerHandlers
import Hix.Managed.Handlers.Lower (LowerHandlers (..))
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import qualified Hix.Managed.Handlers.Report.Prod as ReportHandlers
import Hix.Managed.Lower.Init (lowerInitMain)
import Hix.Managed.ProjectContext (withProjectContext)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Utils (UnitTest, runMLogTest)

packages :: Packages ManagedPackageProto
packages =
  managedPackages [
    (("local1", "1.0"), [
      "direct1 >5.0",
      "direct2 <5.1",
      "direct3",
      "direct4"
    ]),
    (("local2", "1.0"), ["local1", "local3"]),
    (("local3", "1.0"), ["local1", "direct1"]),
    (("local4", "1.0"), ["direct4"]),
    (("local5", "1.0"), ["direct5"]),
    (("local6", "1.0"), ["direct3"]),
    (("local7", "1.0"), ["local6 <2", "direct2"])
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

ghcPackages :: GhcPackages
ghcPackages = GhcPackages {installed = [], available = packageDb}

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
    ("direct3", [1, 4]),
    ("transitive3", [1, 0, 1])
    ] -> pure Success
  [
    ("direct2", [5, 0]),
    ("transitive3", [1, 0, 1])
    ] -> pure Success
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

initialState :: ProjectStateProto
initialState =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("direct2", [[4, 0], [4, 4]]),
        ("direct3", [[1, 0, 1], [1, 5]])
      ]),
      ("local5", [("direct5", [[1, 5], [1, 6]])])
    ],
    versions = [
      ("lower-main", [
        ("direct1", [1, 0, 1]),
        ("direct2", [4, 3, 1]),
        ("direct3", [1, 0, 1])
      ])
    ],
    overrides = [
      ("latest", [("direct2", Override {version = [5, 0], hash = SourceHash "direct2-5.0"})]),
      ("lower-main", [("direct3", Override {version = [1, 0, 1], hash = SourceHash "direct3-1.0.1"})])
    ],
    initial = [("lower-main", [("direct3", [1, 0, 1])])],
    resolving = False
  }

-- | This uses a very bespoke @IsString@ instance for @(PackageName, MutationConstraints)@ that parses a @Dep@ and uses
-- the bounds (inclusive or exclusive) to construct @VersionBounds@.
cabalTarget :: [(EnvConstraints, Maybe SolverPlan)]
cabalTarget =
  [
    item1 3 1,
    item1 4 1,
    item1 5 2,
    ([
      "direct1",
      "direct2 ==5.0",
      "direct3 <=1.0.1",
      "direct4"
    ], Nothing),
    ([
      "direct1",
      "direct2 ==5.0.5",
      "direct3 <=1.0.1",
      "direct4"
    ], plan ["direct1-1.0.5", "direct2-5.0.5", "direct3-1.0.1", "direct4-1.0.4", "transitive2-1.0.1", "transitive3-1.0.1", "transitive4-1.0.1"]),
    item2 1,
    item2 2,
    item2 3,
    -- Iteration 2
    item3 3 1,
    item3 4 1,
    item3 5 2,
    ([
      "direct1 <=1.0.5",
      "direct2 <=5.0.5 && ==5.0",
      "direct3 <=1.0.1",
      "direct4 <=1.0.3"
    ], Nothing),
    ([
      "direct1 <=1.0.5",
      "direct2 <=5.0.5 && ==5.0.5",
      "direct3 <=1.0.1",
      "direct4 <=1.0.3"
    ], plan ["direct1-1.0.5", "direct2-5.0.5", "direct3-1.0.1", "direct4-1.0.3", "transitive2-1.0.1", "transitive3-1.0.1", "transitive4-1.0.1"]),
    -- lower-special
    ([
      "direct2 ==5.0",
      "local6"
    ], plan ["direct2-5.0", "direct3-1.4", "local6-1.0", "transitive3-1.0.1"])
  ]
  where
    item1 (v1 :: Natural) (nt :: Natural) =
      ([
        fromString [exon|direct1 ==1.0.#{show v1}|],
        "direct2",
        "direct3 <=1.0.1",
        "direct4"
      ], plan [fromString [exon|direct1-1.0.#{show v1}|], "direct2-5.0.5", "direct3-1.0.1", "direct4-1.0.4", fromString [exon|transitive#{show nt}-1.0.1|], "transitive3-1.0.1", "transitive4-1.0.1"])

    item2 (v1 :: Natural) =
      ([
        "direct1",
        "direct2",
        "direct3 <=1.0.1",
        fromString [exon|direct4 ==1.0.#{show v1}|]
      ], plan ["direct1-1.0.5", "direct2-5.0.5", "direct3-1.0.1", fromString [exon|direct4-1.0.#{show v1}|], "transitive2-1.0.1", "transitive3-1.0.1", "transitive4-1.0.1"])

    item3 (v1 :: Natural) (nt :: Natural) =
      ([
        fromString [exon|direct1 <=1.0.5 && ==1.0.#{show v1}|],
        "direct2 <=5.0.5",
        "direct3 <=1.0.1",
        "direct4 <=1.0.3"
      ], plan [fromString [exon|direct1-1.0.#{show v1}|], "direct2-5.0.5", "direct3-1.0.1", "direct4-1.0.3", fromString [exon|transitive#{show nt}-1.0.1|], "transitive3-1.0.1", "transitive4-1.0.1"])

    plan changes = Just SolverPlan {changes, matching = [], nonReinstallable = Nothing}

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = {
        lower = "1.0.5";
        upper = null;
      };
      direct2 = {
        lower = "5.0.5";
        upper = "5.1";
      };
      direct3 = {
        lower = "1.0.1";
        upper = "1.5";
      };
      direct4 = {
        lower = "1.0.3";
        upper = null;
      };
    };
    local2 = {};
    local3 = {
      direct1 = {
        lower = "1.0.5";
        upper = null;
      };
    };
    local4 = {
      direct4 = {
        lower = "1.0.3";
        upper = null;
      };
    };
    local5 = {
      direct5 = {
        lower = "1.5";
        upper = "1.6";
      };
    };
    local6 = {
      direct3 = {
        lower = "1.0.1";
        upper = "1.5";
      };
    };
    local7 = {
      direct2 = {
        lower = "5.0";
        upper = null;
      };
    };
  };
  versions = {
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
        version = "1.4";
        hash = "direct3-1.4";
      };
      transitive3 = {
        version = "1.0.1";
        hash = "transitive3-1.0.1";
      };
    };
  };
  initial = {
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
  Text.lines [exon|
[35m[1m>>>[0m [33mlower-main[0m
[35m[1m>>>[0m Found initial lower bounds for all deps after 2 iterations.
[35m[1m>>>[0m Added new versions:
    📦 [34mdirect4[0m   1.0.3   ↕ >=1.0.3
[35m[1m>>>[0m Updated versions:
    📦 [34mdirect1[0m   [31m1.0.1[0m -> [32m1.0.5[0m   ↕ [no bounds] -> >=[32m1.0.5[0m
    📦 [34mdirect2[0m   [31m4.3.1[0m -> [32m5.0.5[0m   ↕ [[31m4.0[0m, 5.1] -> [[32m5.0.5[0m, 5.1]

[35m[1m>>>[0m [33mlower-special[0m
[35m[1m>>>[0m Found initial lower bounds for all deps after 1 iteration.
[35m[1m>>>[0m Added new versions:
    📦 [34mdirect2[0m   5.0   ↕ >=5.0
|]

-- | Goals for these deps:
--
-- - @direct1@ will have three available versions and build successfully for the third one, 1.0.5, which will be added
--   to the overrides. Its bound will be @>=1.0.5@.
--   It also has a lower bound, 5.0, in the flake config, which is ignored.
--
-- - @direct2@ builds successfully with its lowest version 5.0, but @direct3@ restricts its version to 5.0.5, which
--   will be in the final state as lower bound and override.
--   It has preexisting managed bounds that will be printed as a diff.
--   Its upper bound from the user config will be retained, replacing the managed bound.
--   It also has a preexisting entry in the overrides of another env, @latest@, which will be retained.
--
-- - @direct3@ has a preexisting entry in `initial`, so it will be ignored completely as a candidate, since we want to
--   be able to run @lower.init@ after @lower.optimize@ without resetting all lower bounds to the initial states.
--   It has preexisting managed bounds that will be printed as a diff.
--
-- - @direct4@ is a dependency of two targets that have no dependency on each other, but one of them has a stricter
--   version requirement, so that the build fails for @<= direct4-1.0.2@ in @local1@ but only for @direct4-1.0.1@ in
--   @local4@.
--
-- - @direct5@ is a dependency of @local5@, which is not part of the target set.
--   The managed bounds for @local5@ in the initial file should be preserved and unchanged (save for version range
--   normalization), and @direct5@ should never appear in the solver and build deps.
--
-- - @local6@ is a cross-env dependency of @local7@ and should be omitted from the overrides in @lower-special@.
--   It has a dependency on @direct3@, which is not among the direct dependencies of @lower-special@, therefore being
--   excluded from the solver params when mutating @local7@.
--   Even though @direct3@ has a lower bound in @lower-main@, 1.0.1, it is treated as a regular transitive dep, and the
--   solver chooses the latest matching version for it.
--   Since the pre-existing, exclusive, upper bound in the deps of @local6@ is 1.5, the chosen version is the
--   next-lower, 1.4.
--   This version is written to the overrides for @lower-special@, like any non-installed transitive dep.
test_lowerInitMutation :: UnitTest
test_lowerInitMutation = do
  (handlers0, stateFileRef, _) <- LowerHandlers.handlersUnitTest buildVersions ghcPackages
  (cabalRef, handlers) <- LowerHandlers.logCabal handlers0 {build = handlers0.build {report = ReportHandlers.handlersProd}}
  let
    ghc = GhcDbSynthetic ghcPackages

    opts = def {envs = ["lower-main", "lower-special"], readUpperBounds = True}

    proto =
      ProjectContextProto {
        packages,
        state = initialState,
        envs = [
          ("lower-main", EnvConfig {targets = ["local1", "local2", "local3", "local4", "local6"], ghc}),
          ("lower-special", EnvConfig {targets = ["local7"], ghc})
        ],
        buildOutputsPrefix = Nothing
      }

  (log, result) <- liftIO do
    runMLogTest False True do
      withProjectContext handlers.build opts proto \ project ->
        lowerInitMain def handlers project
  evalEither result
  cabalLog <- reverse <$> liftIO (readIORef cabalRef)
  stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  for_ (zip [0 :: Natural ..] (zipWithLongest (,) cabalTarget cabalLog)) \case
    (i, (Just t, Just l)) -> (i, t) === (i, l)
    _ | [] <- cabalLog -> fail "Cabal log is empty."
    _ -> cabalTarget === cabalLog
  cabalTarget === cabalLog
  eqLines stateFileTarget (renderRootExpr stateFile)
  logTarget === drop 24 (reverse log)
