module Hix.Test.Managed.LowerAuto.MutationOptimizeTest where

import Exon (exon)
import Test.Tasty (TestTree, testGroup)

import Hix.Class.Map (nGen, (!!))
import Hix.Data.Error (ErrorMessage (Fatal))
import Hix.Data.Version (Versions)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages), InstalledPackages)
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Data.LowerConfig (LowerConfig (reset))
import Hix.Managed.Data.ManagedPackage (ManagedPackage, managedPackages)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Managed.Lower.Auto (lowerAutoMain)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Run (Result (..), TestParams (..), lowerTest, testParams)
import Hix.Test.Utils (UnitTest, unitTest)

packages :: Packages ManagedPackage
packages =
  managedPackages [
    (("local1", "1.0"), [
      "direct1",
      "direct2",
      "direct3 <1.9"
    ])
  ]

initialState :: ProjectStateProto
initialState =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("direct1", [[1, 0], [2, 0]]),
        ("direct2", [[1, 0], [2, 0]])
      ])
    ],
    versions = [
      ("lower", [
        ("direct1", [1, 0]),
        ("direct2", [1, 0])
      ])
    ],
    overrides = [
      ("lower", nGen @[] [1 .. 2] override)
    ],
    initial = [
      ("lower", [
        ("direct1", [1, 5]),
        ("direct2", [1, 5])
      ])
    ],
    resolving = False
  }
  where
    override (num :: Natural) =
      (
        fromString [exon|direct#{show num}|],
        fromString [exon|direct#{show num}-1.0|]
      )

installed :: InstalledPackages
installed =
  [
    ("direct1-1.2", []),
    ("direct2-1.2", []),
    ("direct3-1.2", [])
  ]

available :: SourcePackages
available =
  [
    ("direct1", versions),
    ("direct2", versions),
    ("direct3", versions)
  ]
  where
    versions =
      [
        ([0, 9], []),
        ([1, 0], []),
        ([1, 2], []),
        ([1, 5], []),
        ([1, 9], [])
      ]

ghcPackages :: GhcPackages
ghcPackages = GhcPackages {installed, available}

buildVersionsBasic :: Versions -> M BuildStatus
buildVersionsBasic = \case
  versions
    | Just v <- versions !! "direct2"
    , v <= [1, 0]
    -> pure Failure
    -- direct3 initialization works only at 1.2
    | Just v <- versions !! "direct3"
    , v >= [1, 5]
    -> pure Failure
  -- initialize direct3
  [
    ("direct1", [1, 5]),
    ("direct2", [1, 5]),
    ("direct3", [1, 2])
    ] -> pure Success
  -- optimize direct1
  [
    ("direct1", _),
    ("direct2", [1, 5]),
    ("direct3", [1, 2])
    ] -> pure Success
  -- optimize direct2
  [
    ("direct1", [0, 9]),
    ("direct2", [1, 2]),
    ("direct3", [1, 2])
    ] -> pure Success
  -- optimize direct3
  [
    ("direct1", [0, 9]),
    ("direct2", [1, 2]),
    ("direct3", [1, 0])
    ] -> pure Success
  -- optimize direct3
  [
    ("direct1", [0, 9]),
    ("direct2", [1, 2]),
    ("direct3", [0, 9])
    ] -> pure Success
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

stateFileTargetBasic :: Text
stateFileTargetBasic =
  [exon|{
  bounds = {
    local1 = {
      direct1 = {
        lower = "0.9";
        upper = "2.0";
      };
      direct2 = {
        lower = "1.2";
        upper = "2.0";
      };
      direct3 = {
        lower = "0.9";
        upper = null;
      };
    };
  };
  versions = {
    lower = {
      direct1 = "0.9";
      direct2 = "1.2";
      direct3 = "0.9";
    };
  };
  initial = {
    lower = {
      direct1 = "1.5";
      direct2 = "1.5";
      direct3 = "1.2";
    };
  };
  overrides = {
    lower = {
      direct1 = {
        version = "0.9";
        hash = "direct1-0.9";
      };
      direct3 = {
        version = "0.9";
        hash = "direct3-0.9";
      };
    };
  };
  resolving = false;
}
|]

-- | Goals for these deps:
--
-- A new dependency, @direct3@, has been added to the project, so Init resets all versions to initial and finds an
-- initial bound for the new dep.
-- Afterwards, Optimize successfully determines the best bounds.
--
-- - @direct1@ succeeds for all versions, so its optimized bound will be 0.9.
--   Its initial bound 1.5 will be preserved.
--
-- - @direct2@ succeeds for all versions above 1.0, so its optimized bound will be 1.2.
--   Its initial bound 1.5 will be preserved.
--
-- - @direct3@ succeeds for all versions below 1.5, so its initial bound will be 1.2 and its optimized bound will be
--   0.9.
test_lowerAutoMutationOptimizeBasic :: UnitTest
test_lowerAutoMutationOptimizeBasic = do
  Result {stateFile} <- lowerTest params (lowerAutoMain def)
  eqLines stateFileTargetBasic (renderRootExpr stateFile)
  where
    params =
      (testParams False packages) {
        ghcPackages,
        state = initialState,
        build = buildVersionsBasic
      }

buildVersionsReset :: Versions -> M BuildStatus
buildVersionsReset = \case
  versions
    | Just v <- versions !! "direct2"
    , v <= [1, 0]
    -> pure Failure
    -- direct3 initialization works only at 1.2
    | Just v <- versions !! "direct3"
    , v >= [1, 5]
    -> pure Failure
  -- initialize all versions (latest versions build for direct1 and direct2, so only direct3 deviates)
  [
    ("direct1", [1, 9]),
    ("direct2", [1, 9]),
    ("direct3", [1, 2])
    ] -> pure Success
  -- optimize direct1
  [
    ("direct1", _),
    ("direct2", [1, 9]),
    ("direct3", [1, 2])
    ] -> pure Success
  -- optimize direct2, first major
  [
    ("direct1", [0, 9]),
    ("direct2", [1, 5]),
    ("direct3", [1, 2])
    ] -> pure Success
  -- optimize direct2, second major
  [
    ("direct1", [0, 9]),
    ("direct2", [1, 2]),
    ("direct3", [1, 2])
    ] -> pure Success
  -- optimize direct3, first major
  [
    ("direct1", [0, 9]),
    ("direct2", [1, 2]),
    ("direct3", [1, 0])
    ] -> pure Success
  -- optimize direct3, second major
  [
    ("direct1", [0, 9]),
    ("direct2", [1, 2]),
    ("direct3", [0, 9])
    ] -> pure Success
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

stateFileTargetReset :: Text
stateFileTargetReset =
  [exon|{
  bounds = {
    local1 = {
      direct1 = {
        lower = "0.9";
        upper = "2.0";
      };
      direct2 = {
        lower = "1.2";
        upper = "2.0";
      };
      direct3 = {
        lower = "0.9";
        upper = null;
      };
    };
  };
  versions = {
    lower = {
      direct1 = "0.9";
      direct2 = "1.2";
      direct3 = "0.9";
    };
  };
  initial = {
    lower = {
      direct1 = "1.9";
      direct2 = "1.9";
      direct3 = "1.2";
    };
  };
  overrides = {
    lower = {
      direct1 = {
        version = "0.9";
        hash = "direct1-0.9";
      };
      direct3 = {
        version = "0.9";
        hash = "direct3-0.9";
      };
    };
  };
  resolving = false;
}
|]

-- | Goals for these deps:
--
-- A new dependency, @direct3@, has been added to the project and @--reset@ was specified, so all deps are completely
-- recomputed from scratch.
-- Afterwards, Optimize successfully determines the best bounds.
--
-- - @direct1@ succeeds for all versions, so its optimized bound will be 0.9.
--   Its initial bound will be 1.9.
--
-- - @direct2@ succeeds for all versions above 1.0, so its optimized bound will be 1.2.
--   Its initial bound will be 1.9.
--
-- - @direct3@ succeeds for all versions below 1.5, so its initial bound will be 1.2 and its optimized bound will be
--   0.9.
test_lowerAutoMutationOptimizeReset :: UnitTest
test_lowerAutoMutationOptimizeReset = do
  Result {stateFile} <- lowerTest params (lowerAutoMain def {reset = True})
  eqLines stateFileTargetReset (renderRootExpr stateFile)
  where
    params =
      (testParams False packages) {
        ghcPackages,
        state = initialState,
        build = buildVersionsReset
      }

test_lowerAutoMutationOptimize :: TestTree
test_lowerAutoMutationOptimize =
  testGroup "lower.auto optimize" [
    unitTest "basic" test_lowerAutoMutationOptimizeBasic,
    unitTest "reset initial" test_lowerAutoMutationOptimizeReset
  ]
