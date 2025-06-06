module Hix.Test.Managed.LowerAuto.MutationStabilizeTest where

import Exon (exon)

import Hix.Class.Map (nGen, (!!))
import Hix.Data.Error (ErrorMessage (Fatal))
import Hix.Data.Overrides (Overrides)
import Hix.Data.Version (Versions)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages), InstalledPackages)
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Data.LowerConfig (LowerConfig (stabilize))
import Hix.Managed.Data.ManagedPackage (ProjectPackages, managedPackages)
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Managed.Lower.Auto (lowerAutoMain)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Run (Result (..), TestParams (..), lowerTest, testParams, withoutRevisions)
import Hix.Test.Utils (UnitTest)

packages :: ProjectPackages
packages =
  managedPackages [
    (("local1", "1.0"), [
      "direct1",
      "direct2",
      "direct3"
    ])
  ]

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

state :: ProjectStateProto
state =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("direct1", [[1, 0], [2, 0]]),
        ("direct2", [[1, 0], [2, 0]]),
        ("direct3", [[1, 0], [2, 0]])
      ])
    ],
    versions = [
      ("lower", [
        ("direct1", [1, 0]),
        ("direct2", [1, 0]),
        ("direct3", [1, 0])
      ])
    ],
    initial = [
      ("lower", [
        ("direct1", [1, 5]),
        ("direct2", [1, 5]),
        ("direct3", [1, 5])
      ])
    ],
    overrides = [
      ("lower", nGen @[] [1 .. 3] override)
    ],
    solver = [],
    resolving = False
  }
  where
    override (num :: Natural) =
      (
        fromString [exon|direct#{show num}|],
        fromString [exon|direct#{show num}-1.0|]
      )

build :: Versions -> M (BuildStatus, Overrides)
build = withoutRevisions \case
  versions
    | Just v <- versions !! "direct2"
    , v <= [1, 0]
    -> pure Failure
    | Just v <- versions !! "direct3"
    , v <= [1, 2]
    -> pure Failure
  -- initial bounds
  [
    ("direct1", [1, 5]),
    ("direct2", [1, 5]),
    ("direct3", [1, 5])
    ] -> pure Success
  -- stabilizing direct1
  [
    ("direct1", [1, 0]),
    ("direct2", [1, 5]),
    ("direct3", [1, 5])
    ] -> pure Success
  -- stabilizing direct2 and direct3
  [
    ("direct1", [1, 0]),
    ("direct2", [1, 2]),
    ("direct3", [1, 5])
    ] -> pure Success
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = {
        lower = "1.0";
        upper = "2.0";
      };
      direct2 = {
        lower = "1.2";
        upper = "2.0";
      };
      direct3 = {
        lower = "1.5";
        upper = "2.0";
      };
    };
  };
  versions = {
    lower = {
      direct1 = "1.0";
      direct2 = "1.2";
      direct3 = "1.5";
    };
  };
  initial = {
    lower = {
      direct1 = "1.5";
      direct2 = "1.5";
      direct3 = "1.5";
    };
  };
  overrides = {
    lower = {
      direct1 = {
        version = "1.0";
        hash = "direct1-1.0";
      };
      direct3 = {
        version = "1.5";
        hash = "direct3-1.5";
      };
    };
  };
  solver = {
    lower = {};
  };
  resolving = false;
}
|]

-- | Goals for these deps:
--
-- The project was changed and the lower bounds have been invalidated.
-- LowerInit does not process mutations since all deps have initial bounds.
-- LowerStabilize is executed because the config flag is enabled.
--
-- - @direct1@ is compatible in all versions.
--
-- - @direct2@ has become incompatible for all versions up to 1.0.
--   The mutation handler tries 1.0 and 1.2, the latter of which succeeds.
--
-- - @direct2@ has become incompatible for all versions up to 1.2.
--   The mutation handler tries 1.0, 1.2 and 1.5, the third of which succeeds.
test_lowerAutoMutationStabilize :: UnitTest
test_lowerAutoMutationStabilize = do
  Result {stateFile} <- lowerTest params (lowerAutoMain def {stabilize = True})
  eqLines stateFileTarget (renderRootExpr stateFile)
  where
    params =
      (testParams False packages) {
        ghcPackages,
        state,
        build
      }
