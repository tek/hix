module Hix.Test.Managed.LowerStabilize.MutationTest where

import Exon (exon)

import Hix.Data.Error (ErrorMessage (Fatal))
import Hix.Data.Version (Versions)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Data.ManagedPackage (ProjectPackages, managedPackages)
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Managed.Lower.Stabilize (lowerStabilizeMain)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Run (Result (..), TestParams (..), lowerTest, testParams)
import Hix.Test.Utils (UnitTest)

available :: SourcePackages
available =
  [
    ("direct1", [
      ([1, 8, 1], []),
      ([1, 9, 1], []),
      ([2, 0, 1], [])
    ]),
    ("direct2", [
      ([1, 8, 1], []),
      ([1, 9, 1], []),
      ([2, 0, 1], [])
    ])
  ]

ghcPackages :: GhcPackages
ghcPackages = GhcPackages {installed = [], available}

packages :: ProjectPackages
packages =
  managedPackages [(("local1", "1.0"), ["direct1", "direct2"])]

state :: ProjectStateProto
state =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("direct1", [[1, 8, 1], [2, 1]]),
        ("direct2", [[1, 8, 1], [2, 1]])
      ])
    ],
    versions = [
      ("lower", [
        ("direct1", [1, 8, 1]),
        ("direct2", [1, 8, 1])
      ])
    ],
    initial = [
      ("lower", [
        ("direct1", [2, 0, 1]),
        ("direct2", [2, 0, 1])
      ])
    ],
    overrides = [
      ("lower", [
        ("direct1", "direct1-1.8.1"),
        ("direct2", "direct2-1.8.1")
      ])
    ],
    solver = [],
    resolving = False
  }

build :: Versions -> M BuildStatus
build = \case
  [("direct1", [2, 0, 1]), ("direct2", [2, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 8, 1])] -> pure Failure
  [("direct1", [1, 8, 1]), ("direct2", [2, 0, 1])] -> pure Failure
  [("direct1", [1, 9, 1]), ("direct2", [2, 0, 1])] -> pure Success
  [("direct1", [1, 9, 1]), ("direct2", [1, 8, 1])] -> pure Failure
  [("direct1", [1, 9, 1]), ("direct2", [1, 9, 1])] -> pure Success
  [] -> throwM (Fatal "Build with no overrides")
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = {
        lower = "1.9.1";
        upper = "2.1";
      };
      direct2 = {
        lower = "1.9.1";
        upper = "2.1";
      };
    };
  };
  versions = {
    lower = {
      direct1 = "1.9.1";
      direct2 = "1.9.1";
    };
  };
  initial = {
    lower = {
      direct1 = "2.0.1";
      direct2 = "2.0.1";
    };
  };
  overrides = {
    lower = {
      direct1 = {
        version = "1.9.1";
        hash = "direct1-1.9.1";
      };
      direct2 = {
        version = "1.9.1";
        hash = "direct2-1.9.1";
      };
    };
  };
  solver = {
    lower = {};
  };
  resolving = false;
}
|]

test_lowerStabilizeMutation :: UnitTest
test_lowerStabilizeMutation = do
  Result {stateFile} <- lowerTest params lowerStabilizeMain
  eqLines stateFileTarget (renderRootExpr stateFile)
  where
    params =
      (testParams False packages) {
        ghcPackages,
        state,
        build
      }
