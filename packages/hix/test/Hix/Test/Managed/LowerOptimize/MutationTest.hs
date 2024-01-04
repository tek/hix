module Hix.Test.Managed.LowerOptimize.MutationTest where

import Exon (exon)

import Hix.Data.Error (Error (Fatal))
import Hix.Data.Version (Versions)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Cabal.Mock.SourcePackage (allDep)
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto, managedPackages)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Managed.Lower.Optimize (lowerOptimizeMain)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Lower (LowerTestParams (..), Result (..), lowerParams, lowerTest)
import Hix.Test.Utils (UnitTest)

packages :: Packages ManagedPackageProto
packages =
  managedPackages [(("local1", "1.0"), ["direct1", "direct2"])]

packageDb :: SourcePackages
packageDb =
  [
    ("direct1", allDep "transitive1 >=1.0" [
      ([1, 8, 1], []),
      ([1, 9, 1], []),
      ([2, 0, 1], [])
    ]),
    ("direct2", [
      ([1, 7, 1], []),
      ([1, 8, 1], []),
      ([1, 9, 1], []),
      ([1, 9, 2], []),
      ([2, 0, 1], [])
    ]),
    ("transitive1", [
      ([1, 0, 1], [])
    ])
  ]

ghcPackages :: GhcPackages
ghcPackages = GhcPackages {installed = [], available = packageDb}

state :: ProjectStateProto
state =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("direct1", [[2, 0, 1], [2, 1]]),
        ("direct2", [[2, 0, 1], [2, 1]])
      ])
    ],
    versions = [
      ("lower", [
        ("direct1", [2, 0, 1]),
        ("direct2", [2, 0, 1])
      ])
    ],
    overrides = [
      ("lower", [
        ("direct1", "direct1-2.0.1"),
        ("direct2", "direct2-2.0.1")
      ])
    ],
    initial = [
      ("lower", [
        ("direct1", [2, 0, 1]),
        ("direct2", [2, 0, 1])
      ])
    ],
    resolving = False
  }

build :: Versions -> M BuildStatus
build = \case
  [("direct1", [1, 9, 1]), ("direct2", [2, 0, 1]), ("transitive1", [1, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [2, 0, 1]), ("transitive1", [1, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 9, 1]), ("transitive1", [1, 0, 1])] -> pure Failure
  [("direct1", [1, 8, 1]), ("direct2", [1, 9, 2]), ("transitive1", [1, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 8, 1]), ("transitive1", [1, 0, 1])] -> pure Failure
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = {
        lower = "1.8.1";
        upper = "2.1";
      };
      direct2 = {
        lower = "1.9.2";
        upper = "2.1";
      };
    };
  };
  versions = {
    lower = {
      direct1 = "1.8.1";
      direct2 = "1.9.2";
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
  Result {stateFile} <- lowerTest params lowerOptimizeMain
  eqLines stateFileTarget (renderRootExpr stateFile)
  where
    params =
      (lowerParams False packages) {
        ghcPackages,
        state,
        build
      }
