module Hix.Test.Managed.Bump.MutationTest where

import qualified Data.Text as Text
import Exon (exon)
import Hedgehog ((===))

import Hix.Data.Error (Error (Fatal))
import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (Versions)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto, managedPackages)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import qualified Hix.Test.Managed.Run
import Hix.Test.Managed.Run (Result (Result), TestParams (..), lowerTest, testParams)
import Hix.Test.Utils (UnitTest)

packages :: Packages ManagedPackageProto
packages =
  managedPackages [(("local1", "1.0"), [
    "base",
    "direct1",
    "direct2",
    "direct3",
    "direct4",
    "direct5"
  ])]

installed :: [(PackageId, [PackageId])]
installed =
  [
    ("base-4.12.0.0", []),
    ("direct1-1.0.1", []),
    ("direct2-1.0.1", []),
    ("direct3-1.0.1", []),
    ("direct4-1.0.1", []),
    ("direct5-1.0.1", [])
  ]

packageDb :: SourcePackages
packageDb =
  [
    ("base", [
      ([4, 11, 0, 0], []),
      ([4, 12, 0, 0], []),
      ([4, 13, 0, 0], [])
    ]),
    ("direct1", [
      ([1, 0, 1], []),
      ([1, 1, 1], []),
      ([1, 2, 1], ["direct2 ==1.2.1"])
    ]),
    ("direct2", [
      ([1, 0, 1], []),
      ([1, 1, 1], []),
      ([1, 2, 1], [])
    ]),
    ("direct3", [
      ([1, 0, 1], []),
      ([1, 1, 1], []),
      ([1, 2, 1], [])
    ]),
    ("direct4", [
      ([1, 0, 1], []),
      ([1, 1, 1], []),
      ([1, 2, 1], [])
    ]),
    ("direct5", [
      ([1, 0, 1], []),
      ([1, 1, 1], []),
      ([1, 2, 1], [])
    ])
  ]

ghcPackages :: GhcPackages
ghcPackages = GhcPackages {installed, available = packageDb}

build :: Versions -> M BuildStatus
build = \case
  [("base", [4, 12, 0, 0]), ("direct1", [1, 2, 1]), ("direct2", [1, 2, 1]), ("direct3", [1, 0, 1]), ("direct4", [1, 0, 1]), ("direct5", [1, 1, 1])] -> pure Success
  [("base", [4, 12, 0, 0]), ("direct1", [1, 2, 1]), ("direct2", [1, 2, 1]), ("direct3", [1, 2, 1]), ("direct4", [1, 0, 1]), ("direct5", [1, 1, 1])] -> pure Failure
  [("base", [4, 12, 0, 0]), ("direct1", [1, 2, 1]), ("direct2", [1, 2, 1]), ("direct3", [1, 0, 1]), ("direct4", [1, 2, 1]), ("direct5", [1, 1, 1])] -> pure Success
  [("base", [4, 12, 0, 0]), ("direct1", [1, 2, 1]), ("direct2", [1, 2, 1]), ("direct3", [1, 0, 1]), ("direct4", [1, 2, 1]), ("direct5", [1, 2, 1])] -> pure Success
  -- second iteration
  [("base", [4, 12, 0, 0]), ("direct1", [1, 2, 1]), ("direct2", [1, 2, 1]), ("direct3", [1, 2, 1]), ("direct4", [1, 2, 1]), ("direct5", [1, 2, 1])] -> pure Failure
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

state :: ProjectStateProto
state =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("direct1", ">=0.1"),
        ("direct2", ">=0.1"),
        ("direct4", "^>=1.0")
      ])
    ],
    versions = [("fancy", [("direct4", "1.0.1")])],
    overrides = [
      ("fancy", [("direct5", "direct5-1.1.1")])
    ],
    initial = [],
    resolving = False
  }

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      base = {
        lower = null;
        upper = "4.13";
      };
      direct1 = {
        lower = "0.1";
        upper = "1.3";
      };
      direct2 = {
        lower = "0.1";
        upper = "1.3";
      };
      direct3 = {
        lower = null;
        upper = "1.1";
      };
      direct4 = {
        lower = "1.0";
        upper = "1.3";
      };
      direct5 = {
        lower = null;
        upper = "1.3";
      };
    };
  };
  versions = {
    fancy = {
      base = "4.12.0.0";
      direct1 = "1.2.1";
      direct2 = "1.2.1";
      direct3 = "1.0.1";
      direct4 = "1.2.1";
      direct5 = "1.2.1";
    };
  };
  initial = {
    fancy = {};
  };
  overrides = {
    fancy = {
      direct1 = {
        version = "1.2.1";
        hash = "direct1-1.2.1";
      };
      direct2 = {
        version = "1.2.1";
        hash = "direct2-1.2.1";
      };
      direct4 = {
        version = "1.2.1";
        hash = "direct4-1.2.1";
      };
      direct5 = {
        version = "1.2.1";
        hash = "direct5-1.2.1";
      };
    };
  };
  resolving = false;
}
|]

-- | Goals for these deps:
--
-- Unless stated otherwise, packages have an installed version, 1.0.1, and a latest available version, 1.2.1, which will
-- be their candidate.
--
-- - @base@ is marked as non-reinstallable in Cabal, so it will never produce a plan that includes a different version
--   than its installed one, 4.12.0.0.
--   Its mutation is excluded from being built by matching its name, so it will only be in the solver bounds, and
--   therefore still get a managed bound, version and initial entry in the state file, but no override.
--
-- - @direct1@ The candidate has a dep on @direct2 ==1.2.1@, so the solver chooses 1.2.1 for both deps in the first
--   mutation, which succeeds.
--
-- - @direct2@ has been updated to the latest version by the mutation for @direct1@ and will try the same version again,
--   resulting in success.
--
-- - @direct3@ has the latest version 1.2.1, for which the build fails.
--   It will be tried again in the second iteration, where it will fail once more.
--   It will not be bumped, but an upper bound will be added based on its installed version, 1.0.1, as @<1.1@.
--   No override will be added to the state.
--   TODO We could use multiple condidates, between the latest version and the maximum of the prior upper bound and the
--   installed version.
--   Probably only the latest in each major.
--
-- - @direct4@ has preexisting managed bounds, @^>=1.0@.
--   The candidate succeeds, so the lower bound will be preserved, to yield the final bounds, @>=1.0 && <1.3@.
--   It has an existing entry in @initial@, which will be preserved.
--
-- - @direct5@ has an existing override for version 1.1.1, which will be replaced by the successful candidate 1.2.1.
test_bumpMutation :: UnitTest
test_bumpMutation = do
  Result {stateFile, log} <- lowerTest params bumpOptimizeMain
  eqLines stateFileTarget (renderRootExpr stateFile)
  where
    params = (testParams False packages) {
      log = True,
      envs = [("fancy", ["local1"])],
      ghcPackages,
      state,
      build
    }
