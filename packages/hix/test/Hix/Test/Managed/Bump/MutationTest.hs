module Hix.Test.Managed.Bump.MutationTest where

import qualified Data.Text as Text
import Exon (exon)
import Test.Tasty (TestTree, testGroup)

import Hix.Class.Map ((!!))
import Hix.Data.Error (ErrorMessage (Fatal))
import Hix.Data.Options (ProjectOptions (..))
import Hix.Data.Overrides (IsRevision (..), Override (..), Overrides)
import Hix.Data.Version (SourceHash (..), Versions)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages), InstalledPackages)
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Data.ManagedPackage (ProjectPackages, managedPackages)
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines, listEqTail)
import qualified Hix.Test.Managed.Run
import Hix.Test.Managed.Run (Result (Result), TestParams (..), bumpTest, nosortOptions, testParams, withoutRevisions)
import Hix.Test.Utils (UnitTest, unitTest)

packages :: ProjectPackages
packages =
  managedPackages [
    (("local1", "1.0"), [
      "base",
      "direct1",
      "direct2",
      "direct3",
      "direct4",
      "direct5",
      "direct6",
      "direct7",
      "direct8",
      "local1",
      "local2 <0.9",
      "local3 <0.9",
      "local4 <0.9",
      "local5"
    ]),
    (("local2", "1.0"), []),
    (("local3", "1.0"), []),
    (("local4", "1.0"), []),
    (("local5", "1.0"), [])
  ]

installed :: InstalledPackages
installed =
  [
    ("base-4.12.0.0", []),
    ("direct1-1.0.1", []),
    ("direct2-1.0.1", []),
    ("direct3-1.0.1", []),
    ("direct4-1.0.1", []),
    ("direct5-1.0.1", []),
    ("direct6-1.0.1", []),
    ("direct7-1.0.1", ["transitive1-1.0.1"]),
    ("direct8-1.0.1", []),
    ("transitive1-1.0.1", ["direct6-1.0.1"])
  ]

available :: SourcePackages
available =
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
      ([1, 2, 1], ["direct4 <1.2"])
    ]),
    ("direct6", [
      ([1, 0, 1], []),
      ([1, 1, 1], [])
    ]),
    ("direct7", [
      ([1, 0, 1], ["transitive1 <1.1"])
    ]),
    ("direct8", [
      ([1, 0, 1], [])
    ]),
    ("transitive1", [
      ([1, 0, 1], ["direct6 <1.2"])
    ]),
    ("local2", [
      ([0, 8, 0], []),
      ([0, 9, 0], [])
    ]),
    ("local3", [
      ([0, 8, 0], []),
      ([0, 9, 0], [])
    ]),
    ("local4", [
      ([0, 8, 0], []),
      ([0, 9, 0], []),
      ([0, 10, 0], [])
    ])
  ]

ghcPackages :: GhcPackages
ghcPackages = GhcPackages {installed, available}

build :: Versions -> M (BuildStatus, Overrides)
build = \case
  versions
    | Just [0, 9, 0] <- versions !! "local2"
    -> failure
  versions
    | Just [0, 9, 0] <- versions !! "local3"
    -> failure
  [
    ("base", [4, 12, 0, 0]),
    ("direct1", [1, 2, 1]),
    ("direct2", [1, 2, 1]),
    ("direct3", [1, 0, 1]),
    ("direct4", [1, 0, 1]),
    ("direct5", [1, 1, 1]),
    ("direct6", [1, 0, 1]),
    ("direct7", [1, 0, 1]),
    ("direct8", [1, 0, 1]),
    ("local2", [0, 8, 0]),
    ("local3", [0, 8, 0]),
    ("local4", [0, 8, 0]),
    ("local5", [1, 0]),
    ("transitive1", [1, 0, 1])
    ] -> pure (Success, [
      ("direct8", Override {
        version = [1, 0, 1],
        hash = SourceHash "direct8-1.0.1",
        repo = Nothing,
        revision = Just IsRevision
      })
    ])

  [
    ("base", [4, 12, 0, 0]),
    ("direct1", [1, 2, 1]),
    ("direct2", [1, 2, 1]),
    ("direct3", [1, 2, 1]),
    ("direct4", [1, 0, 1]),
    ("direct5", [1, 1, 1]),
    ("direct6", [1, 0, 1]),
    ("direct7", [1, 0, 1]),
    ("direct8", [1, 0, 1]),
    ("local2", [0, 8, 0]),
    ("local3", [0, 8, 0]),
    ("local4", [0, 8, 0]),
    ("local5", [1, 0]),
    ("transitive1", [1, 0, 1])
    ] -> failure

  [
    ("base", [4, 12, 0, 0]),
    ("direct1", [1, 2, 1]),
    ("direct2", [1, 2, 1]),
    ("direct3", [1, 0, 1]),
    ("direct4", [1, 2, 1]),
    ("direct5", [1, 1, 1]),
    ("direct6", [1, 0, 1]),
    ("direct7", [1, 0, 1]),
    ("direct8", [1, 0, 1]),
    ("local2", [0, 8, 0]),
    ("local3", [0, 8, 0]),
    ("local4", [0, 8, 0]),
    ("local5", [1, 0]),
    ("transitive1", [1, 0, 1])
    ] -> success

  [
    ("base", [4, 12, 0, 0]),
    ("direct1", [1, 2, 1]),
    ("direct2", [1, 2, 1]),
    ("direct3", [1, 0, 1]),
    ("direct4", [1, 2, 1]),
    ("direct5", [1, 2, 1]),
    ("direct6", [1, 0, 1]),
    ("direct7", [1, 0, 1]),
    ("direct8", [1, 0, 1]),
    ("local2", [0, 8, 0]),
    ("local3", [0, 8, 0]),
    ("local4", [0, 8, 0]),
    ("local5", [1, 0]),
    ("transitive1", [1, 0, 1])
    ] -> success

  [
    ("base", [4, 12, 0, 0]),
    ("direct1", [1, 2, 1]),
    ("direct2", [1, 2, 1]),
    ("direct3", [1, 0, 1]),
    ("direct4", [1, 2, 1]),
    ("direct5", [1, 2, 1]),
    ("direct6", [1, 1, 1]),
    ("direct7", [1, 0, 1]),
    ("direct8", [1, 0, 1]),
    ("local2", [0, 8, 0]),
    ("local3", [0, 8, 0]),
    ("local4", [0, 8, 0]),
    ("local5", [1, 0]),
    ("transitive1", [1, 0, 1])
    ] -> success

  [
    ("base", [4, 12, 0, 0]),
    ("direct1", [1, 2, 1]),
    ("direct2", [1, 2, 1]),
    ("direct3", [1, 0, 1]),
    ("direct4", [1, 2, 1]),
    ("direct5", [1, 2, 1]),
    ("direct6", [1, 1, 1]),
    ("direct7", [1, 0, 1]),
    ("direct8", [1, 0, 1]),
    ("local2", [0, 8, 0]),
    ("local3", [0, 8, 0]),
    ("local4", [0, 8, 0]),
    ("local5", [1, 0]),
    ("transitive1", [1, 0, 1])
    ] -> success

  [
    ("base", [4, 12, 0, 0]),
    ("direct1", [1, 2, 1]),
    ("direct2", [1, 2, 1]),
    ("direct3", [1, 0, 1]),
    ("direct4", [1, 2, 1]),
    ("direct5", [1, 2, 1]),
    ("direct6", [1, 1, 1]),
    ("direct7", [1, 0, 1]),
    ("direct8", [1, 0, 1]),
    ("local2", [0, 8, 0]),
    ("local3", [0, 8, 0]),
    ("local4", [0, 10, 0]),
    ("local5", [1, 0]),
    ("transitive1", [1, 0, 1])
    ] -> success

--   -- second iteration
  [
    ("base", [4, 12, 0, 0]),
    ("direct1", [1, 2, 1]),
    ("direct2", [1, 2, 1]),
    ("direct3", [1, 2, 1]),
    ("direct4", [1, 2, 1]),
    ("direct5", [1, 2, 1]),
    ("direct6", [1, 1, 1]),
    ("direct7", [1, 0, 1]),
    ("direct8", [1, 0, 1]),
    ("local2", [0, 8, 0]),
    ("local3", [0, 8, 0]),
    ("local4", [0, 10, 0]),
    ("local5", [1, 0]),
    ("transitive1", [1, 0, 1])
    ] -> failure

  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])
  where
    success = pure (Success, mempty)

    failure = pure (Failure, mempty)

state :: ProjectStateProto
state =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("direct1", ">=0.1"),
        ("direct2", ">=0.1"),
        ("direct4", "^>=1.0"),
        ("direct6", "<1.1")
      ])
    ],
    versions = [("fancy", [("direct4", "1.0.1")])],
    overrides = [
      ("fancy", [("direct5", "direct5-1.1.1")])
    ],
    solver = [],
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
      direct6 = {
        lower = null;
        upper = "1.2";
      };
      direct7 = {
        lower = null;
        upper = "1.1";
      };
      direct8 = {
        lower = null;
        upper = "1.1";
      };
      local1 = {
        lower = null;
        upper = null;
      };
      local2 = {
        lower = null;
        upper = "0.9";
      };
      local3 = {
        lower = null;
        upper = "0.9";
      };
      local4 = {
        lower = null;
        upper = "0.11";
      };
      local5 = {
        lower = null;
        upper = "1.1";
      };
    };
    local2 = {};
    local3 = {};
    local4 = {};
    local5 = {};
  };
  versions = {
    fancy = {
      base = "4.12.0.0";
      direct1 = "1.2.1";
      direct2 = "1.2.1";
      direct3 = "1.0.1";
      direct4 = "1.2.1";
      direct5 = "1.2.1";
      direct6 = "1.1.1";
      direct7 = "1.0.1";
      direct8 = "1.0.1";
      local2 = "0.8.0";
      local3 = "0.8.0";
      local4 = "0.10.0";
      local5 = "1.0";
    };
    other = {};
  };
  initial = {
    fancy = {};
    other = {};
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
      direct6 = {
        version = "1.1.1";
        hash = "direct6-1.1.1";
      };
      direct7 = {
        version = "1.0.1";
        hash = "direct7-1.0.1";
      };
      direct8 = {
        version = "1.0.1";
        hash = "direct8-1.0.1";
        revision = true;
      };
      local2 = {
        version = "0.8.0";
        hash = "local2-0.8.0";
      };
      local3 = {
        version = "0.8.0";
        hash = "local3-0.8.0";
      };
      local4 = {
        version = "0.10.0";
        hash = "local4-0.10.0";
      };
      local5 = {
        version = "1.0";
        hash = "local5-1.0";
      };
      transitive1 = {
        version = "1.0.1";
        hash = "transitive1-1.0.1";
      };
    };
  };
  solver = {
    fancy = {};
  };
  resolving = false;
}
|]

logTarget :: [Text]
logTarget =
  Text.lines [exon|
[35m[1m>>>[0m [33mfancy[0m
[35m[1m>>>[0m Couldn't find working latest versions for some deps after 2 iterations.
    📦 direct3
    📦 local2
    📦 local3
[35m[1m>>>[0m Added new versions:
    📦 [34mbase[0m      4.12.0.0   ↕ [no bounds] -> <[32m4.13[0m
    📦 [34mdirect1[0m   1.2.1      ↕ >=0.1 -> [0.1, [32m1.3[0m]
    📦 [34mdirect2[0m   1.2.1      ↕ >=0.1 -> [0.1, [32m1.3[0m]
    📦 [34mdirect3[0m   1.0.1      ↕ [no bounds] -> <[32m1.1[0m
    📦 [34mdirect5[0m   1.2.1      ↕ [no bounds] -> <[32m1.3[0m
    📦 [34mdirect6[0m   1.1.1      ↕ <[31m1.1[0m -> <[32m1.2[0m
    📦 [34mdirect7[0m   1.0.1      ↕ [no bounds] -> <[32m1.1[0m
    📦 [34mdirect8[0m   1.0.1      ↕ [no bounds] -> <[32m1.1[0m
    📦 [34mlocal2[0m    0.8.0      ↕ <0.9
    📦 [34mlocal3[0m    0.8.0      ↕ <0.9
    📦 [34mlocal4[0m    0.10.0     ↕ <[31m0.9[0m -> <[32m0.11[0m
    📦 [34mlocal5[0m    1.0        ↕ [no bounds] -> <[32m1.1[0m
[35m[1m>>>[0m Updated versions:
    📦 [34mdirect4[0m   [31m1.0.1[0m -> [32m1.2.1[0m   ↕ [1.0, [31m1.1[0m] -> [1.0, [32m1.3[0m]
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
--   It has a dependency on @direct4@ with an upper bound of <1.2, which would prevent it from being selected by the
--   solver if we didn't use @AllowNewer@.
--
-- - @direct6@ is a direct dependency of the project, and a direct dependency of @transitive1@.
--   @transitive1@ is a direct dependency of @direct7@, and therefore @direct6@ is a transitive dependency of both
--   @direct7@ and the project.
--   While @direct7@ and @transitive1@ both have their latest versions installed, the installed version of @transitive1@
--   has a dependency on @direct6-1.0.1@ (since installed versions don't have range deps, and need the precise package
--   ID of dependencies with which they were installed).
--   When @direct6@ is then bumped regularly to the latest version, both @transitive1@ and @direct7@ are overridden as
--   well by the solver.
--
-- - The installed version of @direct8@ is simulated to have incompatible bounds, which is indicated by the fact that
--   'build' returns a revision override for it in the first build.
--   In later builds, the solver configures an override for @direct8@ initially, since revisions are persisted across
--   mutations in @SolverState@.
--   In the final state file, the override for @direct8@ includes the flag @revision = true@.
--
-- - @local2@ is in a different env, so it will be treated as a mutable dependency despite being a local package.
--   It also has no installed version, since it wasn't available on Hackage (yet) at the time of the nixpkgs snapshot
--   and therefore isn't present in nix;
--   and because packages from other envs aren't added as local derivations to the solver GHC.
--   It also has the bound @<0.9@ in the config (but not the existing state) that prevents other mutations from building
--   with @0.9.0@, which would break the build.
--
-- - @local3@ is like @local2@ and exists primarily to ensure that none of the two deps can be mutated without the
--   constraint that keeps the other below 0.9.
--
-- - @local4@ is like @local2@ and @local3@, with the slight variation that it has a buildable version 0.10.0, which
--   is not used for prior mutations (rather, like the other two, it uses 0.8.0), but its own mutation succeeds.
--
-- - @local5@ is like @local2@, with the variation that it also has no available versions.
--   This has the effect that when initializing the Cabal resources, a synthetic entry for this dep is inserted into the
--   source package DB that contains the available packages (in production, those would be from Hackage snapshots).
test_bumpMutationBasic :: UnitTest
test_bumpMutationBasic = do
  Result {stateFile, log} <- bumpTest params bumpOptimizeMain
  eqLines stateFileTarget (renderRootExpr stateFile)
  listEqTail logTarget (reverse log)
  where
    params = (testParams False packages) {
      log = True,
      envs = [
        ("fancy", (Nothing, ["local1"])),
        ("other", (Nothing, ["local2", "local3", "local4", "local5"]))
      ],
      ghcPackages,
      state,
      projectOptions = nosortOptions {envs = ["fancy"], readUpperBounds = True, localDeps = True},
      build
    }

packages_upToDate :: ProjectPackages
packages_upToDate =
  managedPackages [(("local1", "1.0"), ["direct1"])]

installed_upToDate :: InstalledPackages
installed_upToDate =
  [("direct1-1.0.0", [])]

packageDb_upToDate :: SourcePackages
packageDb_upToDate =
  [("direct1", [([1, 0, 1], [])])]

ghcPackages_upToDate :: GhcPackages
ghcPackages_upToDate = GhcPackages {installed = installed_upToDate, available = packageDb_upToDate}

state_upToDate :: ProjectStateProto
state_upToDate =
  ProjectStateProto {
    bounds = [("local1", [("direct1", ">=0.1 && <1.1")])],
    versions = [],
    initial = [],
    overrides = [("latest", [("direct1", "direct1-1.0.1")])],
    solver = [],
    resolving = False
  }

build_upToDate :: Versions -> M BuildStatus
build_upToDate = \case
  [("direct1", [1, 0, 1])] -> pure Success
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

stateFileTarget_upToDate :: Text
stateFileTarget_upToDate =
  [exon|{
  bounds = {
    local1 = {
      direct1 = {
        lower = "0.1";
        upper = "1.1";
      };
    };
  };
  versions = {
    latest = {
      direct1 = "1.0.1";
    };
  };
  initial = {
    latest = {};
  };
  overrides = {
    latest = {
      direct1 = {
        version = "1.0.1";
        hash = "direct1-1.0.1";
      };
    };
  };
  solver = {
    latest = {};
  };
  resolving = false;
}
|]

-- | This has a minimal configuration: Only one target with one dep with one version.
-- The version is already in the overrides and bounds.
-- The only thing missing is the entry in @versions@.
--
-- In the final state, the version should have been added.
--
-- This is a bugfix test, since at some point, the versions would not have been written back if _all_ candidates
-- resulted in @MutationKeep@ because they had been classified as up to date based on the comparison of the candidate
-- version and the preexisting override.
-- Comparing with the override was wrong to begin with, since that would fail to notice an up to date version if it was
-- installed.
-- But in general, versions from successful builds should be written to the mutation state even for @MutationKeep@.
-- However, this is only significant if the state has been tampered with between runs, since a preexisting override
-- should have a corresponding version entry.
test_bumpMutationUpToDate :: UnitTest
test_bumpMutationUpToDate = do
  Result {stateFile} <- bumpTest params bumpOptimizeMain
  eqLines stateFileTarget_upToDate (renderRootExpr stateFile)
  where
    params = (testParams False packages_upToDate) {
      ghcPackages = ghcPackages_upToDate,
      state = state_upToDate,
      build = withoutRevisions build_upToDate
    }

test_bumpMutation :: TestTree
test_bumpMutation =
  testGroup "bump" [
    unitTest "basic" test_bumpMutationBasic,
    unitTest "up to date" test_bumpMutationUpToDate
  ]
