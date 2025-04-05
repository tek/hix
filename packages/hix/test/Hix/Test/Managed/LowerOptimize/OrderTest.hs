module Hix.Test.Managed.LowerOptimize.OrderTest where

import Hix.Data.Monad (M)
import Hix.Data.Version (Versions)
import Hix.Managed.Cabal.Changes (SolverPlan (..))
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Data.Constraints (EnvConstraints)
import Hix.Managed.Data.ManagedPackage (ProjectPackages, managedPackages)
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.StageState (BuildStatus (Success))
import Hix.Managed.Lower.Optimize (lowerOptimizeMain)
import Hix.Test.Hedgehog (listEqZip)
import Hix.Test.Managed.Run (Result (..), TestParams (..), lowerTest, testParams)
import Hix.Test.Utils (UnitTest)

packages :: ProjectPackages
packages =
  managedPackages [(("local1", "1.0"), ["dep1", "dep2", "dep3"])]

available :: SourcePackages
available =
  [
    ("dep1", [
      ([1, 8], []),
      ([1, 9], []),
      ([2, 0], []),
      ([2, 1], [])
    ]),
    ("dep2", [
      ([1, 9], ["dep1 <1.9"]),
      ([2, 0], []),
      ([2, 1], [])
    ]),
    ("dep3", [
      ([1, 9], ["dep2 <2"]),
      ([2, 0], ["dep2 <2"]),
      ([2, 1], [])
    ])
  ]

ghcPackages :: GhcPackages
ghcPackages = GhcPackages {installed = [], available}

state :: ProjectStateProto
state =
  ProjectStateProto {
    bounds = [
      ("local1", [
        ("dep1", ">=2.1"),
        ("dep2", ">=2.1"),
        ("dep3", ">=2.1")
      ])
    ],
    versions = [
      ("lower", [
        ("dep1", [2, 1]),
        ("dep2", [2, 1]),
        ("dep3", [2, 1])
      ])
    ],
    initial = [
      ("lower", [
        ("dep1", [2, 1]),
        ("dep2", [2, 1]),
        ("dep3", [2, 1])
      ])
    ],
    overrides = [("lower", [])],
    solver = [],
    resolving = False
  }

build :: Versions -> M BuildStatus
build _ = pure Success

cabalTarget :: [(EnvConstraints, Maybe SolverPlan)]
cabalTarget =
  [
    ([
      "dep1 <=2.1",
      "dep2 <=2.1",
      "dep3 ==2.0"
    ], plan ["dep1-1.8", "dep2-1.9", "dep3-2.0"]),
    ([
      "dep1 <=2.1",
      "dep2 <=2.1",
      "dep3 ==1.9"
    ], plan ["dep1-1.8", "dep2-1.9", "dep3-1.9"]),
    ([
      "dep1 <=1.8",
      "dep2 ==2.0",
      "dep3 <=1.9"
    ], Nothing),
    ([
      "dep1 <=1.8",
      "dep2 ==1.9",
      "dep3 <=1.9"
    ], plan ["dep1-1.8", "dep2-1.9", "dep3-1.9"]),
    ([
      "dep1 ==2.0",
      "dep2 <=1.9",
      "dep3 <=1.9"
    ], Nothing),
    ([
      "dep1 ==1.9",
      "dep2 <=1.9",
      "dep3 <=1.9"
    ], Nothing),
    ([
      "dep1 ==1.8",
      "dep2 <=1.9",
      "dep3 <=1.9"
    ], plan ["dep1-1.8", "dep2-1.9", "dep3-1.9"])
  ]
  where
    plan changes = Just SolverPlan {changes, matching = [], nonReinstallable = Nothing}

test_mutationOrder :: UnitTest
test_mutationOrder = do
  Result {cabalLog} <- lowerTest params lowerOptimizeMain
  listEqZip cabalTarget cabalLog
  where
    params =
      (testParams False packages) {
        cabalLog = True,
        ghcPackages,
        state,
        build,
        projectOptions = def
      }
