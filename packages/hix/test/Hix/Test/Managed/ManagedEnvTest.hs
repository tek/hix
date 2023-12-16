module Hix.Test.Managed.ManagedEnvTest where

import Data.Aeson (eitherDecodeStrict')
import qualified Distribution.Compat.NonEmptySet as NonEmptySet
import Distribution.PackageDescription (LibraryName (LSubLibName))
import Distribution.Version (earlierVersion, intersectVersionRanges, majorBoundVersion, orLaterVersion, thisVersion)
import Exon (exon)
import Path (absdir)

import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep (Dep), mainDep)
import Hix.Data.ManagedEnv (EnvConfig (..), ManagedEnv (..), ManagedEnvState (..), ManagedLowerEnv (..))
import Hix.Data.Version (range0)
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..), ManagedPackages)
import Hix.Managed.Solve.Config (GhcDb (GhcDb))
import Hix.Test.Hedgehog (assertRight)
import Hix.Test.Utils (UnitTest)

json :: ByteString
json =
  [exon|{
    "packages": {
      "panda": {
        "name": "panda",
        "version": "1.2.1",
        "deps": [
          "direct1 ^>=2.0",
          {
            "name": "direct2",
            "version": "<1.5"
          },
          "direct3",
          "direct4:{internal,external} ==13.23",
          "direct5 >=5.0 && <5.1"
        ]
      }
    },
    "state": {
      "bounds": {
        "panda": {
          "direct2": "<2.3"
        }
      }
    },
    "lower": {
      "solverBounds": {
        "direct1": ">=2.0.2"
      }
    },
    "envs": {
      "lower-main": {
        "targets": ["panda"],
        "ghc": "/ghc"
      }
    }
  }|]

packages :: ManagedPackages
packages =
  [
    ("panda", ManagedPackage {
      name = "panda",
      version = "1.2.1",
      deps = [
        mainDep "direct1" (majorBoundVersion [2, 0]),
        mainDep "direct2" (earlierVersion [1, 5]),
        mainDep "direct3" range0,
        Dep {
          package = "direct4",
          version = thisVersion [13, 23],
          libs = NonEmptySet.fromNonEmpty [LSubLibName "internal", LSubLibName "external"]
        },
        mainDep "direct5" (intersectVersionRanges (orLaterVersion [5, 0]) (earlierVersion [5, 1]))
      ]
    })
  ]

target :: ManagedEnv
target =
  ManagedEnv {
    packages,
    state = ManagedEnvState {
      bounds = [("panda", [("direct2", "<2.3")])],
      overrides = mempty,
      lowerInit = mempty,
      resolving = False
    },
    lower = ManagedLowerEnv {solverBounds = [("direct1", ">=2.0.2")]},
    envs = [("lower-main", EnvConfig {targets = ["panda"], ghc = Just (GhcDb [absdir|/ghc|])})],
    buildOutputsPrefix = Nothing
  }

test_parseManagedEnv :: UnitTest
test_parseManagedEnv =
  assertRight target (eitherDecodeStrict' json)
