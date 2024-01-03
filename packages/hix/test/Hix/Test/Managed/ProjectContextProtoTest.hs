module Hix.Test.Managed.ProjectContextProtoTest where

import Data.Aeson (eitherDecodeStrict')
import Distribution.Version (earlierVersion, intersectVersionRanges, majorBoundVersion, orLaterVersion, thisVersion)
import Exon (exon)
import Hedgehog (evalEither, (===))
import Path (absdir)

import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep (Dep), mkDep)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Version (range0)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem), GhcPath (GhcPath))
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext (EnvContext), EnvDeps (EnvDeps))
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage (ManagedPackage))
import qualified Hix.Managed.Data.ManagedPackageProto
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto (ManagedPackageProto))
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext (ProjectContext))
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (ProjectContextProto))
import qualified Hix.Managed.Data.ProjectState
import Hix.Managed.Data.ProjectState (ProjectState (ProjectState))
import qualified Hix.Managed.Data.ProjectStateProto
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import Hix.Managed.Data.Targets (unsafeTargets)
import qualified Hix.Managed.ProjectContextProto as ProjectContextProto
import Hix.Test.Hedgehog (assertRight)
import Hix.Test.Utils (UnitTest, runMTest)

json :: ByteString
json =
  [exon|{
    "packages": {
      "local1": {
        "name": "local1",
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
      },
      "local2": {
        "name": "local2",
        "version": "1.1.1",
        "deps": [
          "local1"
        ]
      },
      "local3": {
        "name": "local3",
        "version": "1.1.1",
        "deps": []
      },
      "local4": {
        "name": "local4",
        "version": "1.1.1",
        "deps": [
          "local2",
          "direct1",
          "direct2"
        ]
      },
      "local5": {
        "name": "local5",
        "version": "1.1.1",
        "deps": [
          "local4",
          "local3"
        ]
      }
    },
    "state": {
      "bounds": {
        "local1": {
          "direct1": {
            "lower": null,
            "upper": "2.3"
          },
          "direct2": {
            "upper": "1.5"
          }
        }
      }
    },
    "solverBounds": {
      "direct1": ">=2.0.2"
    },
    "envs": {
      "lower-main": {
        "targets": ["local1", "local2"],
        "ghc": "/ghc"
      },
      "lower-special": {
        "targets": ["local3", "local4"],
        "ghc": "/ghc"
      }
    }
  }|]

packages :: Packages ManagedPackageProto
packages =
  [
    ("local1", ManagedPackageProto {
      name = "local1",
      version = "1.2.1",
      deps = [
        mkDep "direct1" (majorBoundVersion [2, 0]),
        mkDep "direct2" (earlierVersion [1, 5]),
        mkDep "direct3" range0,
        Dep {
          package = "direct4",
          version = thisVersion [13, 23]
        },
        mkDep "direct5" (intersectVersionRanges (orLaterVersion [5, 0]) (earlierVersion [5, 1]))
      ]
    }),
    ("local2", ManagedPackageProto {
      name = "local2",
      version = "1.1.1",
      deps = ["local1"]
    }),
    ("local3", ManagedPackageProto {
      name = "local3",
      version = "1.1.1",
      deps = []
    }),
    ("local4", ManagedPackageProto {
      name = "local4",
      version = "1.1.1",
      deps = ["local2", "direct1", "direct2"]
    }),
    ("local5", ManagedPackageProto {
      name = "local5",
      version = "1.1.1",
      deps = ["local4", "local3"]
    })
  ]

ghc :: GhcDb
ghc =
  GhcDbSystem (Just (GhcPath [absdir|/ghc|]))

targetProto :: ProjectContextProto
targetProto =
  ProjectContextProto {
    packages,
    state = ProjectStateProto {
      bounds = [("local1", [("direct1", "<2.3"), ("direct2", "<1.5")])],
      versions = [],
      overrides = mempty,
      initial = mempty,
      resolving = False
    },
    envs = [
      ("lower-main", EnvConfig {targets = ["local1", "local2"], ghc}),
      ("lower-special", EnvConfig {targets = ["local3", "local4"], ghc})
    ],
    buildOutputsPrefix = Nothing
  }

targetEnvs :: NonEmpty (Either EnvName EnvContext)
targetEnvs =
  [
    Right EnvContext {
      env = "lower-main",
      ghc,
      targets = unsafeTargets ["local1", "local2"],
      deps = EnvDeps {mutable = ["direct1", "direct2", "direct3", "direct4", "direct5"], local = []},
      query = ["direct1", "direct2", "direct3", "direct4", "direct5"],
      solverBounds = mempty
    },
    Right EnvContext {
      env = "lower-special",
      ghc,
      targets = unsafeTargets ["local3", "local4"],
      deps = EnvDeps {mutable = ["direct1", "direct2"], local = ["local2"]},
      query = ["direct1", "direct2"],
      solverBounds = mempty
    }
  ]

targetProject :: ProjectContext
targetProject =
  ProjectContext {
    build = def,
    packages = [
      ("local1", ManagedPackage {
        package = "local1",
        version = [1, 2, 1],
        local = [],
        mutable = [
          ("direct1", "^>=2.0"),
          ("direct2", "<1.5"),
          ("direct3", ">=0"),
          ("direct4", "==13.23"),
          ("direct5", ">=5.0 && <5.1")
        ]
      }),
      ("local2", ManagedPackage {
        package = "local2",
        version = [1, 1, 1],
        local = [("local1", ">=0")],
        mutable = []
      }),
      ("local3", ManagedPackage {
        package = "local3",
        version = [1, 1, 1],
        local = [],
        mutable = []
      }),
      ("local4", ManagedPackage {
        package = "local4",
        version = [1, 1, 1],
        local = [("local2", ">=0")],
        mutable = [("direct1", ">=0"), ("direct2", ">=0")]
      }),
      ("local5", ManagedPackage {
        package = "local5",
        version = [1, 1, 1],
        local = [("local3", ">=0"), ("local4", ">=0")],
        mutable = []
      })
    ],
    state = ProjectState {
      bounds = [
        ("local1", [
          ("direct1", "<2.3"),
          ("direct2", "<1.5"),
          ("direct3", ">=0"),
          ("direct4", ">=0"),
          ("direct5", ">=0")
        ]),
        ("local2", []),
        ("local3", []),
        ("local4", [("direct1", ">=0"), ("direct2", ">=0")]),
        ("local5", [])
      ],
      versions = [
        ("lower-main", [
          ("direct1", Nothing),
          ("direct2", Nothing),
          ("direct3", Nothing),
          ("direct4", Nothing),
          ("direct5", Nothing)
        ]),
        ("lower-special", [
          ("direct1", Nothing),
          ("direct2", Nothing)
        ])
      ],
      overrides = [],
      initial = [
        ("lower-main", [
          ("direct1", Nothing),
          ("direct2", Nothing),
          ("direct3", Nothing),
          ("direct4", Nothing),
          ("direct5", Nothing)
        ]),
        ("lower-special", [
          ("direct1", Nothing),
          ("direct2", Nothing)
        ])
      ],
      resolving = False
    },
    envs = targetEnvs
  }

test_parseProjectContextProto :: UnitTest
test_parseProjectContextProto = do
  assertRight targetProto (eitherDecodeStrict' json)
  project <- evalEither =<< liftIO do
    runMTest False (ProjectContextProto.validate def targetProto)
  targetProject === project
