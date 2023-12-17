module Hix.Test.Managed.Bump.MutationTest where

import Data.IORef (IORef, readIORef)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe)

import Hix.Data.Error (Error (Fatal))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  EnvConfig (EnvConfig),
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import Hix.Data.NixExpr (Expr)
import Hix.Data.Version (Versions)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Bump.App (bump)
import Hix.Managed.Data.BuildState (BuildStatus (Failure, Success))
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedConfig (ManagedConfig))
import Hix.Managed.Data.ManagedOp (ManagedOp (OpBump))
import Hix.Managed.Data.ManagedPackage (ManagedPackages, managedPackages)
import Hix.Managed.Handlers.Build (BuildHandlers (..), versionsBuilder)
import Hix.Managed.Handlers.Bump (BumpHandlers (..), handlersNull)
import qualified Hix.Managed.Handlers.StateFile.Test as StateFileHandlers
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages, queryPackagesLatest)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Config (stateFileConfig)
import Hix.Test.Utils (UnitTest, runMTest)

packages :: ManagedPackages
packages =
  managedPackages [(("panda", "1.0"), [
    "direct1 ^>=1.0",
    "direct2 <1.0",
    "direct3",
    "direct4",
    "direct5 >=1.0 && <1.1"
  ])]

packageDb :: SourcePackages
packageDb =
  [
    ("direct1", [
      ([1, 2, 1], [])
    ]),
    ("direct2", [
      ([1, 2, 1], [])
    ]),
    ("direct3", [
      ([1, 0, 1], [])
    ]),
    ("direct4", [
      ([1, 2, 1], [])
    ]),
    ("direct5", [
      ([1, 0, 1], [])
    ])
  ]

buildVersions :: Versions -> M BuildStatus
buildVersions = \case
  [("direct1", [1, 2, 1]), ("direct5", [1, 0, 1])] -> pure Success
  [("direct1", [1, 2, 1]), ("direct2", [1, 2, 1]), ("direct5", [1, 0, 1])] -> pure Success
  [("direct1", [1, 2, 1]), ("direct2", [1, 2, 1]), ("direct3", [1, 0, 1]), ("direct5", [1, 0, 1])] -> pure Failure
  [("direct1", [1, 2, 1]), ("direct2", [1, 2, 1]), ("direct4", [1, 2, 1]), ("direct5", [1, 0, 1])] -> pure Success
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

handlersTest :: IO (BumpHandlers, IORef [Expr])
handlersTest = do
  (stateFile, stateFileRef) <- StateFileHandlers.handlersUnitTest
  let
    handlers = handlersNull {
      build = handlersNull.build {
        withBuilder = versionsBuilder buildVersions,
        stateFile
      },
      latestVersion = queryPackagesLatest packageDb
    }
  pure (handlers, stateFileRef)

initialState :: ManagedEnvState
initialState =
  ManagedEnvState {
    bounds = [
      ("panda", [("direct4", "^>=1.0")])
    ],
    overrides = [
      ("fancy", [("direct5", "direct5-1.0.1")])
    ],
    lowerInit = [],
    resolving = False
  }

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    panda = {
      direct1 = ">=1.0 && <1.3";
      direct2 = ">=1.2 && <1.3";
      direct3 = ">=0";
      direct4 = ">=1.0 && <1.3";
      direct5 = ">=1.0 && <1.1";
    };
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
        version = "1.0.1";
        hash = "direct5-1.0.1";
      };
    };
  };
  lowerInit = {};
  resolving = false;
}
|]

test_bumpMutation :: UnitTest
test_bumpMutation = do
  (handlers, stateFileRef) <- liftIO handlersTest
  let
    env =
      ManagedEnv {
        packages,
        state = initialState,
        lower = ManagedLowerEnv mempty,
        envs = [("fancy", EnvConfig {targets = ["panda"], ghc = Nothing})],
        buildOutputsPrefix = Nothing
      }
    conf =
      ManagedConfig {
        stateFile = stateFileConfig,
        envs = ["fancy"]
      }
  evalEither =<< liftIO do
    runMTest False do
      runManagedApp handlers.build handlers.report env conf OpBump (bump handlers)
  files <- liftIO (readIORef stateFileRef)
  eqLines stateFileTarget . renderRootExpr =<< evalMaybe (head files)
