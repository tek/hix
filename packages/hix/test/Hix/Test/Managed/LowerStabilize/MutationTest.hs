module Hix.Test.Managed.LowerStabilize.MutationTest where

import Data.IORef (readIORef)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe)

import Hix.Data.Error (Error (Fatal))
import Hix.Data.LowerConfig (lowerConfigStabilize)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  EnvConfig (EnvConfig),
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import qualified Hix.Data.Overrides
import Hix.Data.Overrides (Override (Override))
import Hix.Data.Version (SourceHash (SourceHash), Versions)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Data.BuildState (BuildStatus (Failure, Success))
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedConfig (ManagedConfig), ManagedOp (OpLowerStabilize))
import Hix.Managed.Data.ManagedPackage (ManagedPackages, managedPackages)
import Hix.Managed.Handlers.Lower (LowerHandlers (..))
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import Hix.Managed.Lower.Stabilize (lowerStabilize)
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Config (stateFileConfig)
import Hix.Test.Utils (UnitTest, runMTest)

packageDb :: SourcePackages
packageDb =
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

buildVersions :: Versions -> M BuildStatus
buildVersions = \case
  [("direct1", [2, 0, 1]), ("direct2", [2, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 8, 1])] -> pure Failure
  [("direct1", [1, 8, 1]), ("direct2", [2, 0, 1])] -> pure Failure
  [("direct1", [1, 9, 1]), ("direct2", [2, 0, 1])] -> pure Success
  [("direct1", [1, 9, 1]), ("direct2", [1, 8, 1])] -> pure Failure
  [("direct1", [1, 9, 1]), ("direct2", [1, 9, 1])] -> pure Success
  [] -> throwM (Fatal "Build with no overrides")
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

packages :: ManagedPackages
packages =
  managedPackages [(("local1", "1.0"), ["direct1", "direct2"])]

initialState :: ManagedEnvState
initialState =
  ManagedEnvState {
    bounds = [
      ("local1", [
        ("direct1", [[1, 8, 1], [2, 1]]),
        ("direct2", [[1, 8, 1], [2, 1]])
      ])
    ],
    overrides = [
      ("lower", [
        ("direct1", Override {version = [1, 8, 1], hash = SourceHash "direct1-1.8.1"}),
        ("direct2", Override {version = [1, 8, 1], hash = SourceHash "direct2-1.8.1"})
      ])
    ],
    lowerInit = [
      ("lower", [
        ("direct1", [2, 0, 1]),
        ("direct2", [2, 0, 1])
      ])
    ],
    resolving = False
  }

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = ">=1.9.1 && <2.1";
      direct2 = ">=1.9.1 && <2.1";
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
  lowerInit = {
    lower = {
      direct1 = "2.0.1";
      direct2 = "2.0.1";
    };
  };
  resolving = false;
}
|]

test_lowerStabilizeMutation :: UnitTest
test_lowerStabilizeMutation = do
  (handlers, stateFileRef, _) <- liftIO (LowerHandlers.handlersUnitTest buildVersions packages [] packageDb)
  let
    env =
      ManagedEnv {
        packages,
        state = initialState,
        lower = ManagedLowerEnv {solverBounds = mempty},
        envs = [("lower", EnvConfig {targets = ["local1"], ghc = Nothing})],
        buildOutputsPrefix = Nothing
      }
    conf =
      ManagedConfig {
        stateFile = stateFileConfig,
        envs = ["lower"]
      }
  evalEither =<< liftIO do
    runMTest False $ runManagedApp handlers.build handlers.report env conf OpLowerStabilize \ app ->
      Right <$> lowerStabilize handlers lowerConfigStabilize app
  finalStateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr finalStateFile)
