module Hix.Test.Managed.LowerOptimize.MutationTest where

import Data.IORef (readIORef)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe)
import Path (Abs, Dir, Path, absdir)

import Hix.Data.Bounds (TargetBound (TargetLower))
import Hix.Data.Error (Error (Fatal))
import Hix.Data.LowerConfig (lowerConfigOptimize)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  EnvConfig (EnvConfig),
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import Hix.Data.Version (Versions)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Data.BuildState (BuildStatus (Failure, Success))
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedConfig (ManagedConfig), ManagedOp (OpLowerOptimize))
import Hix.Managed.Data.ManagedPackage (ManagedPackages, managedPackages)
import Hix.Managed.Handlers.Lower (LowerHandlers (..))
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import Hix.Managed.Lower.Optimize (lowerOptimize)
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages, allDep)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Config (stateFileConfig)
import Hix.Test.Utils (UnitTest, runMTest)

tmpRoot :: Path Abs Dir
tmpRoot = [absdir|/tmp/project|]

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

buildVersions :: Versions -> M BuildStatus
buildVersions = \case
  [("direct1", [1, 9, 1]), ("direct2", [1, 9, 2]), ("transitive1", [1, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 9, 2]), ("transitive1", [1, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 9, 1]), ("transitive1", [1, 0, 1])] -> pure Failure
  [("direct1", [1, 8, 1]), ("direct2", [1, 8, 1]), ("transitive1", [1, 0, 1])] -> pure Failure
  versions -> throwM (Fatal [exon|Unexpected build plan: #{showP versions}|])

packages :: ManagedPackages
packages =
  managedPackages [(("local1", "1.0"), ["direct1", "direct2"])]

initialState :: ManagedEnvState
initialState =
  ManagedEnvState {
    bounds = [
      ("local1", [
        ("direct1", [[2, 0], [2, 1]]),
        ("direct2", [[2, 0], [2, 1]])
      ])
    ],
    overrides = [
      ("lower", [
        ("direct1", "direct1-2.0.1"),
        ("direct2", "direct2-2.0.1")
      ])
    ],
    lowerInit = [],
    resolving = False
  }

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = ">=1.8.1 && <2.1";
      direct2 = ">=1.9.2 && <2.1";
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
  lowerInit = {};
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
        operation = OpLowerOptimize,
        stateFile = stateFileConfig,
        envs = ["lower"],
        targetBound = TargetLower
      }
  evalEither =<< liftIO do
    runMTest False $ runManagedApp handlers.build handlers.report env conf \ app ->
      Right <$> lowerOptimize handlers lowerConfigOptimize app
  stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr stateFile)
