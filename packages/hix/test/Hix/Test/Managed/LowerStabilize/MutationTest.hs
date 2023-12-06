module Hix.Test.Managed.LowerStabilize.MutationTest where

import Data.Aeson (eitherDecodeStrict')
import Data.IORef (IORef, readIORef)
import Distribution.Pretty (pretty)
import Distribution.Version (Version)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe)
import Path (Abs, Dir, Path, absdir, relfile)

import Hix.Data.Bounds (TargetBound (TargetLower))
import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.Error (Error (Client, Fatal))
import Hix.Data.LowerConfig (lowerConfigStabilize)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  EnvConfig (EnvConfig),
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import Hix.Data.NixExpr (Expr)
import qualified Hix.Data.Overrides
import Hix.Data.Overrides (Override (Override))
import Hix.Data.Package (PackageName (PackageName))
import Hix.Data.Version (SourceHash (SourceHash), Versions)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.Build (BuildStatus (Failure, Success))
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (
  ManagedConfig (ManagedConfig),
  ManagedOp (OpLowerStabilize),
  StateFileConfig (StateFileConfig),
  )
import Hix.Managed.Handlers.Build (BuildHandlers (..), versionsBuilder)
import qualified Hix.Managed.Handlers.Build.Test as BuildHandlers
import Hix.Managed.Handlers.Hackage (fetchHash)
import Hix.Managed.Handlers.Lower (LowerHandlers (..), versions)
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import qualified Hix.Managed.Handlers.Solve
import Hix.Managed.Handlers.Solve (SolveHandlers (SolveHandlers))
import Hix.Managed.Lower.Stabilize (lowerStabilize)
import Hix.Managed.Lower.Data.Lower (Lower)
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import qualified Hix.Test.Managed.Solver
import Hix.Test.Managed.Solver (TestDeps (TestDeps), testSolver)
import Hix.Test.Utils (UnitTest, runMTest, testRoot)

tmpRoot :: Path Abs Dir
tmpRoot = [absdir|/tmp/project|]

fetchVersions :: PackageName -> M [Version]
fetchVersions = \case
  "direct1" -> pure basic
  "direct2" -> pure basic
  "direct3" -> pure basic
  package -> throwM (Client [exon|No such package: ##{package}|])
  where
    basic = [[1, 8, 1], [1, 9, 1], [2, 0, 1]]

fetchHash :: PackageName -> Version -> M SourceHash
fetchHash (PackageName name) v =
  pure (SourceHash [exon|#{name}-#{show (pretty v)}|])

testDeps :: TestDeps
testDeps = TestDeps {fetchVersions, byPackage = [], byVersion = []}

buildWithState :: Versions -> M BuildStatus
buildWithState = \case
  [("direct1", [2, 0, 1]), ("direct2", [2, 0, 1])] -> pure Success
  [("direct1", [1, 8, 1]), ("direct2", [1, 8, 1])] -> pure Failure
  [("direct1", [1, 8, 1]), ("direct2", [2, 0, 1])] -> pure Failure
  [("direct1", [1, 9, 1]), ("direct2", [2, 0, 1])] -> pure Success
  [("direct1", [1, 9, 1]), ("direct2", [1, 8, 1])] -> pure Failure
  [("direct1", [1, 9, 1]), ("direct2", [1, 9, 1])] -> pure Success
  [] -> throwM (Fatal "Build with no overrides")
  versions -> throwM (Fatal [exon|Unexpected overrides: #{showP versions}|])

handlersTest :: IO (LowerHandlers, IORef [Expr], IORef [DepMutation Lower])
handlersTest = do
  (build, stateFileRef) <- BuildHandlers.handlersUnitTest
  (handlers, bumpsRef) <- LowerHandlers.handlersUnitTest
  let handlers' = handlers {
    solve = \ _ -> pure SolveHandlers {solveForVersion = testSolver testDeps},
    build = build {
      withBuilder = versionsBuilder buildWithState,
      hackage = build.hackage {fetchHash}
    },
    versions = fetchVersions
  }
  pure (handlers', stateFileRef, bumpsRef)

depsConfig :: Either String ConfigDeps
depsConfig =
  eitherDecodeStrict' [exon|{
    "local1": {
      "library": {
        "dependencies": [
          "direct1",
          "direct2",
          "direct3"
        ]
      }
    }
  }|]

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
      direct3 = ">=0";
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
  deps <- leftA fail depsConfig
  (handlers, stateFileRef, _) <- liftIO handlersTest
  let
    env =
      ManagedEnv {
        deps,
        state = initialState,
        lower = ManagedLowerEnv {solverBounds = mempty},
        envs = [("lower", EnvConfig {targets = ["local1"]})],
        buildOutputsPrefix = Nothing
      }
    conf =
      ManagedConfig {
        operation = OpLowerStabilize,
        ghc = Nothing,
        stateFile = StateFileConfig {
          file = [relfile|ops/managed.nix|],
          updateProject = True,
          projectRoot = Just testRoot
        },
        envs = ["lower"],
        targetBound = TargetLower
      }
  evalEither =<< liftIO do
    runMTest False $ runManagedApp handlers.build handlers.report env conf \ app ->
      Right <$> lowerStabilize handlers lowerConfigStabilize app
  finalStateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr finalStateFile)
