module Hix.Test.Managed.LowerOptimize.MutationTest where

import Data.Aeson (eitherDecodeStrict')
import Data.IORef (IORef, readIORef)
import Distribution.Pretty (pretty)
import Distribution.Version (Version, VersionRange, orLaterVersion)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe)
import Path (Abs, Dir, Path, absdir, relfile)

import Hix.Data.Bounds (TargetBound (TargetLower), TargetBounds)
import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Client, Fatal))
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerOptimizeConfig (LowerOptimizeConfig))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import Hix.Data.NixExpr (Expr)
import Hix.Data.OutputFormat (OutputFormat (OutputNone))
import Hix.Data.Overrides (EnvOverrides)
import Hix.Data.Package (LocalPackage, PackageName (PackageName))
import Hix.Data.Version (NewVersion (..), SourceHash (SourceHash))
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedConfig (ManagedConfig), StateFileConfig (StateFileConfig))
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import qualified Hix.Managed.Handlers.Build.Test as BuildHandlers
import Hix.Managed.Handlers.Hackage (fetchHash)
import Hix.Managed.Handlers.LowerOptimize (LowerOptimizeHandlers (..), versions)
import qualified Hix.Managed.Handlers.LowerOptimize.Test as LowerOptimizeHandlers
import qualified Hix.Managed.Handlers.Solve
import Hix.Managed.Handlers.Solve (SolveHandlers (SolveHandlers))
import Hix.Managed.Lower.App (lowerOptimize)
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimize)
import Hix.Monad (M, runMWith, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Test.Hedgehog (eqLines)
import qualified Hix.Test.Managed.Solver
import Hix.Test.Managed.Solver (TestDeps (TestDeps), testSolver)
import Hix.Test.Utils (UnitTest)

root :: Path Abs Dir
root = [absdir|/project|]

tmpRoot :: Path Abs Dir
tmpRoot = [absdir|/tmp/project|]

fetchVersions :: PackageName -> M [Version]
fetchVersions = \case
  "direct1" -> pure basic
  "direct2" -> pure [[1, 7, 1], [1, 8, 1], [1, 9, 1], [1, 9, 2], [2, 0, 1]]
  "direct3" -> pure basic
  "transitive1" -> pure [[1, 0, 1]]
  package -> throwM (Client [exon|No such package: ##{package}|])
  where
    basic = [[1, 8, 1], [1, 9, 1], [2, 0, 1]]

fetchHash :: PackageName -> Version -> M SourceHash
fetchHash (PackageName name) v =
  pure (SourceHash [exon|#{name}-#{show (pretty v)}|])

td :: Int -> (PackageName, VersionRange)
td n =
  ([exon|transitive#{show n}|], orLaterVersion [1, 0])

byPackage :: Map PackageName [(PackageName, VersionRange)]
byPackage =
  [
    ("direct1", [td 1])
  ]

testDeps :: TestDeps
testDeps = TestDeps {fetchVersions, byPackage, byVersion = []}

buildProjectTest :: Path Abs Dir -> EnvName -> LocalPackage -> NewVersion -> M Bool
buildProjectTest _ _ target newVersion =
  case newVersion.package of
    "direct1" -> pure True
    "direct2" -> case newVersion.version of
      [1, 8, 1] -> pure False
      [1, 9, 1] -> pure False
      _ -> pure True
    "direct3" -> pure True
    pkg -> throwM (Fatal [exon|Unexpected dep for building ##{target}: ##{pkg}|])

handlersTest :: IO (LowerOptimizeHandlers, IORef [Expr], IORef [DepMutation LowerOptimize])
handlersTest = do
  (build, stateFileRef) <- BuildHandlers.handlersUnitTest tmpRoot
  (handlers, bumpsRef) <- LowerOptimizeHandlers.handlersUnitTest
  let handlers' = handlers {
    build = build {
      buildProject = buildProjectTest,
      solve = SolveHandlers {solveForVersion = testSolver testDeps},
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

managedBoundsFile :: Either String TargetBounds
managedBoundsFile =
  eitherDecodeStrict' [exon|{
    "local1": {
      "direct1": ">=2.0 && <2.1",
      "direct2": ">=2.0 && <2.1"
    }
  }|]

managedOverridesFile :: Either String EnvOverrides
managedOverridesFile =
  eitherDecodeStrict' [exon|{
    "lower": {
      "direct1": {
        "version": "2.0.1",
        "hash": "direct1-2.0.1"
      },
      "direct2": {
        "version": "2.0.1",
        "hash": "direct2-2.0.1"
      },
      "direct3": {
        "version": "2.0.1",
        "hash": "direct3-2.0.1"
      }
    }
  }|]

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    local1 = {
      direct1 = ">=1.8.1 && <2.1";
      direct2 = ">=1.9.2 && <2.1";
      direct3 = ">=0";
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
--
-- - @direct3@ has no managed bounds, resulting in it being skipped, and getting a trivial bound in the state file.
test_lowerOptimizeMutation :: UnitTest
test_lowerOptimizeMutation = do
  deps <- leftA fail depsConfig
  managedBounds <- leftA fail managedBoundsFile
  managedOverrides <- leftA fail managedOverridesFile
  (handlers, stateFileRef, _) <- liftIO handlersTest
  let
    env =
      ManagedEnv {
        deps,
        state = ManagedEnvState {bounds = managedBounds, overrides = managedOverrides, resolving = False},
        lower = ManagedLowerEnv {solverBounds = mempty},
        targets = ["local1"]
      }
    conf =
      ManagedConfig {
        ghc = Nothing,
        stateFile = StateFileConfig {
          file = [relfile|ops/managed.nix|],
          updateProject = True,
          projectRoot = Just root,
          latestOverrides = True
        },
        env = "lower",
        targetBound = TargetLower
      }
    lowerConf = LowerOptimizeConfig {oldest = False, initialBounds = []}
  evalEither =<< liftIO do
    runMWith False False True OutputNone root $ runManagedApp handlers.build handlers.report env conf \ app ->
      Right <$> lowerOptimize handlers lowerConf app
  stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
  eqLines stateFileTarget (renderRootExpr stateFile)
