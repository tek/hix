module Hix.Test.Managed.Bump.MutationTest where

import Data.Aeson (eitherDecodeStrict')
import Data.IORef (IORef, readIORef)
import Distribution.Version (Version)
import Exon (exon)
import Hedgehog (evalEither, evalMaybe)
import Path (Abs, Dir, Path, absdir, relfile)

import Hix.Data.Bounds (TargetBound (TargetUpper), TargetBounds)
import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.Error (Error (Client, Fatal))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (
  EnvConfig (EnvConfig),
  ManagedEnv (ManagedEnv),
  ManagedEnvState (ManagedEnvState),
  ManagedLowerEnv (ManagedLowerEnv),
  )
import Hix.Data.NixExpr (Expr)
import Hix.Data.Overrides (EnvOverrides)
import Hix.Data.Package (PackageName (PackageName))
import Hix.Data.Version (SourceHash (SourceHash), Versions)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Bump.App (bump)
import Hix.Managed.Data.BuildState (BuildStatus (Failure, Success))
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (
  ManagedConfig (ManagedConfig),
  ManagedOp (OpBump),
  StateFileConfig (StateFileConfig),
  )
import Hix.Managed.Handlers.Build (BuildHandlers (..), versionsBuilder)
import Hix.Managed.Handlers.Bump (BumpHandlers (..), handlersNull)
import Hix.Managed.Handlers.Hackage (fetchHash)
import qualified Hix.Managed.Handlers.StateFile.Test as StateFileHandlers
import Hix.Monad (M, throwM)
import Hix.NixExpr (renderRootExpr)
import Hix.Pretty (showP)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Utils (UnitTest, runMTest, testRoot)

tmpRoot :: Path Abs Dir
tmpRoot = [absdir|/tmp/project|]

depsConfig :: Either String ConfigDeps
depsConfig =
  eitherDecodeStrict' [exon|{
    "panda": {
      "library": {
        "dependencies": [
          "dep1 ^>= 2.0",
          {
            "name": "dep2",
            "version": "< 1.5"
          },
          "dep3",
          "dep4:{internal,external}",
          "dep5 >=5.0 && < 5.1"
        ]
      }
    }
  }|]

latestVersion :: PackageName -> M (Maybe Version)
latestVersion =
  fmap Just . \case
    "dep1" -> pure [2, 2, 0, 5]
    "dep2" -> pure [1, 7, 14]
    "dep3" -> pure [1, 0, 5]
    "dep4" -> pure [2, 2]
    "dep5" -> pure [5, 0]
    _ -> throwM (Client "No such package")

buildWithState :: Versions -> M BuildStatus
buildWithState = \case
  [("dep1", [2, 2, 0, 5]), ("dep5", [5, 0])] -> pure Success
  [("dep1", [2, 2, 0, 5]), ("dep2", [1, 7, 14]), ("dep5", [5, 0])] -> pure Success
  [("dep1", [2, 2, 0, 5]), ("dep2", [1, 7, 14]), ("dep3", [1, 0, 5]), ("dep5", [5, 0])] -> pure Failure
  [("dep1", [2, 2, 0, 5]), ("dep2", [1, 7, 14]), ("dep4", [2, 2]), ("dep5", [5, 0])] -> pure Success
  versions -> throwM (Fatal [exon|Unexpected overrides: #{showP versions}|])

fetchHash :: PackageName -> Version -> M SourceHash
fetchHash "dep5" _ =
  pure (SourceHash "invalid-hash")
fetchHash (PackageName name) _ =
  pure (SourceHash name)

handlersTest :: IO (BumpHandlers, IORef [Expr])
handlersTest = do
  (stateFile, stateFileRef) <- StateFileHandlers.handlersUnitTest
  let
    handlers = handlersNull {
      build = handlersNull.build {
        hackage = handlersNull.build.hackage {fetchHash},
        withBuilder = versionsBuilder buildWithState,
        stateFile
      },
      latestVersion
    }
  pure (handlers, stateFileRef)

managedBoundsConfig :: Either String TargetBounds
managedBoundsConfig =
  eitherDecodeStrict' [exon|{
    "panda": {
      "dep4": ">= 2.0"
      }
  }|]

managedOverridesConfig :: Either String EnvOverrides
managedOverridesConfig =
  eitherDecodeStrict' [exon|{
    "fancy": {
      "dep5": {
          "version": "5.0",
          "hash": "dep5"
        }
      }
  }|]

stateFileTarget :: Text
stateFileTarget =
  [exon|{
  bounds = {
    panda = {
      dep1 = ">=2.0 && <2.3";
      dep2 = ">=1.7 && <1.8";
      dep3 = ">=0";
      dep4 = ">=2.0 && <2.3";
      dep5 = ">=5.0 && <5.1";
    };
  };
  overrides = {
    fancy = {
      dep1 = {
        version = "2.2.0.5";
        hash = "dep1";
      };
      dep2 = {
        version = "1.7.14";
        hash = "dep2";
      };
      dep4 = {
        version = "2.2";
        hash = "dep4";
      };
      dep5 = {
        version = "5.0";
        hash = "dep5";
      };
    };
  };
  lowerInit = {};
  resolving = false;
}
|]

test_bumpMutation :: UnitTest
test_bumpMutation = do
  deps <- leftA fail depsConfig
  managedBounds <- leftA fail managedBoundsConfig
  managedOverrides <- leftA fail managedOverridesConfig
  (handlers, stateFileRef) <- liftIO handlersTest
  let
    env =
      ManagedEnv {
        deps = deps,
        state = ManagedEnvState {
          bounds = managedBounds,
          overrides = managedOverrides,
          lowerInit = mempty,
          resolving = False
        },
        lower = ManagedLowerEnv mempty,
        envs = [("fancy", EnvConfig {targets = ["panda"]})],
        buildOutputsPrefix = Nothing
      }
    conf =
      ManagedConfig {
        ghc = Nothing,
        operation = OpBump,
        stateFile = StateFileConfig {
          file = [relfile|ops/managed.nix|],
          updateProject = True,
          projectRoot = Just testRoot
        },
        envs = ["fancy"],
        targetBound = TargetUpper
      }
  evalEither =<< liftIO do
    runMTest False do
      runManagedApp handlers.build handlers.report env conf (bump handlers)
  files <- liftIO (readIORef stateFileRef)
  eqLines stateFileTarget . renderRootExpr =<< evalMaybe (head files)
