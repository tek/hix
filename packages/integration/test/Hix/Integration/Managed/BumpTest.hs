module Hix.Integration.Managed.BumpTest where

import Control.Monad.Trans.Reader (ask)
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing)

import Hix.Data.Dep (Dep (..))
import qualified Hix.Data.Monad
import Hix.Data.Monad (AppResources (AppResources), M (M))
import Hix.Data.Options (projectOptions)
import Hix.Integration.Utils (UnitTest, addFile, eqLines, runMTest, withHixDir)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo (..), contextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (centralName)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ProjectPackages, managedPackages)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (ProjectContextProto))
import qualified Hix.Managed.Data.ProjectResult
import Hix.Managed.Flake (runFlakeGenCabal)
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.ProjectContext (updateProject)
import qualified Hix.Managed.ProjectContextProto as ProjectContextProto
import Hix.Monad (withProjectRoot)
import Hix.CabalParsec (unsafeParsec)

packages :: ProjectPackages
packages =
  managedPackages [
    (("local1", [1, 0]), [Dep {package = "extra", version = []}]),
    (("local2", [1, 0]), [Dep {package = "extra", version = []}, Dep {package = "local1", version = []}])
  ]

flake :: Text -> Text
flake path =
  [exon|{
  description = "hix test project";
  inputs.hix.url = "#{path}";
  inputs.hix.inputs.nixpkgs.url = "github:nixos/nixpkgs/b2243f41e860ac85c0b446eadc6930359b294e79";
  outputs = {hix, ...}: hix.lib.flake {
    managed = {
      enable = true;
      latest.compiler = "ghc910";
      sets = "each";
    };
    cabal = {
      license = "MIT";
      meta = {
        category = "Test";
        maintainer = "hix";
        synopsis = "Synopsis";
      };
    };
    packages = {
      local1 = {
        src = ./packages/local1;
        library = {
          enable = true;
          dependencies = ["extra"];
        };
      };
      local2 = {
        src = ./packages/local2;
        library = {
          enable = true;
          dependencies = ["extra" "local1"];
        };
      };
    };
    envs.latest.localPackage = api: api.minimal;
  };
}
|]

libModLocal1 :: Text
libModLocal1 =
  [exon|module Local1 where
local1 :: String
local1 = "local1"
|]

libModLocal2 :: Text
libModLocal2 =
  [exon|module Local2 where
local2 :: String
local2 = "local2"
|]

setupProject :: Text -> M (Path Abs Dir)
setupProject hixRoot = do
  AppResources {tmp} <- M ask
  let projectRoot = tmp </> [reldir|project|]
  createDirIfMissing True projectRoot
  addFile projectRoot [relfile|flake.nix|] (flake hixRoot)
  addFile projectRoot [relfile|packages/local1/lib/Local1.hs|] libModLocal1
  addFile projectRoot [relfile|packages/local2/lib/Local2.hs|] libModLocal2
  runFlakeGenCabal projectRoot
  pure projectRoot

targetStateFile :: Text
targetStateFile =
  [exon|{
  bounds = {
    local1 = {
      extra = {
        lower = null;
        upper = "1.9";
      };
    };
    local2 = {
      extra = {
        lower = null;
        upper = "1.9";
      };
      local1 = {
        lower = null;
        upper = "1.1";
      };
    };
  };
  versions = {
    latest-local1 = {
      extra = "1.8";
    };
    latest-local2 = {
      extra = "1.8";
      local1 = "1.0";
    };
  };
  initial = {
    latest-local1 = {};
    latest-local2 = {};
  };
  overrides = {
    latest-local1 = {
      extra = {
        version = "1.8";
        hash = "0cnk9ncn0k7fv24g0v3rhqd3z9zcz9cgz0rf59vs6v9kappbidmx";
        repo = "hackage.haskell.org";
      };
    };
    latest-local2 = {
      extra = {
        version = "1.8";
        hash = "0cnk9ncn0k7fv24g0v3rhqd3z9zcz9cgz0rf59vs6v9kappbidmx";
        repo = "hackage.haskell.org";
      };
      local1 = {
        local = true;
      };
    };
  };
  solver = {
    latest-local1 = {};
    latest-local2 = {};
  };
  resolving = false;
}
|]

envsConfig :: Envs EnvConfig
envsConfig =
  [
    ("latest-local1", EnvConfig {targets = ["local1"], ghc = Nothing}),
    ("latest-local2", EnvConfig {targets = ["local2"], ghc = Nothing})
  ]

bumpNativeTest :: Text -> M Text
bumpNativeTest hixRoot = do
  root <- setupProject hixRoot
  withProjectRoot root do
    handlersProject <- Project.handlersProd def
    handlers <- Build.handlersProd handlersProject def def
    let
      stateFile = root </> [relfile|ops/managed.nix|]

      run context process = do
        result <- process handlers context
        updateProject handlers.project False result
        stateFileContent <- liftIO (Text.readFile (toFilePath stateFile))
        pure (result.state, stateFileContent)

    context0 <- ProjectContextProto.validate opts proto0
    (_, stateFileContent) <- run context0 bumpOptimizeMain
    pure stateFileContent
  where
    opts = projectOptions ["latest-local1", "latest-local2"]

    proto0 =
      ProjectContextProto {
        packages,
        state = def,
        envs = envsConfig,
        hackage = [
          (centralName, (contextHackageRepo centralName) {
            indexState = Just (unsafeParsec ("2024-01-01T00:00:00Z" :: String))
          })
        ]
      }

-- | Goals for these deps:
--
-- - @local2@ is in a different env, so its dependency on @local1@ will be treated as mutable despite being a local
--   package.
--   However, since there is no available version on Hackage for @local1@, an entry will be inserted into @sourcePkgDb@,
--   and when fetching the hash for the package fails it will get a special override (@Local@) that uses @source.root@
--   with the package's local source dir.
test_bump :: UnitTest
test_bump =
  withHixDir \ hixRoot -> do
    stateFileContent <- runMTest False (bumpNativeTest hixRoot)
    eqLines targetStateFile stateFileContent
