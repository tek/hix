module Hix.Test.Managed.BumpNativeTest where

import Control.Monad.Trans.Reader (ask)
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog (evalEither)
import Path (Abs, Dir, Path, parent, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing, getCurrentDir)

import qualified Hix.Data.Monad
import Hix.Data.Monad (AppResources (AppResources), M (M))
import Hix.Data.Options (ProjectOptions (query), projectOptions)
import Hix.Error (pathText)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem))
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.ManagedPackage (ManagedPackage, managedPackages)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (ProjectContextProto))
import qualified Hix.Managed.Data.ProjectResult
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import qualified Hix.Managed.Data.StateFileConfig
import Hix.Managed.Data.StateFileConfig (StateFileConfig (StateFileConfig))
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.ProjectContext (updateProject)
import qualified Hix.Managed.ProjectContextProto as ProjectContextProto
import Hix.Monad (withProjectRoot)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Managed.Run (addFile)
import Hix.Test.Utils (UnitTest, logConfigDebug, runMTest')

packages :: Packages ManagedPackage
packages =
  managedPackages [
    (("root", "1.0"), ["tasty <1.5", "criterion <1.7"])
  ]

flake :: Path Abs Dir -> Text
flake hixRoot =
  [exon|{
  description = "hix test project";
  inputs.hix.url = "path:#{pathText hixRoot}";
  outputs = {self, hix, ...}: hix.lib.flake ({config, lib, ...}: {
    managed = {
      enable = true;
      latest.compiler = "ghc98";
    };
    compat.enable = false;
    ghcVersions = [];
    packages = {
      root = {
        src = ./.;
        library = {
          enable = true;
          dependencies = ["tasty" "criterion"];
        };
      };
    };
    envs.latest.localPackage = api: api.minimal;
  });
}|]

libMod :: Text
libMod =
  [exon|module Root where
-- import Test.Tasty (blarkh)
string :: String
string = "hello"
|]

-- | This accesses cwd because it needs to access the hix repo.
-- Won't work when run outside of the repo.
setupProject :: M (Path Abs Dir)
setupProject = do
  AppResources {tmp} <- M ask
  cwd <- getCurrentDir
  let projectRoot = tmp </> [reldir|project|]
  let hixRoot = parent (parent cwd)
  createDirIfMissing True projectRoot
  addFile projectRoot [relfile|flake.nix|] (flake hixRoot)
  addFile projectRoot [relfile|lib/Root.hs|] libMod
  pure projectRoot

targetStateFile :: Text
targetStateFile =
  [exon|{
  resolving = false;
}
|]

bumpNativeTest :: M Text
bumpNativeTest = do
  root <- setupProject
  withProjectRoot root do
    let
      stateFileConf = StateFileConfig {
        file = [relfile|ops/managed.nix|]
      }
      envsConfig = [("latest", EnvConfig {targets = ["root"], ghc = GhcDbSystem Nothing})]
      buildConfig = def
    handlersProject <- Project.handlersProd stateFileConf
    handlers <- Build.handlersProd handlersProject envsConfig buildConfig def
    let
      opts = (projectOptions ["latest"]) {query = ["tasty"]}

      proto0 =
        ProjectContextProto {
          packages,
          state = ProjectStateProto [] [] mempty mempty False,
          envs = envsConfig,
          hackage = []
        }

      stateFile = root </> [relfile|ops/managed.nix|]

      run context process = do
        result <- process handlers context
        updateProject handlers.project False result
        stateFileContent <- liftIO (Text.readFile (toFilePath stateFile))
        pure (result.state, stateFileContent)

    context0 <- ProjectContextProto.validate opts proto0
    (_, stateFileContent) <- run context0 bumpOptimizeMain
    pure stateFileContent

test_bumpNative :: UnitTest
test_bumpNative = do
  stateFileContent <- evalEither =<< liftIO do
    runMTest' logConfigDebug bumpNativeTest
  eqLines targetStateFile stateFileContent
