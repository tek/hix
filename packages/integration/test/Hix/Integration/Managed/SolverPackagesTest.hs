module Hix.Integration.Managed.SolverPackagesTest where

import Control.Monad.Trans.State.Strict (runStateT)
import Exon (exon)
import Hedgehog ((===))
import Path (relfile)

import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Data.Overrides (Override (..), Overrides)
import Hix.Data.Version (SourceHash (..))
import Hix.Integration.Utils (UnitTest, addFile, libHs, runMTest, withHixDir)
import Hix.Managed.Build.Adapt (buildAdaptive)
import Hix.Managed.Build.Target (BuilderResources (..), buildSolverPackages, suggestJailbreakAndLatestVersion)
import Hix.Managed.Data.StageState (BuildResult (..))
import Hix.Managed.Flake (runFlakeLock)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Monad (withTempRoot)

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
          dependencies = ["base" "semigroups" "extra" "exon" "polysemy-test" "polysemy-conc"];
        };
      };
    };
    envs.latest.localPackage = api: api.minimal;
  };
}
|]

setupProject :: Text -> M ()
setupProject hixRoot = do
  root <- appRes.root
  addFile root [relfile|flake.nix|] (flake hixRoot)
  addFile root [relfile|packages/local1/lib/Lib.hs|] libHs
  runFlakeLock root

targetResult :: BuildResult
targetResult =
  BuildSuccess ["/nix/store/ka5p0aywm9b8rr3n527czi466dghrfka-ghc-9.10.1-with-packages"]

targetOverrides :: Overrides
targetOverrides =
  [
    ("exon", Override {
      version = [1, 7, 2, 0],
      hash = SourceHash "0hg271cvjqm4ps75qpnirq9nvjwpwb03mcbn1a364jrysrj6bg3b",
      repo = Just "hackage.haskell.org"
    }),
    ("incipit-base", Jailbreak),
    ("incipit-core", Jailbreak),
    ("polysemy-conc", Jailbreak),
    ("polysemy-resume", Jailbreak),
    ("polysemy-test", Jailbreak),
    ("polysemy-time", Jailbreak)
  ]

test_solverPackages :: UnitTest
test_solverPackages =
  withHixDir \ hixRoot ->  do
    (result, overrides) <- runMTest False $ withTempRoot "test-maint" \ root -> do
      setupProject hixRoot
      handlersProject <- Project.handlersProd def
      (hackage, versions) <- Build.handlersFixedResources def
      let resources = BuilderResources {hackage, stateFile = handlersProject.stateFile, root, buildConfig = def, ..}
          build = buildAdaptive (buildSolverPackages resources "latest") (suggestJailbreakAndLatestVersion resources)
      (result, (overrides, _)) <- runStateT build (mempty, mempty)
      pure (result, overrides)
    targetResult === result
    targetOverrides === overrides
