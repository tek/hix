module Hix.Test.Managed.LowerNativeTest where

import Control.Monad.Trans.Reader (ask)
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog (evalEither)
import Path (Abs, Dir, File, Path, Rel, parent, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing, getCurrentDir)

import qualified Hix.Data.Monad
import Hix.Data.Monad (AppResources (AppResources), M (M))
import Hix.Data.Options (projectOptions)
import Hix.Error (pathText)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem))
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto, managedPackages)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectContext
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (ProjectContextProto))
import qualified Hix.Managed.Data.ProjectResult
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (ProjectStateProto))
import qualified Hix.Managed.Data.StateFileConfig
import Hix.Managed.Data.StateFileConfig (StateFileConfig (StateFileConfig))
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Lower
import qualified Hix.Managed.Handlers.Lower.Prod as Lower
import Hix.Managed.Lower.Init (lowerInitMain)
import Hix.Managed.Lower.Optimize (lowerOptimizeMain)
import Hix.Managed.ProjectContext (updateProject)
import qualified Hix.Managed.ProjectContextProto as ProjectContextProto
import Hix.Managed.ProjectContextProto (projectContext)
import Hix.Test.Hedgehog (eqLines)
import Hix.Test.Utils (UnitTest, runMTest)

-- TODO when aeson's lower bound is set to 2.2 here, the build of 2.1.0.0 fails with an infinite recursion in nix when
-- reaching optimize.
-- But when it is set to 2.1, the build succeeds during init.
-- in the former case, there are a few more overrides added from the solver plan.
packages :: Packages ManagedPackageProto
packages =
  managedPackages [
    (("root", "1.0"), ["aeson >=2.2 && <2.3", "extra >=1.6 && <1.8"])
  ]

flake :: Path Abs Dir -> Text
flake hixRoot =
  [exon|{
  description = "hix test project";
  inputs.hix.url = "path:#{pathText hixRoot}";
  outputs = {self, hix, ...}: hix.lib.flake ({config, lib, ...}: {
    managed = {
      enable = true;
      lower.enable = true;
    };
    compat.enable = false;
    ghcVersions = [];
    packages = {
      root = {
        src = ./.;
        library = {
          enable = true;
          dependencies = ["aeson" "extra"];
        };
      };
    };
    envs.lower.localPackage = api: api.minimal;
  });
}|]

libMod :: Text
libMod =
  [exon|module Root where
import Data.Aeson
import Data.List.Extra
string :: String
string = "hello"
|]

addFile :: Path Abs Dir -> Path Rel File -> Text -> M ()
addFile root path content = do
  createDirIfMissing True (parent file)
  liftIO (Text.writeFile (toFilePath file) content)
  where
    file = root </> path

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

targetStateFileInit :: Text
targetStateFileInit =
  [exon|{
  bounds = {
    root = {
      aeson = ">=2.2.0.0 && <2.3";
      extra = ">=1.7.7 && <1.8";
    };
  };
  overrides = {
    lower = {
      aeson = {
        version = "2.2.0.0";
        hash = "1rxbydr7mvchhlyz2111n70db90s1zd9h6miqbbqh2kyc2l0b3pd";
      };
      bifunctors = {
        version = "5.6.1";
        hash = "12k2v7334brn4v7yg3vjd6yv6sh4pzffc3d89qgc2llw7ncv4krw";
      };
      extra = {
        version = "1.7.7";
        hash = "0jgcd8gw6d22ngbi0lp3ak2ghzza59nb3vssrjldwxiim0nzf71v";
      };
      integer-conversion = {
        version = "0.1.0.1";
        hash = "1qy49ig5k8wcqsgjf2rkbv0dy9gpbdzg8yid1kcdn0s7vys59plj";
      };
      semialign = {
        version = "1.3";
        hash = "09147lz8z79ghnpr7z08y9dd0l6c9dz9hz85avfh6z330vl25r95";
      };
      semigroupoids = {
        version = "6.0.0.1";
        hash = "1fijnbfn29iwj567kdbhk67pn11ajm80p9d3hg48hppyx7yzpb2k";
      };
      text-iso8601 = {
        version = "0.1";
        hash = "0zsqjrks9spakiwhbc7xi4dqsx68lb2cd4rvrin3iclyrqg3a6xg";
      };
      th-abstraction = {
        version = "0.5.0.0";
        hash = "0dkilfrvk8zdn3gvyfv5zgjbwqhdf1yg90fk4byka0ib43kgkyvf";
      };
    };
  };
  initial = {
    lower = {
      aeson = "2.2.0.0";
      extra = "1.7.7";
    };
  };
  resolving = false;
}
|]

targetStateFileOptimize :: Text
targetStateFileOptimize =
  [exon|{
  bounds = {
    root = {
      aeson = ">=2.1.0.0 && <2.3";
      extra = ">=1.7.7 && <1.8";
    };
  };
  overrides = {
    lower = {
      OneTuple = {
        version = "0.3.1";
        hash = "0mb81j6zhvzq7h9yyhi9cszrq1g5d4lv3a8wpwimcvzbyg9bdd6p";
      };
      aeson = {
        version = "2.1.0.0";
        hash = "08s162yh716aaxd42k1kyv18p9nsyab42ns4340kvs6r0i8riwsi";
      };
      assoc = {
        version = "1.0.2";
        hash = "1sfc21z18sf8rpsbcr77kgw7qjpm5cm1d24n5ifsm2zid88v8fs9";
      };
      attoparsec = {
        version = "0.14.4";
        hash = "0y9dph5axyvr1bfcvmz6qh50bjcp50m2ljra14960anc6g74a3c8";
      };
      extra = {
        version = "1.7.7";
        hash = "0jgcd8gw6d22ngbi0lp3ak2ghzza59nb3vssrjldwxiim0nzf71v";
      };
      indexed-traversable-instances = {
        version = "0.1.1.2";
        hash = "1mmkklfpagv855p12dqq0r6xwg0v6dc1gj1n3nvzzy4b909ajgd0";
      };
      primitive = {
        version = "0.7.4.0";
        hash = "0n7r8al9wgz4r7jzizapn1dbnkqxwx2c4lqkgfm5q5bxj8fl7g1c";
      };
      scientific = {
        version = "0.3.7.0";
        hash = "09iwj0snmx7vj7x03l4vdcn76zylcgxd9pyz0yxkydgfnn3lvc08";
      };
      semialign = {
        version = "1.2.0.1";
        hash = "0mpw54c3s0x70k5l52a57yhnmbgrksb3dn0vjq4m37spyzsfl1v2";
      };
      strict = {
        version = "0.4.0.1";
        hash = "0xhr98m2632k2pic8q9bpnm3mp9098mmg4s66ds052b92494k49f";
      };
      these = {
        version = "1.1.1.1";
        hash = "1i1nfh41vflvqxi8w8n2s35ymx2z9119dg5zmd2r23ya7vwvaka1";
      };
      vector = {
        version = "0.13.1.0";
        hash = "0c1nw2sx14y29afdbwl40sk9vznx71rja5jcg14b8986778kl32d";
      };
      witherable = {
        version = "0.4.2";
        hash = "1ga4al351kwcfvsdr1ngyzj4aypvl46w357jflmgxacad8iqx4ik";
      };
    };
  };
  initial = {
    lower = {
      aeson = "2.2.0.0";
      extra = "1.7.7";
    };
  };
  resolving = false;
}
|]

lowerNativeTest :: M (Text, Text)
lowerNativeTest = do
  root <- setupProject
  let
    stateFileConf = StateFileConfig {
      file = [relfile|ops/managed.nix|],
      projectRoot = Just root
    }
    envsConfig = [("lower", EnvConfig {targets = ["root"], ghc = GhcDbSystem Nothing})]
  build <- Build.handlersProd stateFileConf envsConfig Nothing False
  handlersInit <- Lower.handlersProdWith build
  handlersOptimize <- Lower.handlersProdWith build
  let
    opts = projectOptions ["lower"]

    proto0 =
      ProjectContextProto {
        packages,
        state = ProjectStateProto mempty mempty mempty mempty False,
        envs = envsConfig,
        buildOutputsPrefix = Nothing
      }

    stateFile = root </> [relfile|ops/managed.nix|]

    run context handlers process = do
      result <- process handlers context
      updateProject handlersInit.build context.build result
      stateFileContent <- liftIO (Text.readFile (toFilePath stateFile))
      pure (result.state, stateFileContent)

  context0 <- ProjectContextProto.validate opts proto0
  (state1, stateFileContentInit) <- run context0 handlersInit (lowerInitMain def)
  -- TODO the packages here aren't updated with the result from the first run
  let context1 = projectContext def state1 context0.packages context0.envs
  (_, stateFileContentOptimize) <- run context1 handlersOptimize lowerOptimizeMain
  pure (stateFileContentInit, stateFileContentOptimize)

test_lowerNative :: UnitTest
test_lowerNative = do
  (stateFileContentInit, stateFileContentOptimize) <- evalEither =<< liftIO do
    runMTest True lowerNativeTest
  eqLines targetStateFileInit stateFileContentInit
  eqLines targetStateFileOptimize stateFileContentOptimize
