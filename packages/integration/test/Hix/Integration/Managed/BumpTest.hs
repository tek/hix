module Hix.Integration.Managed.BumpTest where

import Control.Monad.Trans.Reader (ask)
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing)

import Hix.CabalParsec (unsafeParsec)
import Hix.Data.Dep (Dep (..), unsafeDep)
import qualified Hix.Data.Monad
import Hix.Data.Monad (AppResources (AppResources), M (M))
import Hix.Data.Options (projectOptions)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (PackageName)
import Hix.Data.VersionBounds (Bound (..))
import Hix.Integration.Hackage (HackageId (..), TestHackage (..), withHackageIds)
import Hix.Integration.Utils (UnitTest, addFile, runMTestLog, withHixDir)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import Hix.Managed.Cabal.Data.Config (CabalConfig (..))
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo (..), contextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (centralName)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ProjectPackages, managedPackages)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (ProjectContextProto))
import Hix.Managed.Flake (runFlakeGenCabal)
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.ProjectContext (updateProject, withProjectContext)
import Hix.Monad (withProjectRoot)
import Hix.NixCode (NixCode (..), nixCode)
import Hix.Test.Hedgehog (eqLines)

packages :: ProjectPackages
packages =
  managedPackages [
    (("local1", [1, 0]), [Dep {package = "extra", version = []}]),
    (("local2", [1, 0]), [
      Dep {package = "dep1", version = []},
      Dep {package = "dep2", version = []},
      Dep {package = "local1", version = []}
    ]),
    (("local3", [1, 0]), [
      Dep {package = "extra", version = []},
      Dep {package = "local2", version = []}
    ])
  ]

flake :: ContextHackageRepo -> Text -> NixCode
flake repo path =
  [exon|{
  description = "hix test project";
  inputs.hix.url = "#{NixCode path}";
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
          dependencies = ["dep1" "dep2" "local1"];
        };
      };
      local3 = {
        src = ./packages/local3;
        library = {
          enable = true;
          dependencies = ["extra" "local2"];
        };
      };
    };
    envs.latest.localPackage = api: api.minimal;
    hackage.repos.test = ##{repo};
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

libModLocal3 :: Text
libModLocal3 =
  [exon|module Local3 where
local3 :: String
local3 = "local3"
|]

setupProject :: ContextHackageRepo -> Text -> M (Path Abs Dir)
setupProject repo hixRoot = do
  AppResources {tmp} <- M ask
  let projectRoot = tmp </> [reldir|project|]
  createDirIfMissing True projectRoot
  addFile projectRoot [relfile|flake.nix|] (nixCode (flake repo hixRoot))
  addFile projectRoot [relfile|packages/local1/lib/Local1.hs|] libModLocal1
  addFile projectRoot [relfile|packages/local2/lib/Local2.hs|] libModLocal2
  addFile projectRoot [relfile|packages/local3/lib/Local3.hs|] libModLocal3
  runFlakeGenCabal projectRoot
  pure projectRoot

envsConfig :: Envs EnvConfig
envsConfig =
  [
    ("latest-local1", EnvConfig {targets = ["local1"], ghc = Nothing, managedBound = Just BoundUpper}),
    ("latest-local2", EnvConfig {targets = ["local2"], ghc = Nothing, managedBound = Just BoundUpper}),
    ("latest-local3", EnvConfig {targets = ["local3"], ghc = Nothing, managedBound = Just BoundUpper})
  ]

bumpNativeTest :: TestHackage -> Text -> M Text
bumpNativeTest hackage hixRoot = do
  root <- setupProject hackage.context hixRoot
  withProjectRoot root do
    handlersProject <- Project.handlersProd def
    handlers <- Build.handlersFixed handlersProject def def {hackageExtra = [hackage.repo]}
    result <- withProjectContext BoundUpper handlersProject opts proto \ context -> do
      bumpOptimizeMain handlers context
    updateProject handlers.project False result
    liftIO (Text.readFile (toFilePath (root </> [relfile|ops/managed.nix|])))
  where
    opts = projectOptions ["latest-local1", "latest-local2", "latest-local3"]

    proto =
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

hackageId :: PackageName -> [Dep] -> HackageId
hackageId name deps =
  HackageId {package = PackageId {name, version = [1]}, modules = [], deps}

hackageIds :: [HackageId]
hackageIds =
  [
    hackageId "dep1" [],
    hackageId "dep2" [unsafeDep "dep1", unsafeDep "dep3"],
    hackageId "dep3" []
  ]

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
      dep1 = {
        lower = null;
        upper = "1.1";
      };
      dep2 = {
        lower = null;
        upper = "1.1";
      };
      local1 = {
        lower = null;
        upper = "1.1";
      };
    };
    local3 = {
      extra = {
        lower = null;
        upper = "1.9";
      };
      local2 = {
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
      dep1 = "1";
      dep2 = "1";
      local1 = "1.0";
    };
    latest-local3 = {
      extra = "1.8";
      local2 = "1.0";
    };
  };
  initial = {
    latest-local1 = {};
    latest-local2 = {};
    latest-local3 = {};
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
      clock = {
        version = "0.8.4";
        hash = "14gy1a16l5s70pyqlsmylxsiiagas2yflqmjjmrdbzj4g1zxy39r";
        repo = "hackage.haskell.org";
      };
      dep1 = {
        version = "1";
        hash = "1lp6lcf4qqlr9v0s293bxp18ji93l75b52d80h8s76w2zpna9swh";
        repo = "test";
      };
      dep2 = {
        version = "1";
        hash = "1ghvihzgflbqwvx9l607vldxf785hcd6banm1zk4cipvi6wfjz8x";
        repo = "test";
      };
      dep3 = {
        version = "1";
        hash = "1lrmbhqzbqggk3zvszj4sdq4m22jbhpc170ar9n6ja4jrglk9sjz";
        repo = "test";
      };
      extra = {
        version = "1.8";
        hash = "0cnk9ncn0k7fv24g0v3rhqd3z9zcz9cgz0rf59vs6v9kappbidmx";
        repo = "hackage.haskell.org";
      };
      local1 = {
        local = true;
      };
    };
    latest-local3 = {
      dep1 = {
        version = "1";
        hash = "1lp6lcf4qqlr9v0s293bxp18ji93l75b52d80h8s76w2zpna9swh";
        repo = "test";
      };
      dep2 = {
        version = "1";
        hash = "1ghvihzgflbqwvx9l607vldxf785hcd6banm1zk4cipvi6wfjz8x";
        repo = "test";
      };
      dep3 = {
        version = "1";
        hash = "1lrmbhqzbqggk3zvszj4sdq4m22jbhpc170ar9n6ja4jrglk9sjz";
        repo = "test";
      };
      extra = {
        version = "1.8";
        hash = "0cnk9ncn0k7fv24g0v3rhqd3z9zcz9cgz0rf59vs6v9kappbidmx";
        repo = "hackage.haskell.org";
      };
      local1 = {
        local = true;
      };
      local2 = {
        local = true;
      };
    };
  };
  solver = {
    latest-local1 = {};
    latest-local2 = {
      dep1 = {
        version = "1";
        hash = "1lp6lcf4qqlr9v0s293bxp18ji93l75b52d80h8s76w2zpna9swh";
        repo = "test";
      };
      dep2 = {
        version = "1";
        hash = "1ghvihzgflbqwvx9l607vldxf785hcd6banm1zk4cipvi6wfjz8x";
        repo = "test";
      };
      dep3 = {
        version = "1";
        hash = "1lrmbhqzbqggk3zvszj4sdq4m22jbhpc170ar9n6ja4jrglk9sjz";
        repo = "test";
      };
    };
    latest-local3 = {};
  };
  resolving = false;
}
|]

-- | Goals for these deps:
--
-- - @local2@ is in a different env, so its dependency on @local1@ will be treated as mutable despite being a local
--   package.
--   However, since there is no available version on Hackage for @local1@, an entry will be inserted into @sourcePkgDb@,
--   and when fetching the hash for the package fails it will get a special override (@Local@) that uses @source.root@
--   with the package's local source dir.
--   It also depends on the package @dep1@, which is only available on the custom test Hackage, and not in the nixpkgs
--   GHC set.
--   This causes an "unknown dep" error during the solver packages build, resulting in the addition of a solver
--   override.
--   The error occurs, more specifically, because Nix is instantiating the derivation of @local@ to access its
--   dependencies to populate the solver set; not because @local2@ is actually part of the set when it is built.
--   @dep1@ is known to Cabal and the source hash handler by its presence on the local Hackage.
--   Same situation for @dep2@, which is there to ensure that this works with multiple unknown deps; also it has a dep
--   on @dep1@ with mismatching bounds that are jailbroken.
--
-- - @local3@ is also in a different env, and it depends on @local2@ directly, and therefore transitively on the
--   packages that cause difficulties with @local2@.
--   Its dependency on @extra@ serves only the purpose of forcing a mutation to happen, since those aren't triggered for
--   the local deps, who don't have any mutation candidates.
--
--   TODO investigate why dep1 isn't part of the solver set for local3. It seems to me that @packageDbSolver@ should
--   include it in the propagatedBuildInputs; or rather fail because when evaluating local2 to access its deps it can't
--   find dep1.
test_bump :: UnitTest
test_bump =
  withHixDir \ hixRoot -> do
    stateFileContent <- runMTestLog def do
      withHackageIds hackageIds \ hackage ->
        bumpNativeTest hackage hixRoot
    eqLines targetStateFile stateFileContent
