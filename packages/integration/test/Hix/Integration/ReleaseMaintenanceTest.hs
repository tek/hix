module Hix.Integration.ReleaseMaintenanceTest where

import Control.Lens (_3)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Data.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime), fromGregorian, timeOfDayToTime)
import Distribution.Compat.Lens ((.~))
import Distribution.Version (mkVersion)
import Exon (exon)
import Hedgehog ((===))
import Path (Abs, Dir, Path, reldir, relfile, (</>))

import Hix.Class.Map (nGen, nMap)
import Hix.Data.Dep (Dep (..))
import Hix.Data.Options (ManagedOptions (..), projectOptions)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage, PackageName (..))
import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (unsafeVersionBoundsFromRange)
import Hix.Error (pathText)
import Hix.Http (httpManager)
import Hix.Integration.Hackage (withHackage)
import Hix.Integration.Utils (UnitTest, add, addP, libHs, local1, runMTest, withHixDir)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem))
import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation (..), HackageTls (TlsOff))
import Hix.Managed.Cabal.Data.HackageRepo (HackageRepo (..))
import Hix.Managed.Cabal.Data.Revision (Revision (..))
import qualified Hix.Managed.Cabal.Init as Cabal
import Hix.Managed.Cabal.Init (remoteRepo)
import Hix.Managed.Cabal.Resources (cabalVerbosity)
import Hix.Managed.Cabal.Sdist (sourceDistribution)
import Hix.Managed.Cabal.Upload (UploadConfig (..), publishPackage, revisionCabalFile)
import Hix.Managed.Data.BuildOutput (BuildOutput (..), DepChanges (..), DepChangesNames (..), ModifiedId (..))
import Hix.Managed.Data.EnvConfig (EnvConfig (..))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.MaintContext (MaintContext (..), MaintPackage (..))
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..), ProjectPackages)
import Hix.Managed.Data.Mutable (unsafeMutableDep)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (..))
import Hix.Managed.Flake (runFlakeGen, runFlakeLock)
import qualified Hix.Managed.Git as Git
import Hix.Managed.Git (GitNative, runGitNativeHermetic)
import qualified Hix.Managed.Handlers.Context as ContextHandlers
import Hix.Managed.Handlers.Context (ContextKey (..), ContextQuery (ContextQuery))
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.Maint (MaintHandlers (..))
import Hix.Managed.Handlers.Maint.Prod (runBumpProd)
import Hix.Managed.Maint.Data.MaintResult (MaintChanged (..), MaintResult (..))
import Hix.Managed.Maint.Git (gitApiMaintHermetic)
import Hix.Managed.ReleaseMaintenance (releaseMaintenance)
import Hix.Monad (M, appContextDebug, fatalError, withTempRoot)
import Hix.Network (Port (..))
import Hix.Test.Hedgehog (eqLines)

initialPackage :: PackageId
initialPackage = PackageId {name = PackageName "local1", version = mkVersion [0, 2, 0]}

initialDeps :: [Dep]
initialDeps =
  [
    Dep {package = "base", version = [[0], [5]]},
    Dep {package = "semigroups", version = [[0, 19], [0, 20]]},
    Dep {package = "extra", version = [[1, 7], [1, 8]]}
  ]

flake :: Text -> Text
flake path =
  [exon|{
  description = "hix test project";
  inputs.hix.url = "#{path}";
  outputs = {hix, ...}: hix.lib.flake {
    compiler = "ghc98";
    managed = {
      enable = true;
      latest.compiler = "ghc98";
    };
    compat.enable = false;
    ghcVersions = [];
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
        cabal.version = "0.2.0";
        library = {
          enable = true;
          dependencies = ["base" "semigroups" "extra"];
        };
      };
    };
    envs.latest.localPackage = api: api.minimal;
  };
}
|]

initialStateFile :: Text
initialStateFile =
  [exon|{
  bounds = {
    local1 = {
      base = {
        lower = null;
        upper = "5";
      };
      semigroups = {
        lower = "0.19";
        upper = "0.20";
      };
      extra = {
        lower = "1.7";
        upper = "1.8";
      };
    };
  };
  versions = {
    latest = {
      base = "4.19.0.0";
      semigroups = "0.19.2";
      extra = "1.7.16";
    };
  };
  overrides = {
    latest = {};
  };
  initial = {
    latest = {
      base = "4.19.0.0";
      semigroups = "0.19.2";
      extra = "1.7.16";
    };
  };
  resolving = false;
}
|]

setupProject :: Text -> GitNative -> M (Path Abs Dir)
setupProject hixRoot git = do
  git.cmd_ ["init"]
  add git [relfile|flake.nix|] (flake hixRoot)
  add git [relfile|ops/managed.nix|] initialStateFile
  git.cmd_ ["commit", "-m", "1"]
  git.cmd_ ["tag", "--no-sign", "0.1.0"]
  addP git [relfile|lib/Lib.hs|] libHs
  runFlakeLock git.repo
  git.cmd_ ["add", "flake.lock"]
  runFlakeGen git.repo
  git.cmd_ ["add", pathText (local1 </> [relfile|local1.cabal|])]
  git.cmd_ ["commit", "-m", "2"]
  git.cmd_ ["tag", "--no-sign", "0.2.0"]
  pure (git.repo </> local1)

packages :: ProjectPackages
packages =
  [("local1", ManagedPackage {name = "local1", version = mkVersion [0, 2, 0], deps = initialDeps})]

envConfigs :: Envs EnvConfig
envConfigs = [("latest", EnvConfig {targets = ["local1"], ghc = Just (GhcDbSystem Nothing)})]

envTargets :: Envs [LocalPackage]
envTargets = [("latest", ["local1"])]

maintContext :: MaintContext
maintContext =
  MaintContext {
    packages = [
      ("local1", MaintPackage {
        package = ManagedPackage {
          name = "local1",
          version = [0, 2, 0],
          deps = initialDeps
        },
        path = [reldir|packages/local1|]
      })
    ],
    hackage = [],
    envs = envTargets
  }

bumpContext :: ProjectContextProto
bumpContext =
  ProjectContextProto {
    packages,
    state = def {bounds = [("local1", nGen initialDeps \ dep -> (dep.package, unsafeVersionBoundsFromRange dep.version))]},
    envs = envConfigs,
    hackage = []
  }

queryTestContext :: ContextQuery a -> Maybe a
queryTestContext = \case
  ContextQuery ContextMaint -> Just maintContext
  ContextQuery ContextManaged -> Just bumpContext

versions :: PackageName -> M [Version]
versions = \case
  "base" -> pure [[4, 19, 0, 0], [4, 20, 0, 0]]
  "extra" -> pure [[1, 7, 16], [1, 8]]
  "semigroups" -> pure [[0, 19, 2], [0, 20]]
  name -> fatalError [exon|Unexpected query for versions of ##{name}|]

options :: ManagedOptions
options =
  ManagedOptions {
    context = Right Nothing,
    project = (projectOptions ["latest"]),
    stateFile = def,
    handlers = Nothing
  }

maintConfig :: MaintConfig
maintConfig =
  MaintConfig {
    noFailures = True,
    commit = True,
    revision = True,
    targets = Nothing,
    push = False,
    fetch = False,
    globalGit = False,
    pr = False
  }

maintHandlers :: Port -> M MaintHandlers
maintHandlers port = do
  manager <- httpManager
  hackageUpload <- HackageClient.handlersMock manager port
  pure MaintHandlers {
    runBump = runBumpProd context options,
    git = gitApiMaintHermetic maintConfig,
    context,
    publishHackages = [hackageUpload]
  }
  where
    context = ContextHandlers.handlersTest (pure . queryTestContext)

dummyTime :: UTCTime
dummyTime = UTCTime (fromGregorian 2024 1 16) (timeOfDayToTime (TimeOfDay 0 0 0))

targetBuildOutput :: BuildOutput
targetBuildOutput =
  BuildOutput {
    changes = DepChanges {
      modified = [
        ModifiedId {package = unsafeMutableDep "base", version = mkVersion [4, 19, 1, 0], range = mkBounds [[0], [4, 20]]},
        ModifiedId {package = unsafeMutableDep "extra", version = mkVersion [1, 8], range = mkBounds [[1, 7], [1, 9]]},
        ModifiedId {package = unsafeMutableDep "semigroups", version = mkVersion [0, 20], range = mkBounds [[0, 19], [0, 21]]}
      ],
      unmodified = [],
      failed = []
    },
    names = DepChangesNames {
      modifiedNames = Just "base, extra, semigroups",
      unmodifiedNames = Nothing,
      failedNames = Nothing
    }
  }
  where
    mkBounds = Just . unsafeVersionBoundsFromRange

fixTime :: MaintResult -> MaintResult
fixTime = #_Changed . _3 . #_Published . #time .~ dummyTime

testRepo :: Port -> HackageRepo
testRepo port =
  HackageRepo {
    name = "test",
    description = "test",
    location = HackageLocation {
      host = "localhost",
      port = Just (fromIntegral port.value),
      auth = Just ("test", "test"),
      tls = TlsOff
    },
    enable = True,
    secure = Nothing,
    indexState = Nothing,
    solver = False,
    publish = True,
    keys = Nothing
  }

targetResults :: Packages MaintResult
targetResults =
  [
    ("local1", Changed branch branch (Published revision1))
  ]
  where
    branch = "release/local1/0.2.0"

    revision1 =
      Revision {
        user = "test",
        time = dummyTime,
        sha256 = "9332f118cfcfb8eced7d76a338d4da50ca319bea4416fc733dd4167948ea6bf4",
        number = 1
      }

-- TODO for some reason the base bound changed from 4.20 to 5, even though the bump output still shows the precise
-- bound.
targetCabal :: Text
targetCabal =
  [exon|cabal-version: 1.12
name:          local1
version:       0.2.0
license:       MIT
maintainer:    hix
synopsis:      Synopsis
description:
    See https://hackage.haskell.org/package/local1/docs/Local1.html

category:      Test
x-revision:    1
build-type:    Simple

library
    exposed-modules:  Lib
    hs-source-dirs:   lib
    other-modules:    Paths_local1
    default-language: GHC2021
    build-depends:
        base <5,
        extra >=1.7 && <1.9,
        semigroups >=0.19 && <0.21
|]

targetCommit :: Text
targetCommit =
  [exon|
    Bump base, extra, semigroups
    Maintenance for 'local1'
    New versions:
    * base-4.19.2.0 [<4.20]
    * extra-1.8 [>=1.7 && <1.9]
    * semigroups-0.20 [>=0.19 && <0.21]

diff --git a/ops/managed.nix b/ops/managed.nix
index a4cb674..0e77542 100644
--- a/ops/managed.nix
+++ b/ops/managed.nix
@@ -3,34 +3,39 @@
     local1 = {
       base = {
         lower = null;
-        upper = "5";
-      };
-      semigroups = {
-        lower = "0.19";
-        upper = "0.20";
+        upper = "4.20";
       };
       extra = {
         lower = "1.7";
-        upper = "1.8";
+        upper = "1.9";
+      };
+      semigroups = {
+        lower = "0.19";
+        upper = "0.21";
       };
     };
   };
   versions = {
     latest = {
-      base = "4.19.0.0";
-      semigroups = "0.19.2";
-      extra = "1.7.16";
+      base = "4.19.2.0";
+      extra = "1.8";
+      semigroups = "0.20";
     };
   };
-  overrides = {
+  initial = {
     latest = {};
   };
-  initial = {
+  overrides = {
     latest = {
-      base = "4.19.0.0";
-      semigroups = "0.19.2";
-      extra = "1.7.16";
+      extra = {
+        version = "1.8";
+        hash = "0cnk9ncn0k7fv24g0v3rhqd3z9zcz9cgz0rf59vs6v9kappbidmx";
+        repo = "hackage.haskell.org";
+      };
     };
   };
+  solver = {
+    latest = {};
+  };
   resolving = false;
 }
diff --git a/packages/local1/local1.cabal b/packages/local1/local1.cabal
index bbfba8c..76b20d4 100644
--- a/packages/local1/local1.cabal
+++ b/packages/local1/local1.cabal
@@ -22,6 +22,6 @@ library
       lib
   build-depends:
       base <5
-    , extra ==1.7.*
-    , semigroups ==0.19.*
+    , extra >=1.7 && <1.9
+    , semigroups >=0.19 && <0.21
   default-language: GHC2021
|]

test_releaseMaintenance :: UnitTest
test_releaseMaintenance =
  withHixDir \ hixRoot -> do
    (maintResults, revisedCabalContents, commitText) <- runMTest False do
      withTempRoot "test-maint" \ root ->
        withHackage \ port -> do
          handlers <- maintHandlers port
          packageDir <- runGitNativeHermetic root "test: project setup" (setupProject (toText hixRoot))
          let localHackage = remoteRepo (testRepo port)
          appContextDebug "publishing test package" do
            flags <- Cabal.solveFlags [localHackage] def
            targz <- sourceDistribution packageDir initialPackage
            verbosity <- cabalVerbosity
            publishPackage UploadConfig {verbosity, user = "test", password = "test"} flags targz
          maintResults <- releaseMaintenance handlers maintConfig maintContext
          revisedCabalContents <- revisionCabalFile (NonEmpty.head handlers.publishHackages) initialPackage 1
          commitText <- runGitNativeHermetic root "test: get commit message" \ git ->
            git.cmd ["show", "release/local1/0.2.0"]
          pure (maintResults, revisedCabalContents, commitText)
    targetResults === nMap fixTime maintResults
    targetCabal === revisedCabalContents
    eqLines targetCommit (Text.unlines (filter ("    " /=) (drop 3 commitText)))
