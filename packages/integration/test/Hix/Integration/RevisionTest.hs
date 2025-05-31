module Hix.Integration.RevisionTest where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime), fromGregorian, timeOfDayToTime)
import Distribution.Version (mkVersion)
import Exon (exon)
import Hedgehog ((===))
import Path (Abs, Dir, Path, reldir, relfile, (</>))

import Hix.Class.Map (nGen)
import Hix.Data.Dep (Dep (..))
import Hix.Data.Options (ManagedOptions (..), projectOptions)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (PackageName (..))
import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (Bound (BoundUpper), unsafeVersionBoundsFromRange)
import Hix.Error (pathText)
import Hix.Integration.Hackage (TestHackage (..), withHackageClient)
import Hix.Integration.Utils (UnitTest, add, addP, libHs, local1, runMTest, withHixDir)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem))
import Hix.Managed.Cabal.Data.HackageLocation (HackageAuth (..), HackageLocation (..), HackageTls (TlsOff))
import Hix.Managed.Cabal.Data.HackageRepo (HackageRepo (..))
import Hix.Managed.Cabal.Data.Revision (Revision (..))
import qualified Hix.Managed.Cabal.Init as Cabal
import Hix.Managed.Cabal.Init (remoteRepo)
import Hix.Managed.Cabal.Resources (cabalVerbosity)
import Hix.Managed.Cabal.Sdist (sourceDistribution)
import Hix.Managed.Cabal.Upload (UploadConfig (..), publishPackage, revisionCabalFile)
import Hix.Managed.Data.EnvConfig (EnvConfig (..))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.MaintContext (MaintContextProto (..), MaintEnv (..), MaintPackage (..))
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..), ProjectPackages)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto (..))
import Hix.Managed.Data.RevisionConfig (RevisionConfig)
import Hix.Managed.Flake (runFlakeGen, runFlakeLock)
import qualified Hix.Managed.Git as Git
import Hix.Managed.Git (GitNative, runGitNativeHermetic)
import Hix.Managed.Handlers.Context (ContextKey (..), ContextQuery (ContextQuery))
import Hix.Managed.Handlers.HackageClient (HackageClient)
import Hix.Managed.Handlers.Revision (RevisionHandlers (..))
import Hix.Managed.Maint.Data.MaintResult (MaintChanged (..), MaintResult (..))
import Hix.Managed.Maint.Git (gitApiRevisionHermetic)
import Hix.Managed.ReleaseMaintenance (publishRevisions)
import Hix.Monad (M, appContextDebug, fatalError, withTempRoot)
import Hix.Network (Port (..))

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

bumpedStateFile :: Text
bumpedStateFile =
  [exon|{
  bounds = {
    local1 = {
      base = {
        lower = null;
        upper = "5";
      };
      semigroups = {
        lower = "0.19";
        upper = "0.21";
      };
      extra = {
        lower = "1.7";
        upper = "1.9";
      };
    };
  };
  versions = {
    latest = {
      base = "4.19.0.0";
      semigroups = "0.20.0";
      extra = "1.8.1";
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

bumpDeps :: GitNative -> M ()
bumpDeps git = do
  git.cmd_ ["switch", "--create", "release/local1/0.2.0"]
  git.cmd_ ["switch", "master"]
  add git [relfile|ops/managed.nix|] bumpedStateFile
  runFlakeGen git.repo
  git.cmd_ ["add", pathText (local1 </> [relfile|local1.cabal|])]
  git.cmd_ ["commit", "-m", "3"]

packages :: ProjectPackages
packages =
  [("local1", ManagedPackage {name = "local1", version = mkVersion [0, 2, 0], deps = initialDeps})]

envConfigs :: Envs EnvConfig
envConfigs =
  [
    ("latest", EnvConfig {targets = ["local1"], ghc = Just (GhcDbSystem Nothing), managedBound = Just BoundUpper})
  ]

maintContext :: MaintContextProto
maintContext =
  MaintContextProto {
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
    envs = [("latest", MaintEnv { targets = ["local1"], managedBound = Just BoundUpper })]
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
  _ -> Nothing

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

revisionConfig :: RevisionConfig
revisionConfig =
  def

revisionHandlers :: HackageClient -> M RevisionHandlers
revisionHandlers hackageUpload = do
  pure RevisionHandlers {
    git = gitApiRevisionHermetic revisionConfig,
    publishHackages = [hackageUpload]
  }

dummyTime :: UTCTime
dummyTime = UTCTime (fromGregorian 2024 1 16) (timeOfDayToTime (TimeOfDay 0 0 0))

testRepo :: Port -> HackageRepo
testRepo port =
  HackageRepo {
    name = "test",
    description = "test",
    location = HackageLocation {
      host = "localhost",
      port = Just (fromIntegral port.value),
      auth = Just (HackageAuthPassword {user = "test", password = "test"}),
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
    default-language: Haskell2010
    build-depends:
        base <5,
        extra >=1.7 && <1.9,
        semigroups >=0.19 && <0.21
|]

test_revision :: UnitTest
test_revision =
  withHixDir \ hixRoot -> do
    revisedCabalContents <- runMTest False do
      withTempRoot "test-maint" \ root ->
        withHackageClient \ hackage -> do
          packageDir <- runGitNativeHermetic root "test: project setup" (setupProject hixRoot)
          appContextDebug "publishing test package" do
            flags <- Cabal.solveFlags [remoteRepo hackage.repo] def
            targz <- sourceDistribution packageDir initialPackage
            verbosity <- cabalVerbosity
            publishPackage UploadConfig {verbosity, user = "test", password = "test"} flags targz
          runGitNativeHermetic root "test: bump deps" bumpDeps
          handlers <- revisionHandlers hackage.client
          publishRevisions handlers revisionConfig maintContext
          revisionCabalFile (NonEmpty.head handlers.publishHackages) initialPackage 1
    targetCabal === revisedCabalContents
