module Hix.Test.BumpTest where

import Data.Aeson (eitherDecodeStrict')
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Text as Text
import Distribution.Version (
  Version,
  earlierVersion,
  intersectVersionRanges,
  mkVersion,
  orLaterVersion,
  unionVersionRanges,
  )
import Exon (exon)
import Hedgehog (TestT, evalEither, evalMaybe, (===))
import Path (Abs, Dir, File, Path, absdir, relfile)

import Hix.Bump (bump)
import Hix.Bump.Version (bumpPackage)
import qualified Hix.Data.BumpConfig
import Hix.Data.BumpConfig (BumpConfig (BumpConfig), BumpEnv (BumpEnv), ManagedConfig (ManagedConfig), PackagesManaged, OverridesManaged)
import Hix.Data.BumpHandlers (BumpHandlers (..), TempProjectBracket (TempProjectBracket), handlersNull)
import Hix.Data.ComponentConfig (EnvName, PackageName (PackageName), PackagesDeps)
import Hix.Data.Error (Error (BumpError, Fatal))
import qualified Hix.Data.Version
import Hix.Data.Version (
  NewRange (NewRange),
  SourceHash (SourceHash),
  VersionBump (VersionBump),
  VersionBumped (VersionBumped),
  )
import Hix.Monad (M, runM, throwM)
import Hix.NixExpr (Expr, renderRootExpr)

root :: Path Abs Dir
root = [absdir|/project|]

tmpRoot :: Path Abs Dir
tmpRoot = [absdir|/tmp/project|]

depsVersion :: Either String PackagesDeps
depsVersion =
  eitherDecodeStrict' [exon|{
    "panda": {
      "library": {
        "dependencies": [
          "aeson ^>= 2.0",
          {
            "name": "extra",
            "version": "< 1.5"
          },
          "some",
          "containers (>= 0.1 && < 0.3) || (>= 1.1 && < 1.2.3) || >= 2.3",
          "polysemy ==1.0.1",
          "acme < 8.4",
          "cat ^>= 1.1"
        ]
      }
    }
  }|]

aesonVersion :: Version
aesonVersion = mkVersion [2, 2, 0, 5]

extraVersion :: Version
extraVersion = mkVersion [1, 7, 14]

someVersion :: Version
someVersion = mkVersion [1, 0, 5]

containersVersion :: Version
containersVersion = mkVersion [2, 4, 5]

polysemyVersion :: Version
polysemyVersion = mkVersion [1, 9, 1, 0]

acmeVersion :: Version
acmeVersion = mkVersion [7, 3]

catVersion :: Version
catVersion = mkVersion [1, 1, 1]

latestVersion :: PackageName -> M (Maybe Version)
latestVersion =
  fmap Just . \case
    "aeson" -> pure aesonVersion
    "extra" -> pure extraVersion
    "some" -> pure someVersion
    "containers" -> pure containersVersion
    "polysemy" -> pure polysemyVersion
    "acme" -> pure acmeVersion
    "cat" -> pure catVersion
    _ -> throwM (BumpError "No such package")

withTempProject :: Maybe (Path Abs Dir) -> (Path Abs Dir -> M a) -> M a
withTempProject _ use = use tmpRoot

handlersVersion :: BumpHandlers
handlersVersion =
  handlersNull {
    latestVersion,
    withTempProject = TempProjectBracket withTempProject
  }

target :: [VersionBump]
target =
  [
    mkBump "aeson" aesonRange aesonVersion,
    mkBump "extra" extraRange extraVersion,
    mkBump "some" someRange someVersion,
    mkBump "containers" containersRange containersVersion,
    mkBump "polysemy" polysemyRange polysemyVersion,
    mkBump "acme" acmeRange acmeVersion,
    VersionBump {package = "cat", range = Nothing, newVersion = mkVersion [1, 1, 1]}
  ]
  where
    mkBump name range newVersion =
      VersionBump {
        package = fromString name,
        range = Just (NewRange range),
        newVersion
      }

    aesonRange = between [2, 0] [2, 3]

    extraRange = between [1, 7] [1, 8]

    someRange = between [1, 0] [1, 1]

    containersRange =
      foldr1 @NonEmpty unionVersionRanges [
        (between [0, 1] [0, 3]),
        (between [1, 1] [1, 2, 3]),
        (between [2, 3] [2, 5])
      ]

    polysemyRange = between [1, 0, 1] [1, 10]

    acmeRange = between [7, 3] [7, 4]

    between l r = intersectVersionRanges (orLaterVersion (mkVersion l)) (earlierVersion (mkVersion r))

test_bumpVersions :: TestT IO ()
test_bumpVersions = do
  deps <- leftA fail depsVersion
  (_, res) <- evalEither =<< liftIO (runM root (bumpPackage handlersVersion deps mempty mempty "panda"))
  target === res

depsBuild :: Either String PackagesDeps
depsBuild =
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

latestVersionBuild :: PackageName -> M (Maybe Version)
latestVersionBuild =
  fmap Just . \case
    "dep1" -> pure (mkVersion [2, 2, 0, 5])
    "dep2" -> pure (mkVersion [1, 7, 14])
    "dep3" -> pure (mkVersion [1, 0, 5])
    "dep4" -> pure (mkVersion [2, 2])
    "dep5" -> pure (mkVersion [5, 0])
    _ -> throwM (BumpError "No such package")

buildProjectBuild :: Path Abs Dir -> EnvName -> PackageName -> VersionBump -> M Bool
buildProjectBuild _ _ _ b =
  case b.package of
    "dep1" -> pure True
    "dep2" -> pure True
    "dep3" -> pure False
    "dep4" -> pure True
    pkg -> throwM (Fatal [exon|Unexpected package for building project: ##{pkg}|])

fetchHashBuild :: VersionBump -> M VersionBumped
fetchHashBuild b@VersionBump {package = PackageName "dep5"} =
  pure VersionBumped {bump = b, hash = SourceHash "invalid-hash"}
fetchHashBuild b@VersionBump {package = PackageName name} =
  pure VersionBumped {bump = b, hash = SourceHash name}

writeDepsFileBuild ::
  IORef [Expr] ->
  Path Abs File ->
  Expr ->
  M ()
writeDepsFileBuild out _ expr =
  liftIO $ modifyIORef out (expr :)

handlersBuild :: IO (BumpHandlers, IORef [Expr])
handlersBuild = do
  depsFileRef <- newIORef []
  let
    handlers = handlersNull {
      latestVersion = latestVersionBuild,
      withTempProject = TempProjectBracket withTempProject,
      buildProject = buildProjectBuild,
      fetchHash = fetchHashBuild,
      writeDepsFile = writeDepsFileBuild depsFileRef
    }
  pure (handlers, depsFileRef)

managedDepsBuild :: Either String PackagesManaged
managedDepsBuild =
  eitherDecodeStrict' [exon|{
    "panda": {
      "dep4": ">= 2.0"
      }
  }|]

managedOverridesBuild :: Either String OverridesManaged
managedOverridesBuild =
  eitherDecodeStrict' [exon|{
    "fancy": {
      "dep5": {
          "version": "5.0",
          "hash": "dep5"
        }
      }
  }|]

depsFileStep1Target :: Text
depsFileStep1Target =
  [exon|{
  deps = {
    panda = {
      dep1 = ">=2.0 && <2.3";
      dep2 = "<1.5";
      dep3 = ">=0";
      dep4 = ">=2.0";
      dep5 = ">=5.0 && <5.1";
    };
  };
  overrides = {
    fancy = {
      dep1 = {
        version = "2.2.0.5";
        hash = "dep1";
      };
      dep5 = {
        version = "5.0";
        hash = "dep5";
      };
    };
  };
}
|]

-- TODO should @dep2@ be @>= 1.4@ since it was originally @< 1.5@, suggesting that at the very least, it works with
-- that version?
depsFileTarget :: Text
depsFileTarget =
  [exon|{
  deps = {
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
}
|]

eqLines :: Text -> Text -> TestT IO ()
eqLines l r =
  Text.lines l === Text.lines r

test_bumpBuild :: TestT IO ()
test_bumpBuild = do
  deps <- leftA fail depsBuild
  managedDeps <- leftA fail managedDepsBuild
  managedOverrides <- leftA fail managedOverridesBuild
  (handlers, depsFileRef) <- liftIO handlersBuild
  let
    env =
      BumpEnv {
        deps = deps,
        managed = ManagedConfig {deps = managedDeps, overrides = managedOverrides}
      }
    conf =
      BumpConfig {
        env = "fancy",
        package = Nothing,
        file = [relfile|ops/deps.nix|],
        updateProject = True,
        projectRoot = Just root,
        latestOverrides = True
      }
  evalEither =<< liftIO (runM root (bump handlers env conf))
  files <- liftIO (readIORef depsFileRef)
  eqLines depsFileStep1Target . renderRootExpr =<< evalMaybe (last files)
  eqLines depsFileTarget . renderRootExpr =<< evalMaybe (head files)
