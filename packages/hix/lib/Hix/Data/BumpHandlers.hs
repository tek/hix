module Hix.Data.BumpHandlers where

import Distribution.Version (Version)
import Path (Abs, Dir, File, Path, Rel, (</>))

import Hix.Data.BumpConfig (ManagedConfig)
import Hix.Data.ComponentConfig (EnvName, PackageName)
import Hix.Data.Error (Error (Fatal))
import Hix.Data.Version (VersionBump, VersionBumped)
import Hix.Monad (M, throwM)
import Hix.NixExpr (Expr)

data BuildResults =
  BuildResults {
    success :: [VersionBumped],
    failed :: [VersionBump],
    initial :: Bool
  }
  deriving stock (Eq, Show, Generic)

data BuildState =
  BuildState {
    success :: [VersionBumped],
    failed :: [VersionBump],
    managed :: ManagedConfig
  }
  deriving stock (Eq, Show)

data BuildsState =
  BuildsState {
    results :: Map PackageName BuildResults,
    managed :: ManagedConfig
  }
  deriving stock (Eq, Show, Generic)

newtype TempProjectBracket =
  TempProjectBracket (∀ a . Maybe (Path Abs Dir) -> (Path Abs Dir -> M a) -> M a)

tempProjectBracket :: ∀ a . TempProjectBracket -> Maybe (Path Abs Dir) -> (Path Abs Dir -> M a) -> M a
tempProjectBracket (TempProjectBracket f) = f

data BumpHandlers =
  BumpHandlers {
    latestVersion :: PackageName -> M (Maybe Version),
    withTempProject :: TempProjectBracket,
    buildProject :: Path Abs Dir -> EnvName -> PackageName -> VersionBump -> M Bool,
    initDepsFile :: Path Abs Dir -> Path Rel File -> M (Path Abs File),
    fetchHash :: VersionBump -> M VersionBumped,
    writeDepsFile :: Path Abs File -> Expr -> M (),
    reportBumps :: PackageName -> BuildResults -> M ()
  }

data SpecialBumpHandlers =
  TestBumpHandlers
  deriving stock (Eq, Show)

handlersNull :: BumpHandlers
handlersNull =
  BumpHandlers {
    latestVersion = \ _ -> pure Nothing,
    withTempProject = TempProjectBracket \ _ _ -> throwM (Fatal "not implemented: withTempProject"),
    buildProject = \ _ _ _ _ -> pure False,
    initDepsFile = \ r f -> pure (r </> f),
    fetchHash = \ _ -> throwM (Fatal "not implemented: fetchHash"),
    writeDepsFile = \ _ _ -> unit,
    reportBumps = \ _ _ -> unit
  }
