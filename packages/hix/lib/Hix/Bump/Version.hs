module Hix.Bump.Version where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Set as Set
import Distribution.Types.Dependency (Dependency (Dependency))
import Distribution.Types.PackageName (unPackageName)
import Exon (exon)

import qualified Hix.Data.BumpConfig
import Hix.Data.BumpConfig (
  ManagedConfig (deps),
  ManagedDeps (ManagedDeps),
  OverrideManaged (OverrideManaged),
  OverrideVersion (OverrideVersion),
  OverridesEnvManaged (OverridesEnvManaged),
  OverridesManaged (OverridesManaged),
  PackagesManaged (PackagesManaged),
  )
import qualified Hix.Data.BumpHandlers
import Hix.Data.BumpHandlers (BumpHandlers)
import Hix.Data.ComponentConfig (
  ComponentDeps (ComponentDeps),
  EnvName,
  PackageDeps (PackageDeps),
  PackageName,
  PackagesDeps,
  packageNameFromCabal,
  )
import qualified Hix.Data.Version
import Hix.Data.Version (NewRange (NewRange), VersionBump (VersionBump))
import Hix.Monad (M, noteBump)
import Hix.Version (checkVersion)

bumpDep ::
  BumpHandlers ->
  ManagedDeps ->
  OverridesEnvManaged ->
  Dependency ->
  M (Maybe VersionBump)
bumpDep handlers (ManagedDeps managed) (OverridesEnvManaged overrides) dep = do
  latest <- handlers.latestVersion (fromString name)
  pure $ latest >>= \ lv ->
    withRange lv <|> onlyLatest lv
  where
    withRange lv = do
      newDep <- checkVersion (fromMaybe depVersion managedVersion) lv
      cons lv (Just (NewRange newDep))

    onlyLatest latest = case overrides !? fromString name of
      Just (OverrideManaged {version = OverrideVersion override}) | override >= latest ->
        Nothing
      _ -> Just (VersionBump {package, newVersion = latest, range = Nothing})

    cons lv range = Just (VersionBump {package, newVersion = lv, range})

    managedVersion = managed !? fromString name

    package = fromString name

    Dependency (unPackageName -> name) depVersion _ = dep

bumpComponents ::
  BumpHandlers ->
  Set PackageName ->
  PackageDeps ->
  ManagedDeps ->
  OverridesEnvManaged ->
  M [VersionBump]
bumpComponents handlers localNames (PackageDeps deps) managed overrides =
  catMaybes <$> traverse (bumpDep handlers managed overrides) (filter notLocal cdeps)
  where
    notLocal (Dependency (packageNameFromCabal -> name) _ _) = not (Set.member name localNames)
    cdeps = mconcat (coerce (Map.elems deps))

bumpPackage ::
  BumpHandlers ->
  PackagesDeps ->
  PackagesManaged ->
  OverridesEnvManaged ->
  PackageName ->
  M (PackageDeps, [VersionBump])
bumpPackage handlers deps (PackagesManaged managed) overrides pkg = do
  pdeps <- noteBump [exon|Package '##{pkg}' not found in the flake config|] (deps !? pkg)
  let pmanaged = fold (managed !? pkg)
  bumps <- bumpComponents handlers localNames pdeps pmanaged overrides
  pure (pdeps, bumps)
  where
    localNames = Map.keysSet deps

bumpPackages ::
  BumpHandlers ->
  PackagesDeps ->
  ManagedConfig ->
  EnvName ->
  [PackageName] ->
  M [(PackageName, Maybe (PackageDeps, NonEmpty VersionBump))]
bumpPackages handlers deps managed env pkgs = do
  for pkgs \ pkg -> do
    result <- bumpPackage handlers deps managed.deps overrides pkg
    pure (pkg, traverse nonEmpty result)
  where
    overrides = fromMaybe mempty (coerce managed.overrides !? env)
