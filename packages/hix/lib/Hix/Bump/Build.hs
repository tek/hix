module Hix.Bump.Build where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Distribution.Pretty (pretty)
import Distribution.Types.Dependency (Dependency (Dependency))
import Distribution.Version (VersionRange)
import Exon (exon)
import Path (Abs, Dir, File, Path)

import qualified Hix.Data.BumpConfig
import Hix.Data.BumpConfig (
  BumpConfig,
  ManagedConfig (ManagedConfig),
  ManagedDeps (ManagedDeps),
  OverrideManaged (OverrideManaged),
  OverrideVersion (OverrideVersion),
  OverridesEnvManaged (OverridesEnvManaged),
  OverridesManaged (OverridesManaged),
  PackagesManaged (PackagesManaged),
  )
import qualified Hix.Data.BumpHandlers
import Hix.Data.BumpHandlers (BuildResults (BuildResults), BuildState (BuildState), BumpHandlers, tempProjectBracket)
import Hix.Data.ComponentConfig (
  ComponentDeps (ComponentDeps),
  EnvName (EnvName),
  PackageDeps (PackageDeps),
  PackageName (PackageName),
  packageNameFromCabal,
  )
import qualified Hix.Data.Version
import Hix.Data.Version (
  NewRange (NewRange),
  SourceHash (SourceHash),
  VersionBump (VersionBump),
  VersionBumped (VersionBumped),
  )
import Hix.Monad (M, logDebug)
import Hix.NixExpr (Expr (ExprAttrs, ExprString), ExprAttr (ExprAttr), renderRootExpr)

type Deps = Map PackageName VersionRange

cdepsByName :: ComponentDeps -> Deps
cdepsByName (ComponentDeps cdeps) =
  Map.fromList $ cdeps <&> \ (Dependency name version _) -> (packageNameFromCabal name, version)

initManaged ::
  PackageName ->
  ManagedConfig ->
  Deps ->
  ManagedConfig
initManaged pkg ManagedConfig {deps = PackagesManaged pkgs, overrides} deps =
  ManagedConfig {
    deps = PackagesManaged newPkgs,
    overrides
  }
  where
    newPkgs = Map.insert pkg newPkg pkgs

    newPkg = ManagedDeps (Map.foldlWithKey' step oldPkg deps)

    oldPkg = fold (coerce <$> pkgs !? pkg)

    step z name version | Map.member name z = z
                        | Map.member name pkgs = z
                        | otherwise = Map.insert name version z

managedWithBumps ::
  ManagedConfig ->
  EnvName ->
  PackageName ->
  VersionBumped ->
  ManagedConfig
managedWithBumps conf env name VersionBumped {bump = VersionBump {package, range, newVersion}, hash} =
  new
  where
    new = ManagedConfig {deps = newDeps, overrides = newOverrides}

    newDeps = PackagesManaged (Map.insert name (ManagedDeps addBump) pkgs)

    addBump = case range of
      Just (NewRange r) -> Map.insert package r oldDeps
      Nothing -> oldDeps

    ManagedDeps oldDeps = fold (pkgs !? name)

    newOverrides = OverridesManaged (Map.insert env newOverridesEnv overrides)

    newOverridesEnv = OverridesEnvManaged (Map.insert package override overridesEnv)

    override = OverrideManaged {version = OverrideVersion newVersion, hash}

    OverridesEnvManaged overridesEnv = fold (overrides !? env)

    ManagedConfig {deps = PackagesManaged pkgs, overrides = OverridesManaged overrides} = conf

renderMap ::
  Coercible k Text =>
  (v -> Expr) ->
  Map k v ->
  Expr
renderMap v =
  ExprAttrs . fmap (uncurry ExprAttr . bimap coerce v) . Map.toList

renderDeps :: ManagedConfig -> Expr
renderDeps conf =
  ExprAttrs [ExprAttr "deps" (renderPackages conf.deps), ExprAttr "overrides" (renderOverrides conf.overrides)]
  where
    renderPackages (PackagesManaged pkgs) =
      renderMap renderPackage pkgs
    renderPackage (ManagedDeps deps) =
      renderMap (ExprString . show . pretty) deps
    renderOverrides (OverridesManaged os) =
      renderMap renderOverridesEnv os
    renderOverridesEnv (OverridesEnvManaged os) =
      renderMap renderOverride os
    renderOverride (OverrideManaged {version = OverrideVersion version, hash = SourceHash hash}) =
      ExprAttrs [ExprAttr "version" (ExprString (show (pretty version))), ExprAttr "hash" (ExprString hash)]

updateState ::
  BuildState ->
  ManagedConfig ->
  VersionBumped ->
  Bool ->
  BuildState
updateState state managed bumped = \case
  True -> BuildState (bumped : state.success) state.failed managed
  False -> BuildState state.success (bumped.bump : state.failed) state.managed

createDepsFile ::
  BumpHandlers ->
  ManagedConfig ->
  EnvName ->
  PackageName ->
  Path Abs File ->
  VersionBumped ->
  M ManagedConfig
createDepsFile handlers managed env pkg depsFile bump = do
  logDebug [exon|writing deps file:
  #{renderRootExpr expr}|]
  testManaged <$ handlers.writeDepsFile depsFile expr
  where
    expr = renderDeps testManaged
    testManaged = managedWithBumps managed env pkg bump

buildWithBump ::
  BumpHandlers ->
  BumpConfig ->
  PackageName ->
  Path Abs Dir ->
  Path Abs File ->
  BuildState ->
  VersionBump ->
  M BuildState
buildWithBump handlers conf pkg root depsFile state bump = do
  bumped <- handlers.fetchHash bump
  newManaged <- createDepsFile handlers state.managed conf.env pkg depsFile bumped
  updateState state newManaged bumped <$> handlers.buildProject root conf.env pkg bump

buildWithBumps ::
  BumpHandlers ->
  BumpConfig ->
  ManagedConfig ->
  PackageName ->
  PackageDeps ->
  NonEmpty VersionBump ->
  M (BuildResults, ManagedConfig)
buildWithBumps handlers conf oldManaged pkg (PackageDeps pkgDeps) bumps = do
  logDebug [exon|Building '##{pkg}' with bumps: #{show bumps}|]
  final <- tempProjectBracket handlers.withTempProject conf.projectRoot \ tmpRoot -> do
    depsFile <- handlers.initDepsFile tmpRoot conf.file
    foldM (buildWithBump handlers conf pkg tmpRoot depsFile) (BuildState [] [] managed) bumps
  pure (BuildResults {success = final.success, failed = final.failed, initial}, final.managed)
  where
    initial = Map.member pkg pkgs

    ManagedConfig {deps = PackagesManaged pkgs} = managed

    managed = initManaged pkg oldManaged deps

    deps = mconcat (cdepsByName <$> (Map.elems pkgDeps))
