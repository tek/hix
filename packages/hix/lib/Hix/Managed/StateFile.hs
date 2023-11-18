module Hix.Managed.StateFile where

import qualified Data.Map.Strict as Map
import Distribution.Pretty (pretty)
import Distribution.Version (simplifyVersionRange)
import Exon (exon)
import Path (Abs, File, Path)

import Hix.Data.Bounds (Bounds (Bounds), TargetBounds (TargetBounds))
import Hix.Data.EnvName (EnvName (EnvName))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedEnvState, ManagedState)
import Hix.Data.Overrides (EnvOverrides (EnvOverrides), Override (..), Overrides (Overrides))
import Hix.Data.Package (LocalPackage (LocalPackage), PackageName (PackageName))
import Hix.Data.Version (SourceHash (SourceHash))
import qualified Hix.Log as Log
import Hix.Managed.Build.Mutation (Candidate)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Data.ManagedJob (ManagedJob)
import Hix.Managed.Handlers.Build.Prod (rootOrCwd)
import qualified Hix.Managed.Handlers.StateFile
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.State (managedEnvForBuild, managedEnvForProject, managedWithCandidate)
import Hix.Monad (M)
import Hix.NixExpr (Expr (..), ExprAttr (..), renderRootExpr)
import Hix.Pretty (showP)

renderMap ::
  Coercible k Text =>
  (v -> Expr) ->
  Map k v ->
  Expr
renderMap v =
  ExprAttrs . fmap (uncurry ExprAttr . bimap coerce v) . Map.toList

renderManaged :: ManagedEnvState -> Expr
renderManaged conf =
  ExprAttrs [ExprAttr "bounds" (renderPackages conf.bounds), ExprAttr "overrides" (renderOverrides conf.overrides)]
  where
    renderPackages (TargetBounds pkgs) =
      renderMap renderPackage pkgs
    renderPackage (Bounds deps) =
      renderMap (ExprString . show . pretty . simplifyVersionRange) deps
    renderOverrides (EnvOverrides os) =
      renderMap renderOverridesEnv os
    renderOverridesEnv (Overrides os) =
      renderMap renderOverride os
    renderOverride (Override {version = version, hash = SourceHash hash}) =
      ExprAttrs [
        ExprAttr "version" (ExprString (showP version)),
        ExprAttr "hash" (ExprString hash)
      ]

writeStateFile ::
  StateFileHandlers ->
  Path Abs File ->
  ManagedEnvState ->
  M ()
writeStateFile handlers stateFile state = do
  Log.debug [exon|writing managed stated file: #{renderRootExpr expr}|]
  handlers.writeFile stateFile expr
  where
    expr = renderManaged state

writeProjectState ::
  StateFileHandlers ->
  ManagedJob ->
  StateFileConfig ->
  ManagedEnvState ->
  ManagedState ->
  M ()
writeProjectState handlers job conf originalManaged managed = do
  root <- rootOrCwd conf.projectRoot
  stateFile <- handlers.initFile root conf.file
  writeStateFile handlers stateFile (managedEnvForProject job conf originalManaged managed)

writeBuildState ::
  StateFileHandlers ->
  ManagedJob ->
  Bounds ->
  Path Abs File ->
  Candidate ->
  Overrides ->
  M ManagedState
writeBuildState handlers job bounds stateFile candidate overrides =
  state <$ writeStateFile handlers stateFile envState
  where
    envState = managedEnvForBuild job state
    state = managedWithCandidate bounds candidate overrides
