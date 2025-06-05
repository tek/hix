module Hix.Managed.StateFile (
  writeProjectState,
  writeReleaseVersions,
  writeBuildStateFor,
  writeSolverStateFor,
  writeInitialEnvState,
) where

import Exon (exon)
import Path (Abs, Dir, Path)

import Hix.Class.EncodeNix (EncodeNix, encodeNix)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.ProjectState (ProjectState)
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import qualified Hix.Managed.Handlers.StateFile
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.UpdateState (envStateForBuild, envStateForSolver)
import Hix.Monad (appContextDebug)
import Hix.NixExpr (renderRootExpr)

writeStateFile ::
  EncodeNix state =>
  Text ->
  StateFileHandlers ->
  Maybe (Path Abs Dir) ->
  state ->
  M ()
writeStateFile purpose handlers tmpRoot state = do
  appContextDebug [exon|writing managed state file for #{purpose}: #{renderRootExpr expr}|] do
    handlers.writeFile tmpRoot expr
  where
    expr = encodeNix state

writeProjectState ::
  StateFileHandlers ->
  ProjectState ->
  M ()
writeProjectState handlers newState =
  writeStateFile "final result persistence" handlers Nothing newState

writeReleaseVersions ::
  StateFileHandlers ->
  ProjectStateProto ->
  M ()
writeReleaseVersions handlers newState =
  writeStateFile "release versions" handlers Nothing newState

writeBuildStateFor ::
  Text ->
  StateFileHandlers ->
  Path Abs Dir ->
  EnvName ->
  Overrides ->
  M ()
writeBuildStateFor purpose handlers tmpRoot env overrides =
  writeStateFile purpose handlers (Just tmpRoot) (envStateForBuild env overrides)

writeSolverStateFor ::
  Text ->
  StateFileHandlers ->
  Path Abs Dir ->
  EnvName ->
  Overrides ->
  M ()
writeSolverStateFor purpose handlers tmpRoot env overrides =
  writeStateFile purpose handlers (Just tmpRoot) (envStateForSolver env overrides)

writeInitialEnvState ::
  StateFileHandlers ->
  Path Abs Dir ->
  EnvName ->
  Initial EnvState ->
  M ()
writeInitialEnvState handlers tmpRoot env (Initial state) =
  writeBuildStateFor "env initialization" handlers tmpRoot env state.overrides
