module Hix.Managed.StateFile where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Path (Abs, File, Path)

import Hix.Class.EncodeNix (encodeNix)
import Hix.Data.Deps (TargetRemoteDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Data.ManagedEnv (ManagedEnvState, ManagedState)
import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr (..), ExprAttr (..))
import qualified Hix.Log as Log
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import qualified Hix.Managed.Handlers.StateFile
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.Path (rootOrCwd)
import Hix.Managed.State (envStateForBuild)
import Hix.NixExpr (renderRootExpr)

renderMap ::
  Coercible k Text =>
  (v -> Expr) ->
  Map k v ->
  Expr
renderMap v =
  ExprAttrs . fmap (uncurry ExprAttr . bimap coerce v) . Map.toList

renderManaged' :: ManagedEnvState -> Expr
renderManaged' =
  encodeNix

writeStateFile ::
  Text ->
  StateFileHandlers ->
  Path Abs File ->
  ManagedEnvState ->
  M ()
writeStateFile purpose handlers stateFile state = do
  Log.debug [exon|writing managed stated file for #{purpose}: #{renderRootExpr expr}|]
  handlers.writeFile stateFile expr
  where
    expr = encodeNix state

writeProjectState ::
  StateFileHandlers ->
  StateFileConfig ->
  ManagedEnvState ->
  M ()
writeProjectState handlers conf newState = do
  root <- rootOrCwd conf.projectRoot
  stateFile <- handlers.initFile root conf.file
  writeStateFile "final result persistence" handlers stateFile newState

writeBuildStateFor ::
  Text ->
  StateFileHandlers ->
  TargetRemoteDeps ->
  EnvName ->
  ManagedState ->
  Path Abs File ->
  M ()
writeBuildStateFor purpose handlers targetDeps env state stateFile =
  writeStateFile purpose handlers stateFile envState
  where
    envState = envStateForBuild targetDeps env state
