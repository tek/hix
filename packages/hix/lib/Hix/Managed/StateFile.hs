module Hix.Managed.StateFile where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Path (Abs, Dir, Path)

import Hix.Class.EncodeNix (encodeNix)
import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr (..), ExprAttr (..), ExprKey (..))
import Hix.Data.Overrides (Overrides)
import qualified Hix.Log as Log
import Hix.Managed.Data.EnvContext (EnvContext)
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.ProjectState (ProjectState)
import qualified Hix.Managed.Handlers.StateFile
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.UpdateState (envStateForBuild)
import Hix.NixExpr (renderRootExpr)

renderMap ::
  Coercible k Text =>
  (v -> Expr) ->
  Map k v ->
  Expr
renderMap v =
  ExprAttrs . fmap (uncurry ExprAttr . bimap coerce v) . Map.toList

renderManaged' :: ProjectState -> Expr
renderManaged' =
  encodeNix

writeStateFile ::
  Text ->
  StateFileHandlers ->
  Maybe (Path Abs Dir) ->
  ProjectState ->
  M ()
writeStateFile purpose handlers tmpRoot state = do
  Log.trace [exon|writing managed state file for #{purpose}: #{renderRootExpr expr}|]
  handlers.writeFile tmpRoot expr
  where
    expr = encodeNix state

writeProjectState ::
  StateFileHandlers ->
  ProjectState ->
  M ()
writeProjectState handlers newState =
  writeStateFile "final result persistence" handlers Nothing newState

writeBuildStateFor ::
  Text ->
  StateFileHandlers ->
  Path Abs Dir ->
  EnvContext ->
  Overrides ->
  M ()
writeBuildStateFor purpose handlers tmpRoot context overrides =
  writeStateFile purpose handlers (Just tmpRoot) (envStateForBuild context overrides)

writeInitialEnvState ::
  StateFileHandlers ->
  Path Abs Dir ->
  EnvContext ->
  Initial EnvState ->
  M ()
writeInitialEnvState handlers tmpRoot context (Initial state) =
  writeBuildStateFor "env initialization" handlers tmpRoot context state.overrides
