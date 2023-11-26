module Hix.Managed.StateFile where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Path (Abs, File, Path)

import Hix.Class.EncodeNix (encodeNix)
import Hix.Data.Bounds (Bounds)
import Hix.Data.ManagedEnv (ManagedEnvState, ManagedState)
import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr (..), ExprAttr (..))
import Hix.Data.Overrides (Overrides)
import qualified Hix.Log as Log
import Hix.Managed.Data.Candidate (Candidate)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Data.ManagedJob (ManagedJob)
import Hix.Managed.Handlers.Build.Prod (rootOrCwd)
import qualified Hix.Managed.Handlers.StateFile
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.State (managedEnvForBuild, managedEnvForProject, managedWithCandidate)
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
  ManagedJob ->
  StateFileConfig ->
  ManagedEnvState ->
  ManagedState ->
  M ()
writeProjectState handlers job conf originalManaged managed = do
  root <- rootOrCwd conf.projectRoot
  stateFile <- handlers.initFile root conf.file
  writeStateFile "final result persistence" handlers stateFile (managedEnvForProject job originalManaged managed)

writeBuildState ::
  StateFileHandlers ->
  ManagedJob ->
  Bounds ->
  Path Abs File ->
  Candidate ->
  Overrides ->
  M ManagedState
writeBuildState handlers job bounds stateFile candidate overrides =
  state <$ writeStateFile "current build" handlers stateFile envState
  where
    envState = managedEnvForBuild job state
    state = managedWithCandidate bounds candidate overrides
