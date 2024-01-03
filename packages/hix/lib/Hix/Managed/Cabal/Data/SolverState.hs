module Hix.Managed.Cabal.Data.SolverState (
  SolverState (SolverState, constraints),
  solverState,
  updateSolverState,
) where

import GHC.Records (HasField (getField))

import Hix.Class.Map (nGenWith)
import Hix.Data.Bounds (Ranges)
import Hix.Data.PackageName (LocalPackage (LocalPackage))
import Hix.Managed.Constraints (explicitBounds, noBounds)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvDeps)
import Hix.Managed.Data.Mutable (MutableDep, depName)
import Hix.Managed.Data.Constraints (EnvConstraints)

data SolverState =
  UnsafeSolverState EnvConstraints
  deriving stock (Eq, Show, Generic)

pattern SolverState :: EnvConstraints -> SolverState
pattern SolverState {constraints} <- UnsafeSolverState constraints

{-# complete SolverState #-}

instance HasField "constraints" SolverState EnvConstraints where
  getField (UnsafeSolverState c) = c

-- TODO What's the point of this now that there's no local flag in the constraints anymore?
localDepConstraints :: Set LocalPackage -> EnvConstraints
localDepConstraints =
  nGenWith \ (LocalPackage package) -> (package, noBounds)

mutableDepConstraints :: Set MutableDep -> EnvConstraints
mutableDepConstraints =
  nGenWith \ package -> (depName package, noBounds)

solverState ::
  Ranges ->
  EnvDeps ->
  EnvConstraints ->
  SolverState
solverState user deps modeConstraints =
  UnsafeSolverState constraints
  where
    constraints =
      modeConstraints <>
      explicitBounds user <>
      localDepConstraints deps.local <>
      mutableDepConstraints deps.mutable

updateSolverState :: (EnvConstraints -> EnvConstraints) -> SolverState -> SolverState
updateSolverState f (UnsafeSolverState c) = UnsafeSolverState (f c)
