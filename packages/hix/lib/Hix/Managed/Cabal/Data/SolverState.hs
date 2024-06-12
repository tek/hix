module Hix.Managed.Cabal.Data.SolverState (
  SolverState (SolverState, constraints, flags),
  solverState,
  updateSolverState,
  SolverFlags (..),
  compileSolverFlags,
) where

import Distribution.Client.Dependency (DepResolverParams, removeUpperBounds)
import Distribution.Client.Types (RelaxDeps (RelaxDepsAll))
import Distribution.Client.Types.AllowNewer (AllowNewer (AllowNewer))
import GHC.Records (HasField (getField))

import Hix.Class.Map (nGenWith)
import Hix.Data.Bounds (Ranges)
import Hix.Data.PackageName (LocalPackage (LocalPackage))
import Hix.Managed.Constraints (explicitBounds, noBounds)
import Hix.Managed.Data.Constraints (EnvConstraints)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvDeps)
import Hix.Managed.Data.Mutable (MutableDep, depName)

data SolverFlags =
  SolverFlags {
    allowNewer :: Bool,
    forceRevisions :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default SolverFlags where
  def = SolverFlags {allowNewer = False, forceRevisions = False}

flagAllowNewer :: DepResolverParams -> DepResolverParams
flagAllowNewer =
  removeUpperBounds (AllowNewer RelaxDepsAll)

compileSolverFlags :: SolverFlags -> DepResolverParams -> DepResolverParams
compileSolverFlags SolverFlags {..} =
  flag flagAllowNewer allowNewer
  where
    flag f v | v = f
             | otherwise = id

data SolverState =
  UnsafeSolverState EnvConstraints SolverFlags
  deriving stock (Eq, Show)

pattern SolverState :: EnvConstraints -> SolverFlags -> SolverState
pattern SolverState {constraints, flags} <- UnsafeSolverState constraints flags

{-# complete SolverState #-}

instance HasField "constraints" SolverState EnvConstraints where
  getField (UnsafeSolverState c _) = c

instance HasField "flags" SolverState SolverFlags where
  getField (UnsafeSolverState _ p) = p

-- TODO What's the point of this now that there's no @local@ flag in the constraints anymore?
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
  SolverFlags ->
  SolverState
solverState user deps modeConstraints flags =
  UnsafeSolverState constraints flags
  where
    constraints =
      modeConstraints <>
      explicitBounds user <>
      localDepConstraints deps.local <>
      mutableDepConstraints deps.mutable

updateSolverState :: (EnvConstraints -> EnvConstraints) -> SolverState -> SolverState
updateSolverState f (UnsafeSolverState c p) = UnsafeSolverState (f c) p
