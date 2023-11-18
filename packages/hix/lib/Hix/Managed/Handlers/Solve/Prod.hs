module Hix.Managed.Handlers.Solve.Prod where

import Distribution.Client.Dependency (DepResolverParams)
import Path (Abs, Dir, Path)

import Hix.Managed.Handlers.Solve (SolveHandlers (..))
import Hix.Managed.Solve (solveWithCabal)
import qualified Hix.Managed.Solve.Resources as SolveResources
import Hix.Managed.Solve.Resources (solverParams)
import Hix.Monad (M)

handlersProd ::
  (DepResolverParams -> DepResolverParams) ->
  Maybe (Path Abs Dir) ->
  M SolveHandlers
handlersProd solverParams ghc = do
  solveResources <- SolveResources.acquire ghc
  pure SolveHandlers {
    solveForVersion = solveWithCabal solveResources {solverParams}
  }
