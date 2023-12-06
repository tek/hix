module Hix.Managed.Handlers.Solve.Prod where

import Distribution.Client.Dependency (DepResolverParams)

import Hix.Data.Monad (M)
import Hix.Managed.Handlers.Solve (SolveHandlers (..))
import Hix.Managed.Solve (solveWithCabal)
import Hix.Managed.Solve.Config (GhcDb)
import qualified Hix.Managed.Solve.Resources as SolveResources
import Hix.Managed.Solve.Resources (solverParams)

handlersProd ::
  (DepResolverParams -> DepResolverParams) ->
  Maybe GhcDb ->
  M SolveHandlers
handlersProd solverParams ghc = do
  solveResources <- SolveResources.acquire ghc
  pure SolveHandlers {
    solveForVersion = solveWithCabal solveResources {solverParams}
  }
