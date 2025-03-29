module Hix.Managed.Build.SolverPackages where

import Control.Monad.Trans.State.Strict (runStateT)
import Exon (exon)
import Path (Abs, Dir, Path, parseAbsDir)

import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import qualified Hix.Log as Log
import Hix.Managed.Build.Adapt (buildAdaptive)
import Hix.Managed.Build.Target (BuilderResources (..), buildSolverPackages, suggestJailbreakAndLatestVersion)
import Hix.Managed.Cabal.Data.Config (GhcDb (..), GhcPath (..))
import Hix.Managed.Data.Initial (Initial (..))
import Hix.Managed.Data.StageState (BuildResult (..))
import Hix.Monad (appContext, eitherFatal)
import Hix.Pretty (showP)

badOutput :: Text -> Text
badOutput output = [exon|The output of building the solver package set is not a single valid store path: #{output}|]

validateSolverSuccess :: [Text] -> Either Text (Path Abs Dir)
validateSolverSuccess = \case
  [output] -> maybeToRight (badOutput output) (parseAbsDir (toString output))
  [] -> Left (badOutput "<empty output>")
  output : _ -> Left (badOutput output)

-- | Check stdout of the Nix build to verify it consists of a single line, and parse that line as an absolute path to a
-- GHC package set.
solverBuildResult :: (BuildResult, a) -> Either Text (GhcDb, a)
solverBuildResult = \case
  (BuildSuccess output, overrides) ->
    validateSolverSuccess output <&> \ path ->
      (GhcDbSystem (Just (GhcPath path)), overrides)
  (BuildFailure failure, _) ->
    Left [exon|Some packages could not be fixed automatically. Reason: #{showP failure}|]

solverResult :: (BuildResult, (a, b)) -> M (GhcDb, a)
solverResult (result, (overrides, _)) =
  eitherFatal (solverBuildResult (result, overrides))

nixSolverGhc ::
  BuilderResources ->
  EnvName ->
  Initial Overrides ->
  M (GhcDb, Overrides)
nixSolverGhc resources env (Initial storedOverrides) =
  appContext "validating the solver package set" do
    leftA cleanBuild =<< previousBuild
  where
    cleanBuild err = do
      Log.verbose "Build with previous solver overrides failed"
      Log.debug err
      appContext "computing the overrides for the solver package set" do
        solverResult =<< runStateT (build (suggestJailbreakAndLatestVersion resources)) (mempty, mempty)

    previousBuild = do
      result <- buildSolverPackages resources env storedOverrides
      pure (solverBuildResult (result, storedOverrides))

    build = buildAdaptive (buildSolverPackages resources env)

solverGhc ::
  BuilderResources ->
  EnvName ->
  Initial Overrides ->
  Maybe GhcDb ->
  M (GhcDb, Maybe Overrides)
solverGhc resources env overrides = \case
  Nothing -> second Just <$> nixSolverGhc resources env overrides
  Just ghc -> pure (ghc, Nothing)
