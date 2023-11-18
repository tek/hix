module Hix.Managed.Build where

import Control.Monad (foldM)
import Data.Foldable.Extra (allM)
import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty, pretty)
import Exon (exon)
import Text.PrettyPrint (hang, vcat)

import Hix.Class.Map (ntAmend)
import Hix.Data.Bounds (BoundExtension (LowerBoundExtension, UpperBoundExtension), BoundExtensions, Bounds)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides (Overrides))
import Hix.Data.Package (PackageName)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Env
import Hix.Managed.Build.Env (BuildEnv, withBuildEnv)
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (BuildMutation (BuildMutation), DepMutation, MutationResult (..))
import Hix.Managed.Build.UpdateState (updateState)
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.Build (BuildResult, BuildState (BuildState), buildResult)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
import Hix.Managed.Data.Targets (getTargets)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Hackage
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers)
import Hix.Managed.StateFile (writeBuildState)
import Hix.Pretty (prettyL, showP, showPL)
import Hix.Version (setLowerBound, setUpperBound)

newVersionOverride ::
  BuildHandlers ->
  NewVersion ->
  M (PackageName, Override)
newVersionOverride handlers NewVersion {..} = do
  hash <- handlers.hackage.fetchHash package version
  pure (package, Override {..})

newVersionOverrides ::
  BuildHandlers ->
  [NewVersion] ->
  M Overrides
newVersionOverrides handlers versions =
  Overrides . Map.fromList <$> traverse (newVersionOverride handlers) versions

logBuildInputs ::
  ManagedState ->
  Candidate ->
  [NewVersion] ->
  Overrides ->
  Overrides ->
  M ()
logBuildInputs managed candidate newVersions newOverrides accOverrides = do
  Log.verbose [exon|Executing build with new dep version '#{showP candidate}'|]
  Log.debug (show (vcat ["Managed state pre:", pretty managed]))
  Log.debug (show (hang "Extra versions:" 2 (prettyL newVersions)))
  Log.debug (show (hang "New overrides:" 2 (pretty newOverrides)))
  Log.debug (show (hang "Overrides retained from previous runs:" 2 (pretty accOverrides)))

logCandidateResult :: Candidate -> Bool -> M ()
logCandidateResult candidate success =
  Log.debug [exon|Build for candidate '##{showP candidate}' #{result}|]
  where
    result | success = "succeeded"
           | otherwise = "failed"

buildBounds :: BoundExtensions -> Bounds -> Bounds
buildBounds =
  ntAmend \case
    LowerBoundExtension version -> setLowerBound version
    UpperBoundExtension version -> setUpperBound version

buildMutation ::
  BuildEnv a s ->
  ManagedState ->
  BuildMutation ->
  M (Maybe ManagedState)
buildMutation env managed BuildMutation {candidate, newVersions, accOverrides, newBounds} = do
  newOverrides <- newVersionOverrides env.build (candidate.version : newVersions)
  let overrides = newOverrides <> accOverrides
      bounds = buildBounds newBounds managed.bounds
  logBuildInputs managed candidate newVersions newOverrides accOverrides
  newManaged <- writeBuildState env.build.stateFile env.job bounds env.stateFile candidate overrides
  success <- flip allM (getTargets env.job.targets) \ target ->
    env.build.buildProject env.root env.job.env target candidate.version
  logCandidateResult candidate success
  pure (if success then Just newManaged else Nothing)

logMutationResult ::
  PackageName ->
  MutationResult s ->
  M ()
logMutationResult package = \case
  MutationSuccess candidate _ _ ->
    Log.verbose [exon|Build succeeded for #{showP candidate}|]
  MutationUpdateBounds _ range ->
    Log.verbose [exon|'##{package}' only needs bounds update: #{showP range}|]
  MutationKeep ->
    Log.verbose [exon|Build is up to date for '##{package}'|]
  MutationFailed ->
    Log.verbose [exon|Could not find a buildable version of '##{package}'|]

validateMutation ::
  BuildEnv a s ->
  BuildState a s ->
  DepMutation a ->
  M (BuildState a s)
validateMutation env state mutation = do
  result <- env.mutation.process state.ext mutation build
  logMutationResult mutation.package result
  pure (updateState state mutation result)
  where
    build mut = buildMutation env state.managed mut

buildMutations ::
  Pretty a =>
  BuildHandlers ->
  MutationHandlers a s ->
  StateFileConfig ->
  ManagedJob ->
  ManagedState ->
  [DepMutation a] ->
  s ->
  M (BuildResult a)
buildMutations build mutation conf job managed mutations ext = do
  Log.debug [exon|Building targets with mutations: #{showPL mutations}|]
  withBuildEnv build mutation job conf \ env ->
    buildResult <$> foldM (validateMutation env) initialState mutations
  where
    initialState = BuildState {success = [], failed = [], managed, ext}
