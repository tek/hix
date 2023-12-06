module Hix.Managed.Build where

import Control.Monad (foldM)
import Distribution.Pretty (Pretty, pretty)
import Exon (exon)
import Text.PrettyPrint (hang, vcat)

import Hix.Class.Map (ntAmend)
import Hix.Data.Bounds (BoundExtension (LowerBoundExtension, UpperBoundExtension), BoundExtensions, Bounds)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import Hix.Data.Package (PackageName)
import Hix.Data.Version (NewVersion)
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (BuildMutation (BuildMutation), DepMutation, MutationResult (..))
import Hix.Managed.Build.UpdateState (updateState)
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.Build (
  BuildResult,
  BuildState (BuildState),
  BuildStatus (Failure, Success),
  buildResult,
  justSuccess,
  )
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate)
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (Builder, EnvBuilder)
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers)
import Hix.Managed.Job (withJobBuilder)
import Hix.Managed.Overrides (newVersionOverrides)
import Hix.Managed.State (stateWithCandidate)
import Hix.Pretty (prettyL, showP, showPL)
import Hix.Version (setLowerBound, setUpperBound)

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

logCandidateResult :: Candidate -> BuildStatus -> M ()
logCandidateResult candidate status =
  Log.debug [exon|Build for candidate '##{showP candidate}' #{result status}|]
  where
    result = \case
      Success -> "succeeded"
      Failure -> "failed"

buildBounds :: BoundExtensions -> Bounds -> Bounds
buildBounds =
  ntAmend \case
    LowerBoundExtension version -> setLowerBound version
    UpperBoundExtension version -> setUpperBound version

buildMutation ::
  HackageHandlers ->
  EnvBuilder ->
  ManagedJob ->
  ManagedState ->
  BuildMutation ->
  M (Maybe ManagedState)
buildMutation handlers builder job state BuildMutation {candidate, newVersions, accOverrides, newBounds} = do
  newOverrides <- newVersionOverrides handlers (candidate.version : newVersions)
  let overrides = newOverrides <> accOverrides
      bounds = buildBounds newBounds state.bounds
      newState = stateWithCandidate bounds candidate overrides
  logBuildInputs state candidate newVersions newOverrides accOverrides
  Log.info [exon|Building targets in '##{env}' with '#{showP candidate}'...|]
  status <- builder.buildWithState newState
  logCandidateResult candidate status
  pure (justSuccess newState status)
  where
    env = job.env

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
  HackageHandlers ->
  EnvBuilder ->
  ManagedJob ->
  MutationHandlers a s ->
  BuildState a s ->
  DepMutation a ->
  M (BuildState a s)
validateMutation hackage env job handlers buildState mutation = do
  result <- handlers.process buildState.ext mutation build
  logMutationResult mutation.package result
  pure (updateState buildState mutation result)
  where
    build = buildMutation hackage env job buildState.state

buildMutations ::
  Pretty a =>
  HackageHandlers ->
  Builder ->
  (EnvBuilder -> M (MutationHandlers a s)) ->
  ManagedJob ->
  ManagedState ->
  [DepMutation a] ->
  s ->
  M (BuildResult a)
buildMutations hackage builder mkHandlers job state mutations ext = do
  Log.debug [exon|Building targets with mutations: #{showPL mutations}|]
  withJobBuilder builder job state \ envBuilder -> do
    handlers <- mkHandlers envBuilder
    buildResult job.removable <$> foldM (validateMutation hackage envBuilder job handlers) initialState mutations
  where
    initialState = BuildState {success = [], failed = [], state, ext}
