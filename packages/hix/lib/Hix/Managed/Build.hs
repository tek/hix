module Hix.Managed.Build where

import Control.Monad (foldM)
import Distribution.Pretty (Pretty, pretty)
import Exon (exon)
import Text.PrettyPrint (hang, vcat)

import Hix.Class.Map (ntAmend)
import Hix.Data.Bounds (BoundExtension (LowerBoundExtension, UpperBoundExtension), BoundExtensions, Bounds)
import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (PackageName)
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (BuildMutation (BuildMutation), DepMutation, MutationResult (..))
import Hix.Managed.Build.UpdateState (updateState)
import qualified Hix.Managed.Data.BuildDomain
import Hix.Managed.Data.BuildDomain (BuildDomain)
import qualified Hix.Managed.Data.BuildState
import Hix.Managed.Data.BuildState (BuildState, BuildStatus (Failure, Success), justSuccess)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (Builder (Builder), EnvBuilder)
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers)
import Hix.Managed.Overrides (newVersionOverrides)
import Hix.Managed.State (stateWithCandidate)
import Hix.Pretty (prettyL, showP, showPL)
import Hix.Version (setLowerBound, setUpperBound)

logBuildInputs ::
  ManagedState ->
  Candidate ->
  [PackageId] ->
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
  Log.info [exon|Build for candidate '##{showP candidate}' #{result status}|]
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
  EnvName ->
  ManagedState ->
  BuildMutation ->
  M (Maybe ManagedState)
buildMutation handlers builder env state BuildMutation {candidate, newVersions, accOverrides, newBounds} = do
  newOverrides <- newVersionOverrides handlers (candidate.package : newVersions)
  let overrides = newOverrides <> accOverrides
      bounds = buildBounds newBounds state.bounds
      newState = stateWithCandidate bounds candidate overrides
  logBuildInputs state candidate newVersions newOverrides accOverrides
  Log.info [exon|Building targets in '##{env}' with '#{showP candidate}'...|]
  status <- builder.buildWithState newState
  logCandidateResult candidate status
  pure (justSuccess newState status)

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
  EnvName ->
  MutationHandlers a s ->
  BuildState a s ->
  DepMutation a ->
  M (BuildState a s)
validateMutation hackage envBuilder env handlers buildState mutation = do
  result <- handlers.process buildState.ext mutation build
  logMutationResult mutation.package result
  pure (updateState buildState mutation result)
  where
    build = buildMutation hackage envBuilder env buildState.state

buildMutations ::
  Pretty a =>
  HackageHandlers ->
  Builder ->
  (EnvBuilder -> M (MutationHandlers a s)) ->
  BuildDomain ->
  [DepMutation a] ->
  BuildState a s ->
  M (BuildState a s)
buildMutations hackage Builder {withEnvBuilder} mkHandlers domain mutations state = do
  Log.debug [exon|Building targets with mutations: #{showPL mutations}|]
  withEnvBuilder domain state.state \ envBuilder -> do
    handlers <- mkHandlers envBuilder
    foldM (validateMutation hackage envBuilder domain.env handlers) state mutations
