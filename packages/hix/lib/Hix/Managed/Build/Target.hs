module Hix.Managed.Build.Target where

import Control.Monad.Trans.State.Strict (StateT (runStateT))
import Exon (exon)
import Path (Abs, Dir, Path)

import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage, PackageName)
import Hix.Data.Version (Versions)
import Hix.Managed.Build.Adapt (
  FailedPackage (..),
  FailureCounts (..),
  RetryPackage (..),
  buildAdaptive,
  failedPackageId,
  )
import Hix.Managed.Build.NixOutput.Analysis (FailureReason (..))
import Hix.Managed.Build.NixProcess (nixBuild)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.StageState (BuildResult (..), buildUnsuccessful)
import Hix.Managed.Data.Targets (Targets, firstMTargets)
import Hix.Managed.Handlers.AvailableVersions (AvailableVersionsHandlers (..))
import Hix.Managed.Handlers.SourceHash (SourceHashHandlers)
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.Overrides (packageOverrideRegular, packageOverrides, packageRevision)
import Hix.Managed.StateFile (writeBuildStateFor, writeSolverStateFor)

data BuilderResources =
  BuilderResources {
    hackage :: SourceHashHandlers,
    stateFile :: StateFileHandlers,
    versions :: AvailableVersionsHandlers,
    root :: Path Abs Dir,
    buildConfig :: BuildConfig
  }

data EnvBuilderResources =
  EnvBuilderResources {
    global :: BuilderResources,
    env :: EnvName,
    targets :: Targets,
    localUnavailable :: Set LocalPackage
  }

buildWithOverrides ::
  BuilderResources ->
  EnvName ->
  LocalPackage ->
  Overrides ->
  M BuildResult
buildWithOverrides builder env target overrides = do
  writeBuildStateFor "current build" builder.stateFile builder.root env overrides
  nixBuild builder.buildConfig builder.root [exon|env.##{env}.##{target}|]

buildSolverPackages ::
  BuilderResources ->
  EnvName ->
  Overrides ->
  M BuildResult
buildSolverPackages builder env overrides = do
  writeSolverStateFor "solver packages" builder.stateFile builder.root env overrides
  nixBuild builder.buildConfig builder.root [exon|env.##{env}.solver|]

suggestRevision ::
  BuilderResources ->
  FailureCounts ->
  FailedPackage ->
  Maybe Override ->
  FailureReason ->
  M (Maybe RetryPackage)
suggestRevision resources _ pkg = \cases
  Nothing (BoundsError _)
    | Just package <- failedPackageId pkg
    -> do
      override <- packageRevision resources.hackage [] package
      pure (Just RetryPackage {package, ..})
  _ _ -> pure Nothing

suggestNothing ::
  FailureCounts ->
  FailedPackage ->
  Maybe Override ->
  FailureReason ->
  M (Maybe a)
suggestNothing _ _ _ _ =
  pure Nothing

latestVersionFor :: BuilderResources -> PackageName -> M (Maybe RetryPackage)
latestVersionFor resources target =
  resources.versions.latest target >>= traverse \ latest -> do
    override <- packageOverrideRegular resources.hackage [] PackageId {name = target, version = latest}
    pure RetryPackage {package = PackageId {name = target, version = override.version}, ..}

-- | This might seem wrong at first glance – it immediately jailbreaks the entire package even though a newer revision
-- might relax just the right bounds and leave the rest intact.
-- However, at this point bounds are entirely useless, since a) we already incorporated proper bounds in our plan by
-- running the solver, and b) nix cannot select between different versions anyway.
suggestJailbreakAndLatestVersion ::
  BuilderResources ->
  FailureCounts ->
  FailedPackage ->
  Maybe Override ->
  FailureReason ->
  M (Maybe RetryPackage)
suggestJailbreakAndLatestVersion resources counts pkg = \cases
  Nothing (BoundsError _)
    | Just package <- failedPackageId pkg
    -> pure (Just RetryPackage {package, override = Jailbreak})
  Nothing UnknownDep -> latestVersionFor resources pkg.package
  _ (BuildError _) -> latestVersion
  _ Unclear | counts.clear == 0 -> latestVersion
  _ _ -> pure Nothing
  where
    latestVersion = latestVersionFor resources pkg.package

-- | Build an env's target packages using the given package IDs for its dependencies.
-- Return a resolved set of overrides with hashes for these versions, as well as the set of packages that have been
-- revised.
--
-- Revisions are tried for each package that fails with a bounds error, assuming that nixpkgs has an outdated revision
-- for this package, while Cabal's snapshot is more up to date.
-- In that case, building the same version from Hackage will result in the latest revision being pulled in, which might
-- have updated bounds.
--
-- While the preparation of the solver overrides allows arbitrary substitutions for broken packages, we cannot do that
-- here – the mutation algorithms rely on the property of this function that all deps are built at the specified
-- versions.
buildTargets ::
  EnvBuilderResources ->
  Bool ->
  Versions ->
  [PackageId] ->
  M (BuildResult, Overrides)
buildTargets builder allowRevisions _ overrideVersions = do
  overrides <- packageOverrides builder.global.hackage builder.localUnavailable overrideVersions
  let build target = buildAdaptive (buildWithOverrides builder.global builder.env target) suggest
      s0 = (overrides, [])
  second fst <$> runStateT (firstMTargets (BuildSuccess []) buildUnsuccessful build builder.targets) s0
  where
    suggest = if allowRevisions then suggestRevision builder.global else suggestNothing
