module Hix.Managed.Bump.Candidates where

import Distribution.Version (Version, VersionRange, simplifyVersionRange)

import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Version (NewRange (NewRange, OldRange))
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation))
import qualified Hix.Managed.Handlers.Bump
import Hix.Managed.Handlers.Bump (BumpHandlers)
import qualified Hix.Managed.Lower.Data.Bump
import Hix.Managed.Lower.Data.Bump (Bump (Bump))
import Hix.Data.Monad (M)
import Hix.Version (amendLowerBound, currentMajor, nextMajor, requireUpperBound)

extendCurrentDep :: Version -> VersionRange -> NewRange
extendCurrentDep latest range
  | amended == simplifyVersionRange range
  = OldRange
  | otherwise
  = NewRange amended
  where
    amended = amendLowerBound (currentMajor latest) withUpper
    withUpper = requireUpperBound (nextMajor latest) range

-- | Decide whether the latest version of a dependency requires changes to the currently specified bound.
--
-- If no latest version can be found, we return 'Nothing' to indicate that the dep should be skipped.
--
-- Otherwise, the range field of the 'DepMutation' is determined:
--
-- If the specified dependency has no upper bound (e.g. because the user has only entered the package name and ran the
-- app to resolve the bounds), or if the latest version is outside of the existing bounds, then we add a new bound to
-- the managed state, so we extend the version range by setting the upper bound to the major prefix of the next-highest
-- major of the latest version and return 'NewRange'.
--
-- Otherwise, we return 'OldRange'.
--
-- Whether the project needs to be built to verify that the latest version is compatible will be decided downstream
-- based on the existing override (in the mutation handler).
bumpDep ::
  BumpHandlers ->
  Dep ->
  M (Maybe (DepMutation Bump))
bumpDep handlers dep = do
  fmap mutation <$> handlers.latestVersion package
  where
    mutation latest =
      DepMutation {package, mutation = Bump {version = latest, range = extendCurrentDep latest dep.version}}

    package = dep.package

candidatesBump ::
  BumpHandlers ->
  [Dep] ->
  M [DepMutation Bump]
candidatesBump handlers deps = do
  bumps <- traverse (bumpDep handlers) deps
  pure (catMaybes bumps)
