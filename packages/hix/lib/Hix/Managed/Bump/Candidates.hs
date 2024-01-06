module Hix.Managed.Bump.Candidates where

import Distribution.Version (Version)

import Hix.Data.Monad (M)
import qualified Hix.Managed.Data.Bump
import Hix.Managed.Data.Bump (Bump (Bump))
import Hix.Managed.Data.Mutable (depName)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (DepMutation (DepMutation))
import qualified Hix.Managed.Data.QueryDep
import Hix.Managed.Data.QueryDep (QueryDep (QueryDep))
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Version (nextMajor)

-- | We only want to report a bump if the new version actually changes the build.
isBump :: Version -> Maybe Version -> Bool
isBump version = \case
  Just current -> version > current
  Nothing -> True

-- | Decide whether the latest version of a dependency requires changes to the currently specified bound.
--
-- If no latest version can be found, we return 'Nothing' to indicate that the dep should be skipped.
--
-- Otherwise, the range field of the 'DepMutation' is determined:
--
-- If the specified dependency has no upper bound (e.g. because the user has only entered the package name and ran the
-- app to resolve the bounds), or if the latest version is outside of the existing bounds, then we add a new bound to
-- the managed state, so we extend the version range by setting the upper bound to the major prefix of the next-highest
-- major of the latest version and return 'NewBounds'.
--
-- Otherwise, we return 'OldBounds'.
candidatesBump ::
  BuildHandlers ->
  QueryDep ->
  M (Maybe (DepMutation Bump))
candidatesBump handlers QueryDep {package, current} = do
  fmap mutation <$> handlers.latestVersion (depName package)
  where
    mutation version =
      DepMutation {
        package,
        retract = False,
        mutation = Bump {version, bound = nextMajor version, changed = isBump version current}
      }
