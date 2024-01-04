module Hix.Test.Managed.Bump.CandidatesTest (test_candidatesBump) where

import Distribution.Version (Version)
import Hedgehog (evalEither, (===))

import Hix.Data.PackageName (PackageName)
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Managed.Bump.Candidates (candidatesBump)
import qualified Hix.Managed.Data.Bump
import Hix.Managed.Data.Bump (Bump (Bump))
import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (DepMutation (DepMutation))
import Hix.Managed.Handlers.Bump (BumpHandlers (..), handlersNull)
import Hix.Managed.QueryDep (simpleQueryDep)
import Hix.Monad (M, clientError)
import Hix.Test.Utils (UnitTest, runMTest)

deps :: [(MutableDep, VersionBounds)]
deps =
  [
    ("dep1", "^>=2.0"),
    ("dep2", "<1.5"),
    ("dep3", ">=0"),
    ("dep5", "==1.0.1"),
    ("dep6", "<8.4"),
    ("dep7", "^>=1.1")
  ]

dep1Version :: Version
dep1Version = [2, 2, 0, 5]

dep2Version :: Version
dep2Version = [1, 7, 14]

dep3Version :: Version
dep3Version = [1, 0, 5]

dep4Version :: Version
dep4Version = [2, 4, 5]

dep5Version :: Version
dep5Version = [1, 9, 1, 0]

dep6Version :: Version
dep6Version = [7, 3]

dep7Version :: Version
dep7Version = [1, 1, 1]

latestVersion :: PackageName -> M (Maybe Version)
latestVersion =
  fmap Just . \case
    "dep1" -> pure dep1Version
    "dep2" -> pure dep2Version
    "dep3" -> pure dep3Version
    "dep4" -> pure dep4Version
    "dep5" -> pure dep5Version
    "dep6" -> pure dep6Version
    "dep7" -> pure dep7Version
    _ -> clientError "No such package"

handlersTest :: BumpHandlers
handlersTest =
  handlersNull {latestVersion}

target :: [DepMutation Bump]
target =
  [
    mkBump "dep6" dep6Bound dep6Version,
    mkBump "dep1" dep1Bound dep1Version,
    DepMutation {package = "dep7", retract = False, mutation = Bump {version = [1, 1, 1], bound = [1, 2], changed = True}},
    mkBump "dep2" dep2Bound dep2Version,
    mkBump "dep5" dep5Bound dep5Version,
    mkBump "dep3" dep3Bound dep3Version
  ]
  where
    mkBump name bound newVersion =
      DepMutation {
        package = fromString name,
        retract = False,
        mutation = Bump {
          version = newVersion,
          bound,
          changed = True
        }
      }

    dep1Bound = [2, 3]

    dep2Bound = [1, 8]

    dep3Bound = [1, 1]

    dep5Bound = [1, 10]

    dep6Bound = [7, 4]

test_candidatesBump :: UnitTest
test_candidatesBump = do
  mutations <- evalEither =<< liftIO do
    runMTest False do
      catMaybes <$> traverse (candidatesBump handlersTest) query
  sortOn (.package) target === sortOn (.package) (toList mutations)
  where
    query = uncurry simpleQueryDep <$> deps
