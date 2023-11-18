module Hix.Test.Managed.Bump.CandidatesTest (test_candidatesBump) where

import Data.Aeson (eitherDecodeStrict')
import Distribution.Version (Version, earlierVersion, intersectVersionRanges, orLaterVersion, unionVersionRanges)
import Exon (exon)
import Hedgehog (evalEither, (===))
import Path (Abs, Dir, Path, absdir)

import Hix.Data.ConfigDeps (ConfigDeps)
import Hix.Data.Error (Error (Client))
import Hix.Data.Package (PackageName)
import Hix.Data.Version (NewRange (NewRange, OldRange))
import Hix.Deps (allDeps, depsFromConfig, forTargets)
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation))
import Hix.Managed.Bump.Candidates (candidatesBump)
import Hix.Managed.Handlers.Build (BuildHandlers (..), withTempProject)
import Hix.Managed.Handlers.Build.Test (withTempProjectAt)
import Hix.Managed.Handlers.Bump (BumpHandlers (..), handlersNull)
import qualified Hix.Managed.Lower.Data.Bump
import Hix.Managed.Lower.Data.Bump (Bump (Bump))
import Hix.Monad (M, runM, throwM)
import Hix.Test.Utils (UnitTest)

root :: Path Abs Dir
root = [absdir|/project|]

tmpRoot :: Path Abs Dir
tmpRoot = [absdir|/tmp/project|]

depsConfig :: Either String ConfigDeps
depsConfig =
  eitherDecodeStrict' [exon|{
    "panda": {
      "library": {
        "dependencies": [
          "dep1 ^>= 2.0",
          {
            "name": "dep2",
            "version": "< 1.5"
          },
          "dep3",
          "dep4 (>= 0.1 && < 0.3) || (>= 1.1 && < 1.2.3) || >= 2.3",
          "dep5 ==1.0.1",
          "dep6 < 8.4",
          "dep7 ^>= 1.1"
        ]
      }
    }
  }|]

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
    _ -> throwM (Client "No such package")

handlersTest :: BumpHandlers
handlersTest =
  h {
    build = h.build {withTempProject = withTempProjectAt tmpRoot}
  }
  where
    h = handlersNull {latestVersion}

target :: [DepMutation Bump]
target =
  [
    mkBump "dep6" dep6Range dep6Version,
    mkBump "dep1" dep1Range dep1Version,
    DepMutation {package = "dep7", mutation = Bump {version = [1, 1, 1], range = OldRange}},
    mkBump "dep4" dep4Range dep4Version,
    mkBump "dep2" dep2Range dep2Version,
    mkBump "dep5" dep5Range dep5Version,
    mkBump "dep3" dep3Range dep3Version
  ]
  where
    mkBump name range newVersion =
      DepMutation {
        package = fromString name,
        mutation = Bump {
          version = newVersion,
          range = NewRange range
        }
      }

    dep1Range = between [2, 0] [2, 3]

    dep2Range = between [1, 7] [1, 8]

    dep3Range = between [1, 0] [1, 1]

    dep4Range =
      foldr1 @NonEmpty unionVersionRanges [
        (between [0, 1] [0, 3]),
        (between [1, 1] [1, 2, 3]),
        (between [2, 3] [2, 5])
      ]

    dep5Range = between [1, 0, 1] [1, 10]

    dep6Range = between [7, 3] [7, 4]

    between l r = intersectVersionRanges (orLaterVersion l) (earlierVersion r)

test_candidatesBump :: UnitTest
test_candidatesBump = do
  cdeps <- leftA fail depsConfig
  bumps <- evalEither =<< liftIO do
    runM root do
      configDeps <- depsFromConfig cdeps ["panda"]
      let targetDeps = forTargets "panda" configDeps
      let deps = allDeps targetDeps
      candidatesBump handlersTest deps
  sortOn (.package) target === sortOn (.package) (toList bumps)
