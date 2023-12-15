module Hix.Test.Managed.ConfigDepsTest where

import Data.Aeson (eitherDecodeStrict')
import qualified Distribution.Compat.NonEmptySet as NonEmptySet
import Distribution.PackageDescription (LibraryName (LSubLibName))
import Distribution.Version (earlierVersion, intersectVersionRanges, majorBoundVersion, orLaterVersion, thisVersion)
import Exon (exon)
import Hedgehog (evalEither, (===))

import Hix.Data.ConfigDeps (ConfigDeps)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep (Dep), mainDep)
import Hix.Data.Version (range0)
import Hix.Test.Utils (UnitTest)

json :: ByteString
json =
  [exon|{
    "panda": {
      "library": {
        "dependencies": [
          "direct1 ^>=2.0",
          {
            "name": "direct2",
            "version": "<1.5"
          },
          "direct3",
          "direct4:{internal,external} ==13.23",
          "direct5 >=5.0 && <5.1"
        ]
      }
    }
  }|]

target :: ConfigDeps
target =
  [
    ("panda", [("library", [
      mainDep "direct1" (majorBoundVersion [2, 0]),
      mainDep "direct2" (earlierVersion [1, 5]),
      mainDep "direct3" range0,
      Dep {
        package = "direct4",
        version = thisVersion [13, 23],
        libs = NonEmptySet.fromNonEmpty [LSubLibName "internal", LSubLibName "external"]
      },
      mainDep "direct5" (intersectVersionRanges (orLaterVersion [5, 0]) (earlierVersion [5, 1]))
    ])])
  ]

test_parseConfigDeps :: UnitTest
test_parseConfigDeps = do
  parsed <- evalEither (eitherDecodeStrict' json)
  target === parsed
