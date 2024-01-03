module Hix.Test.Managed.BuildOutputTest where

import qualified Data.Aeson as Aeson
import Exon (exon)
import Hedgehog ((===))
import Test.Tasty (TestTree, testGroup)

import Hix.Managed.BuildOutput (buildOutputFromLists)
import qualified Hix.Managed.Data.BuildOutput
import Hix.Managed.Data.BuildOutput (BuildOutput, ModifiedId (ModifiedId))
import Hix.Test.Utils (UnitTest, unitTest)

output :: BuildOutput
output =
  buildOutputFromLists [
    ModifiedId {package = "direct1", version = [1, 0, 1], range = Just ">=1.1 && >0.8"},
    ModifiedId {package = "direct2", version = [1, 0, 1], range = Nothing}
  ] [
    "direct3", "direct4"
  ] [
    "direct5", "direct6"
  ]

target :: ByteString
target =
  [exon|{"failed":["direct5","direct6"],"failedNames":"direct5, direct6","modified":[{"package":"direct1","range":">=1.1","version":"1.0.1"},{"package":"direct2","range":null,"version":"1.0.1"}],"modifiedNames":"direct1, direct2","unmodified":["direct3","direct4"],"unmodifiedNames":"direct3, direct4"}|]

test_buildOutputJson :: UnitTest
test_buildOutputJson =
  target === toStrict (Aeson.encode output)

test_buildOutput :: TestTree
test_buildOutput =
  testGroup "build output formats" [
    unitTest "json" test_buildOutputJson
  ]
