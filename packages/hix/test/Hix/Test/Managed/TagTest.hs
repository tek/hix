module Hix.Test.Managed.TagTest where

import Distribution.Parsec (eitherParsec)
import Hedgehog ((===))
import Test.Tasty (TestTree, testGroup)

import Hix.Managed.Git (Tag (..))
import Hix.Managed.Maint.Prep (TagPrio (..))
import Hix.Test.Utils (UnitTest, unitTest)

test_parseTag :: UnitTest
test_parseTag = do
  Right (Tag (Just "pack") [1, 2, 3]) === eitherParsec "pack-1.2.3"
  Right (Tag Nothing [1, 2, 3]) === eitherParsec "1.2.3"

test_ordTag :: UnitTest
test_ordTag =
  target === sortOn TagPrio input
  where
    target =
      [
        t p0 v1,
        t p1 v1,
        t p2 v1,
        t p0 v2,
        t p1 v2,
        t p2 v2,
        t p0 v3,
        t p2 v3
      ]

    input =
      [
        t p2 v3,
        t p2 v1,
        t p0 v3,
        t p1 v2,
        t p0 v1,
        t p2 v2,
        t p0 v2,
        t p1 v1
      ]

    t package version = Tag {package, version}

    p0 = Nothing
    p1 = Just "p1"
    p2 = Just "p2"
    v1 = "1.0.0"
    v2 = "1.1.0"
    v3 = "2.0.0"

test_tag :: TestTree
test_tag =
  testGroup "git tags" [
    unitTest "parse" test_parseTag,
    unitTest "order" test_ordTag
  ]
