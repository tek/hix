module Main where

import Hedgehog (TestT, property, test, withTests)
import Hix.Test.CabalTest (test_cabal)
import Hix.Test.GhcidTest (test_ghcid)
import Hix.Test.PreprocTest (
  test_preprocInsertPrelude,
  test_preprocReplacePrelude,
  test_preprocSelfExport,
  test_preprocSelfExport2,
  test_preprocSingleLineModule,
  )
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

unitTest ::
  HasCallStack =>
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc t =
  withFrozenCallStack do
    testProperty desc (withTests 1 (property (test t)))

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "parse cabal file" test_cabal,
    testGroup "preprocess source file" [
      unitTest "insert prelude import" test_preprocInsertPrelude,
      unitTest "replace prelude imports" test_preprocReplacePrelude,
      unitTest "single line module decl" test_preprocSingleLineModule,
      unitTest "self exporting module, inline" test_preprocSelfExport,
      unitTest "self exporting module, separate line" test_preprocSelfExport2,
      unitTest "run ghcid" test_ghcid
    ]
  ]

main :: IO ()
main =
  defaultMain tests
