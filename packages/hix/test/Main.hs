module Main where

import Hedgehog (TestT, property, test, withTests)
import Hix.Test.BootstrapTest (test_bootstrap)
import Hix.Test.BumpTest (test_bumpBuild, test_bumpVersions)
import Hix.Test.CabalTest (test_cabal)
import Hix.Test.GhciTest (test_componentEnv, test_ghcid, test_moduleName)
import Hix.Test.NewTest (test_new)
import Hix.Test.PreprocTest (
  test_preprocInsertPrelude,
  test_preprocNoPrelude,
  test_preprocPreludePrefix,
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
      unitTest "self exporting module, separate line" test_preprocNoPrelude,
      unitTest "modules starting with 'Prelude'" test_preprocPreludePrefix,
      unitTest "run ghcid" test_ghcid,
      unitTest "component env" test_componentEnv,
      unitTest "extract module name from path" test_moduleName,
      unitTest "generate a project" test_new,
      unitTest "bootstrap a project" test_bootstrap
    ],
    testGroup "bump" [
      unitTest "versions" test_bumpVersions,
      unitTest "build" test_bumpBuild
    ]
  ]

main :: IO ()
main =
  defaultMain tests
