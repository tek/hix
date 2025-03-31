module Main where

import Hix ()
import Hix.Test.BootstrapTest (test_bootstrap)
import Hix.Test.BoundsTest (test_bounds)
import Hix.Test.CabalTest (test_cabal)
import Hix.Test.GhciTest (test_ghci)
import Hix.Test.ManagedTest (test_managed)
import Hix.Test.NewTest (test_new)
import Hix.Test.Optparse (test_absPathOrCwd)
import Hix.Test.PreprocTest (
  test_preprocInsertPrelude,
  test_preprocNoPrelude,
  test_preprocPreludePrefix,
  test_preprocReplacePrelude,
  test_preprocSelfExport,
  test_preprocSelfExport2,
  test_preprocSingleLineModule,
  )
import Hix.Test.Utils (unitTest)
import Hix.Test.VersionTest (test_version)
import Test.Tasty (TestTree, defaultMain, testGroup)

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
      test_ghci,
      unitTest "generate a project" test_new,
      unitTest "bootstrap a project" test_bootstrap
    ],
    test_version,
    test_bounds,
    test_managed,
    unitTest "path option parser" test_absPathOrCwd
  ]

main :: IO ()
main = defaultMain tests
