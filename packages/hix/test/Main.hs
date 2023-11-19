module Main where

import Hix.Test.BootstrapTest (test_bootstrap)
import Hix.Test.BoundsTest (test_bounds)
import Hix.Test.CabalTest (test_cabal)
import Hix.Test.GhciTest (test_componentEnv, test_ghcid, test_moduleName)
import Hix.Test.Managed.Bump.CandidatesTest (test_candidatesBump)
import Hix.Test.Managed.Bump.MutationTest (test_bumpMutation)
import Hix.Test.Managed.LowerInit.MutationTest (test_lowerInitMutation)
import Hix.Test.Managed.LowerOptimize.CandidatesTest (test_candidatesOptimize)
import Hix.Test.Managed.LowerOptimize.MutationTest (test_lowerOptimizeMutation)
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
      unitTest "run ghcid" test_ghcid,
      unitTest "component env" test_componentEnv,
      unitTest "extract module name from path" test_moduleName,
      unitTest "generate a project" test_new,
      unitTest "bootstrap a project" test_bootstrap
    ],
    test_version,
    test_bounds,
    testGroup "managed" [
      testGroup "candidates" [
        unitTest "bump" test_candidatesBump,
        unitTest "optimize" test_candidatesOptimize
      ],
      testGroup "mutation build" [
        unitTest "bump" test_bumpMutation,
        unitTest "lower.init" test_lowerInitMutation,
        unitTest "lower.optimize" test_lowerOptimizeMutation
      ]
    ]
  ]

main :: IO ()
main =
  defaultMain tests
