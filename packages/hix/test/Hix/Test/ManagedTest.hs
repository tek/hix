module Hix.Test.ManagedTest where

import Test.Tasty (TestTree, testGroup)

import Hix.Test.Managed.Bump.CandidatesTest (test_candidatesBump)
import Hix.Test.Managed.Bump.MutationTest (test_bumpMutation)
import Hix.Test.Managed.LowerInit.MutationTest (test_lowerInitMutation)
import Hix.Test.Managed.LowerOptimize.CandidatesTest (test_candidatesOptimize)
import Hix.Test.Managed.LowerOptimize.MutationTest (test_lowerOptimizeMutation)
import Hix.Test.Managed.LowerStabilize.MutationTest (test_lowerStabilizeMutation)
import Hix.Test.Utils (unitTest)

test_managed :: TestTree
test_managed =
    testGroup "managed" [
      testGroup "candidates" [
        unitTest "bump" test_candidatesBump,
        unitTest "optimize" test_candidatesOptimize
      ],
      testGroup "mutation build" [
        unitTest "bump" test_bumpMutation,
        unitTest "lower.init" test_lowerInitMutation,
        unitTest "lower.optimize" test_lowerOptimizeMutation,
        unitTest "lower.stabilize" test_lowerStabilizeMutation
      ]
    ]
