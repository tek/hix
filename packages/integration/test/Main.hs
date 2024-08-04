module Main where

import Hix.Integration ()
import Hix.Integration.HackageTest (test_hackage)
import Hix.Integration.ReleaseMaintenanceTest (test_releaseMaintenance)
import Hix.Integration.RevisionTest (test_revision)
import Hix.Integration.Utils (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "hackage" test_hackage,
    unitTest "release maint" test_releaseMaintenance,
    unitTest "revision" test_revision
  ]

main :: IO ()
main = defaultMain tests
