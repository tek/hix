module Main where

import Hix.Integration ()
import Hix.Integration.HackageTest (test_hackage)
import Hix.Integration.Managed.BumpTest (test_bump)
import Hix.Integration.Managed.SolverPackagesTest (test_solverPackages)
import Hix.Integration.ReleaseFlowTest (test_releaseFlow, test_releaseFlowInteractive, test_releaseFlowVersionChange)
import Hix.Integration.ReleaseMaintenanceTest (test_releaseMaintenance)
import Hix.Integration.ReleaseTest (test_release)
import Hix.Integration.ReleaseUiTest (
  test_releaseUi,
  test_releaseUi_colors,
  test_releaseUi_distTargets,
  test_releaseUi_versionProblems_accept,
  test_releaseUi_versionProblems_reject,
  )
import Hix.Integration.RevisionTest (test_revision)
import Hix.Test.Utils (unitTest)
import Test.Tasty (DependencyType (..), TestTree, defaultMain, sequentialTestGroup)

tests :: TestTree
tests =
  sequentialTestGroup "all" AllFinish [
    unitTest "hackage" test_hackage,
    unitTest "release maint" test_releaseMaintenance,
    unitTest "revision" test_revision,
    test_release,
    unitTest "solver packages" test_solverPackages,
    unitTest "bump" test_bump,
    unitTest "release ui" test_releaseUi,
    unitTest "release ui colors" test_releaseUi_colors,
    unitTest "release ui dist targets" test_releaseUi_distTargets,
    unitTest "release ui version problems accept" test_releaseUi_versionProblems_accept,
    unitTest "release ui version problems reject" test_releaseUi_versionProblems_reject,
    unitTest "release flow" test_releaseFlow,
    unitTest "release flow interactive" test_releaseFlowInteractive,
    unitTest "release flow version change" test_releaseFlowVersionChange
  ]

main :: IO ()
main = defaultMain tests
