module Hix.Integration.ReleaseUiTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (wait, withAsync)
import qualified Data.Set as Set
import Exon (exon)
import Graphics.Vty (Key (..))
import Hedgehog ((===))

import Hix.Integration.Pty (assertTmuxTail, showTmux, tmuxCmd, tmuxLiftM, tmuxTest)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.ReleaseUi (ReleaseUi (..))
import qualified Hix.Managed.Handlers.ReleaseUi.Prod as ReleaseUi
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget (..), ReleasePlan (..))
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseTarget (..))
import Hix.Managed.Release.Data.SelectedVersion (explicitVersion, implicitVersion, keepVersion)
import Hix.Managed.Release.Data.Staged (SelectedTargetView (..))
import Hix.Managed.Release.Data.VersionChoice (toReleasePlan)
import Hix.Managed.Release.Validation (ProblematicVersion (..), VersionProblem (..))
import Hix.Test.Utils (UnitTest)
import Hix.Ui.Debug (brickDebug, sendChar, sendKey)

failRelease :: HasCallStack => Either a b -> b
failRelease = \case
  Right a -> a
  Left _ -> withFrozenCallStack $ error "Expected Right"

packagesC :: Packages ConfiguredTarget
packagesC =
  [
    ("loca1", ConfiguredTarget {current = "1.0.1", version = Nothing, selected = True}),
    ("local2", ConfiguredTarget {current = "1.0.2", version = Nothing, selected = True}),
    ("localp3", ConfiguredTarget {current = "1.0.3", version = Nothing, selected = True}),
    ("localpk4", ConfiguredTarget {current = "1.0.4", version = Nothing, selected = True}),
    ("localpkg5", ConfiguredTarget {current = "1", version = Nothing, selected = True})
  ]

targetP :: Packages ReleasePlan
targetP =
  [
    ("loca1", NotPlanned (ConfiguredTarget {current = "1.0.1", version = Nothing, selected = False})),
    ("local2", Planned (ReleaseTarget {package = "local2", current = "1.0.2", version = keepVersion "1.0.2"})),
    ("localp3", NotPlanned (ConfiguredTarget {current = "1.0.3", version = Nothing, selected = False})),
    ("localpk4", Planned (ReleaseTarget {package = "localpk4", current = "1.0.4", version = keepVersion "1.0.4"})),
    ("localpkg5", Planned (ReleaseTarget {package = "localpkg5", current = "1", version = keepVersion "1"}))
  ]

steps :: [Char]
steps =
  [
    'j',
    'l',
    'i',
    'd',
    'b',
    's',
    't',
    'j',
    'j',
    't'
  ]

targetT :: Text
targetT =
  [exon|[38;2;88;110;117m[48;2;0;43;54m┌──────────────────────────────────┐[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;55;60;150m██ [38;2;147;161;161mChoose release versions[38;2;131;148;150m        [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;131;148;150m                                  [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│○[38;2;131;148;150m [38;2;88;110;117mAll packages[38;2;131;148;150m [38;2;88;110;117m1.1.0[38;2;131;148;150m              [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;131;148;150m                                  [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;220;50;47m○[38;2;131;148;150m loca1        [38;2;181;137;0m1[38;2;131;148;150m.[38;2;181;137;0m1[38;2;131;148;150m.[38;2;181;137;0m0[38;2;131;148;150m              [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;133;153;0m○[38;2;131;148;150m local2       [38;2;181;137;0m1[38;2;131;148;150m.[38;2;181;137;0m0[38;2;131;148;150m.[38;2;181;137;0m2[38;2;131;148;150m        [38;2;88;110;117mbump[38;2;131;148;150m [38;2;181;137;0mb[38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[1m[38;2;220;50;47m●[38;2;131;148;150m localp3 [0m[38;2;131;148;150m[48;2;0;43;54m     [1m[38;2;181;137;0m1[38;2;131;148;150m.[38;2;181;137;0m[48;2;55;60;150m0[38;2;131;148;150m[48;2;0;43;54m.[38;2;181;137;0m3[38;2;131;148;150m [0m[38;2;131;148;150m[48;2;0;43;54m     [38;2;88;110;117mshared[38;2;131;148;150m [38;2;181;137;0ms[38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;133;153;0m○[38;2;131;148;150m localpk4     [38;2;181;137;0m1[38;2;131;148;150m.[38;2;181;137;0m0[38;2;131;148;150m.[38;2;181;137;0m4[38;2;131;148;150m        [38;2;88;110;117mquit[38;2;131;148;150m [38;2;181;137;0mq[38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;133;153;0m○[38;2;131;148;150m localpkg5    [38;2;181;137;0m1[38;2;131;148;150m            [38;2;88;110;117mhelp[38;2;131;148;150m [38;2;181;137;0m?[38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m└──────────────────────────────────┘[39m[49m
|]

-- TODO resize doesn't work
test_releaseUi :: UnitTest
test_releaseUi = do
  debug <- brickDebug
  let ui = ReleaseUi.handlersProdWithEvents (Just debug)
  result <- tmuxTest True (if testResize then 20 else 12) do
    when testResize do
      tmuxCmd ["split-window", "-v", "-d"]
      tmuxCmd ["split-window", "-h", "-d"]
      void $ tmuxCmd ["resize-pane", "-x", "60", "-y", "8"]
    withAsync (tmuxLiftM $ ui.chooseVersions (Just (explicitVersion "1.1.0")) packagesC) \ asyncResult -> do
      when testResize do
        tmuxCmd ["resize-pane", "-y", "5"]
        liftIO $ threadDelay 1_000_000
        showTmux
      for_ steps \ c -> do
        sendChar debug c
        liftIO $ putStrLn ([exon|sent '#{[c]}'|])
        showTmux
      sendKey debug KEnter
      assertTmuxTail True targetT
      wait asyncResult
  targetP === toReleasePlan (failRelease result)
    where
      testResize = False

test_help :: UnitTest
test_help = do
  debug <- brickDebug
  let ui = ReleaseUi.handlersProdWithEvents (Just debug)
  void $ tmuxTest True 40 do
    withAsync (tmuxLiftM $ ui.chooseVersions (Just (explicitVersion "1.1.0")) packagesC) \ asyncResult -> do
      sendChar debug '?'
      showTmux
      sendChar debug '?'
      showTmux
      sendKey debug KEnter
      wait asyncResult

colorsTarget :: Text
colorsTarget = [exon|[38;2;88;110;117m[48;2;0;43;54m┌──────────────────────────────────┐[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;55;60;150m██ [38;2;147;161;161mChoose release versions[38;2;131;148;150m        [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;131;148;150m                                  [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[1m[38;2;133;153;0m●[38;2;131;148;150m All packages [38;2;181;137;0m1[38;2;131;148;150m.[38;2;181;137;0m[48;2;55;60;150m1[38;2;131;148;150m[48;2;0;43;54m.[38;2;181;137;0m0[38;2;131;148;150m [0m[38;2;131;148;150m[48;2;0;43;54m             [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│[38;2;131;148;150m                                  [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│○[38;2;131;148;150m [38;2;88;110;117mloca1[38;2;131;148;150m        [38;2;88;110;117m1.0.1[38;2;131;148;150m              [38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│○[38;2;131;148;150m [38;2;88;110;117mlocal2[38;2;131;148;150m       [38;2;88;110;117m1.0.2[38;2;131;148;150m        [38;2;88;110;117mbump[38;2;131;148;150m [38;2;181;137;0mb[38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│○[38;2;131;148;150m [38;2;88;110;117mlocalp3[38;2;131;148;150m      [38;2;88;110;117m1.0.3[38;2;131;148;150m      [38;2;88;110;117mshared[38;2;131;148;150m [38;2;181;137;0ms[38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│○[38;2;131;148;150m [38;2;88;110;117mlocalpk4[38;2;131;148;150m     [38;2;88;110;117m1.0.4[38;2;131;148;150m        [38;2;88;110;117mquit[38;2;131;148;150m [38;2;181;137;0mq[38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m│○[38;2;131;148;150m [38;2;88;110;117mlocalpkg5[38;2;131;148;150m    [38;2;88;110;117m1[38;2;131;148;150m            [38;2;88;110;117mhelp[38;2;131;148;150m [38;2;181;137;0m?[38;2;88;110;117m│[39m[49m
[38;2;88;110;117m[48;2;0;43;54m└──────────────────────────────────┘[39m[49m
|]

test_releaseUi_colors :: UnitTest
test_releaseUi_colors = do
  debug <- brickDebug
  let ui = ReleaseUi.handlersProdWithEvents (Just debug)
  void $ tmuxTest True 16 do
    withAsync (tmuxLiftM $ ui.chooseVersions (Just (explicitVersion "1.1.0")) packagesC) \ asyncResult -> do
      sendChar debug 'l'
      showTmux
      assertTmuxTail True colorsTarget
      sendKey debug KEnter
      wait asyncResult

-- | Packages with SelectedTargetView for dist targets test
packagesDist :: Packages SelectedTargetView
packagesDist =
  [
    ("local1", SelectedTargetView {package = "local1", current = "1.0.1", releaseVersion = "1.0.2", explicit = True, keep = False}),
    ("local2", SelectedTargetView {package = "local2", current = "1.0.2", releaseVersion = "1.0.3", explicit = True, keep = False})
  ]

test_releaseUi_distTargets :: UnitTest
test_releaseUi_distTargets = do
  debug <- brickDebug
  let ui = ReleaseUi.handlersProdWithEvents (Just debug)
  result <- tmuxTest True 8 do
    withAsync (tmuxLiftM $ ui.chooseDistTargets True packagesDist) \ asyncResult -> do
      -- UI shows: local1, local2 (both selected targets)
      -- Initial state: both local1 and local2 are selected
      -- Toggle off local1, keep local2 selected
      sendChar debug 't'  -- toggle local1 off
      showTmux
      sendKey debug KEnter
      wait asyncResult
  Right (Set.fromList ["local2"]) === result

-- | Problematic versions for version problems UI test
problemsPackages :: Packages ProblematicVersion
problemsPackages =
  [
    ("local1", ProblematicVersion {package = "local1", current = "1.0.0", selected = implicitVersion "1.0.1", problem = NoVersionSpecified}),
    ("local2", ProblematicVersion {package = "local2", current = "1.0.0", selected = implicitVersion "1.0.0", problem = SameAsCurrent})
  ]

-- | Test version problems UI: accept (press 'y')
test_releaseUi_versionProblems_accept :: UnitTest
test_releaseUi_versionProblems_accept = do
  debug <- brickDebug
  let ui = ReleaseUi.handlersProdWithEvents (Just debug)
  result <- tmuxTest True 8 do
    withAsync (tmuxLiftM $ ui.chooseVersionProblems problemsPackages) \ asyncResult -> do
      showTmux
      sendChar debug 'y'  -- accept
      wait asyncResult
  Right True === result

-- | Test version problems UI: reject (press 'n')
test_releaseUi_versionProblems_reject :: UnitTest
test_releaseUi_versionProblems_reject = do
  debug <- brickDebug
  let ui = ReleaseUi.handlersProdWithEvents (Just debug)
  result <- tmuxTest True 8 do
    withAsync (tmuxLiftM $ ui.chooseVersionProblems problemsPackages) \ asyncResult -> do
      showTmux
      sendChar debug 'n'  -- reject
      wait asyncResult
  Right False === result

-- | Test quit by pressing 'q' — should return Left TerminateFlow
test_releaseUi_quit :: UnitTest
test_releaseUi_quit = do
  debug <- brickDebug
  let ui = ReleaseUi.handlersProdWithEvents (Just debug)
  result <- tmuxTest True 8 do
    withAsync (tmuxLiftM $ ui.chooseVersions (Just (explicitVersion "1.1.0")) packagesC) \ asyncResult -> do
      showTmux
      sendChar debug 'q'
      wait asyncResult
  True === isLeft result
