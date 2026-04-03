module Hix.Integration.ReleaseFlowTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (withAsync)
import Data.Text qualified as Text
import Exon (exon)
import GHC.IsList (fromList)
import Graphics.Vty.Input.Events (Key (..))
import Hedgehog (TestT)
import Path (reldir)

import Hix.Data.Monad (M)
import Hix.Data.VersionBounds (Bound (BoundUpper))
import Hix.Integration.Pty (TmuxTest, assertTmuxTail, capturePlain, tmuxLiftM, tmuxTest)
import Hix.Managed.Cabal.ContextHackageRepo (unsafeCentralHackageContextFixed)
import Hix.Managed.Cabal.Data.HackageRepo (HackageDescription (..))
import Hix.Managed.Data.EnvConfig (EnvConfig (..))
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..), ReleaseVersion (..), bothArtifacts)
import Hix.Managed.Data.ReleaseContext (ReleaseContextProto (..), ReleasePackage (..))
import Hix.Managed.Data.StateVersionsContext (StateVersionsContext (..))
import Hix.Managed.Data.VersionIncrement (VersionIncrement (..))
import qualified Hix.Managed.Handlers.Context as ContextHandlers
import Hix.Managed.Handlers.HackageClient (HackageClient (..))
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import Hix.Managed.Handlers.Release (ReleaseHandlers (..))
import Hix.Managed.Handlers.Release.Test (
  ReleaseTestConfig (..),
  ReleaseTestData (..),
  defaultTestConfig,
  mkReleaseTestData,
  releaseDistUnitTest,
  runChecksUnitTest,
  uploadArtifactUnitTest,
  )
import qualified Hix.Managed.Handlers.ReleaseUi.Prod as ReleaseUi
import Hix.Managed.Handlers.Report (ReportHandlers (..))
import qualified Hix.Managed.Handlers.StateFile as StateFile
import Hix.Managed.Handlers.Upload (UploadHandlers (..))
import Hix.Managed.Release (release)
import Hix.Managed.Release.Git (gitApiReleaseUnitTest)
import Hix.Ui.Debug (BrickDebug, brickDebug, sendKey, waitForApp)

-- | A single step in the release flow test.
--   Each step corresponds to one Brick UI screen.
data FlowStep =
  FlowStep {
    -- | Human-readable label for diagnostics.
    label :: Text,
    -- | Expected plain-text pane content (no escape sequences).
    --   Only the tail of the pane is matched, so leading terminal setup lines are ignored.
    expected :: Text,
    -- | Keys to send after asserting the screen. Last key should confirm (usually KEnter).
    keys :: [Key]
  }

-- | Construct mock 'ReleaseHandlers' for tmux UI flow testing.
--   Uses unit test implementations for all IO operations but real Brick UI.
handlersFlowTest ::
  ReleaseTestConfig ->
  BrickDebug ->
  M ReleaseHandlers
handlersFlowTest testConfig debug = do
  testData <- liftIO mkReleaseTestData
  pure ReleaseHandlers {
    runChecks = runChecksUnitTest testData.checksRun testConfig.checksPass,
    releaseDist = releaseDistUnitTest,
    uploadArtifact = uploadArtifactUnitTest testConfig testData.uploadedArtifacts,
    git = const gitApiReleaseUnitTest,
    context = contextHandlers,
    project = ProjectHandlers {
      stateFile = StateFile.handlersNull,
      report = ReportHandlers {mutations = \_ -> unit}
    },
    upload = UploadHandlers {upload = \_ _ -> error "upload: not used in flow test"},
    publishHackages = pure HackageClient {
      description = HackageDescription "mock",
      request = \_ -> error "publishHackages: not used in flow test"
    },
    ui = ReleaseUi.handlersProdWithEvents (Just debug)
  }
  where
    contextHandlers = ContextHandlers.handlersTest \case
      ContextHandlers.ContextQuery ContextHandlers.ContextManaged -> pure (Just managedContext)
      ContextHandlers.ContextQuery ContextHandlers.ContextStateVersions -> pure (Just stateVersionsContext)
      _ -> pure Nothing

    stateVersionsContext = StateVersionsContext {
      state = def,
      versionFile = Nothing,
      packages = mempty
    }

    managedContext = def {
      envs = fromList [("test", envConfig)]
    }

    envConfig = EnvConfig {
      targets = [],
      ghc = Nothing,
      managedBound = Just BoundUpper
    }

-- | Release config for the flow test: interactive with both artifacts and major version bump.
flowTestConfig :: ReleaseConfig
flowTestConfig =
  def {
    publish = bothArtifacts,
    targets = Just ["local1", "local2"],
    version = Just (VersionIncrement Major),
    interactive = True
  }

-- | Context proto for the flow test: two packages at known versions.
flowTestContext :: ReleaseContextProto
flowTestContext =
  ReleaseContextProto {
    packages = [
      ("local1", ReleasePackage {name = "local1", version = [1, 1, 1], path = [reldir|local1|]}),
      ("local2", ReleasePackage {name = "local2", version = [2, 2, 2], path = [reldir|local2|]})
    ],
    hackage = [unsafeCentralHackageContextFixed],
    hooks = [],
    commitExtraArgs = [],
    tagExtraArgs = [],
    managed = True
  }

versionBox :: Text
versionBox =
  [exon|┌──────────────────────────────────┐
│██ Choose release versions        │
│                                  │
│● All packages 0            bump b│
│                          shared s│
│○ local1       1.1.1        quit q│
│○ local2       2.2.2        help ?│
└──────────────────────────────────┘
|]

distBox :: Text
distBox =
  [exon|│○ local1       1.1.1        quit q│
│○ local2       2.2.2        help ?│
└──────────────────────────────────┘
┌────────────────────────────────────────────────────────────────────┐
│██ Select packages for which to create release distributions        │
│                                                                    │
│Flake checks passed.                                        toggle t│
│● local1 1.1.1                                                quit q│
│○ local2 2.2.2                                                help ?│
└────────────────────────────────────────────────────────────────────┘
|]

uploadBoxWidth :: Text -> Int
uploadBoxWidth title =
  Text.length title + 42

uploadBox :: Text -> Text -> Text -> [Text] -> [Text]
uploadBox title v1 v2 bottom =
  [
    "│██ Select packages for which to " <> title <> Text.replicate 8 " " <> "│",
    "│" <> Text.replicate (iw - 8) " " <> "toggle t│",
    "│● local1 " <> v1 <> Text.replicate (iw - 15 - Text.length v1) " " <> "quit q│",
    "│○ local2 " <> v2 <> Text.replicate (iw - 15 - Text.length v2) " " <> "help ?│"
  ] <> bottom
  where
    iw = uploadBoxWidth title - 2

-- | The last 3 lines of a closed upload box for use as context above the next box.
uploadBoxContext :: Text -> Text -> Text -> [Text]
uploadBoxContext title v1 v2 =
  drop 3 (uploadBoxClosed title v1 v2)

uploadBoxClosed :: Text -> Text -> Text -> [Text]
uploadBoxClosed title v1 v2 =
  ["┌" <> Text.replicate (uploadBoxWidth title - 2) "─" <> "┐"]
  <> uploadBox title v1 v2
    ["└" <> Text.replicate (uploadBoxWidth title - 2) "─" <> "┘"]

-- | Single-package upload box content (only local1, no local2).
-- With 1 package, the content is 3 lines tall, equal to the key mappings height,
-- so there's no padding: key mappings start at line 0.
uploadBox1 :: Text -> Text -> [Text] -> [Text]
uploadBox1 title v1 bottom =
  [
    "│██ Select packages for which to " <> title <> "toggle t│",
    "│" <> Text.replicate (iw - 8) " " <> "  quit q│",
    "│● local1 " <> v1 <> Text.replicate (iw - 17 - Text.length v1) " " <> "  help ?│"
  ] <> bottom
  where
    iw = uploadBoxWidth title - 2

uploadBox1Context :: Text -> Text -> [Text]
uploadBox1Context title v1 =
  drop 2 (uploadBox1Closed title v1)

uploadBox1Closed :: Text -> Text -> [Text]
uploadBox1Closed title v1 =
  ["┌" <> Text.replicate (uploadBoxWidth title - 2) "─" <> "┐"]
  <> uploadBox1 title v1
    ["└" <> Text.replicate (uploadBoxWidth title - 2) "─" <> "┘"]

-- | Prepend context lines above an upload box for tail-matching assertions.
withContext :: [Text] -> [Text] -> Text
withContext context box =
  Text.unlines (context <> box)

-- | Steps for the basic release flow: press Enter at each screen to accept defaults.
basicFlowSteps :: [FlowStep]
basicFlowSteps =
  [
    FlowStep {
      label = "version-selection",
      expected = versionBox,
      keys = [KEnter]
    },
    FlowStep {
      label = "dist-targets",
      expected = distBox,
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-candidates-sources",
      expected = withContext
        ["│● local1 1.1.1                                                quit q│",
         "│○ local2 2.2.2                                                help ?│",
         "└────────────────────────────────────────────────────────────────────┘"]
        (uploadBoxClosed "upload candidates for sources" "1.1.1" "2.2.2"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-candidates-docs",
      expected = withContext
        (uploadBoxContext "upload candidates for sources" "1.1.1" "2.2.2")
        (uploadBoxClosed "upload candidates for docs" "1.1.1" "2.2.2"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-publish-sources",
      expected = withContext
        (uploadBoxContext "upload candidates for docs" "1.1.1" "2.2.2")
        (uploadBoxClosed "publish sources" "1.1.1" "2.2.2"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-publish-docs",
      expected = withContext
        (uploadBoxContext "publish sources" "1.1.1" "2.2.2")
        (uploadBoxClosed "publish docs" "1.1.1" "2.2.2"),
      keys = [KEnter]
    }
  ]


-- | Synchronize with the Brick event loop by sending an unhandled key.
--   After 'waitForApp', the Brick app has started but may not have completed its first render.
--   Sending an unhandled key forces a full render-read-handle cycle, ensuring the pane content
--   is up to date before capture.
syncRender :: BrickDebug -> TmuxTest M ()
syncRender debug =
  tmuxLiftM (sendKey debug (KFun 12))

-- | Execute one step of the release flow: wait for the Brick app, sync render, assert, send keys.
runStep :: BrickDebug -> FlowStep -> TmuxTest M ()
runStep debug step = do
  tmuxLiftM (waitForApp debug)
  syncRender debug
  when (step.expected /= "")
    (assertTmuxTail False step.expected)
  for_ step.keys \ key ->
    tmuxLiftM (sendKey debug key)

-- | Run the full release flow with Brick UIs, asserting pane contents at each step.
releaseFlowTest :: [FlowStep] -> TmuxTest M ()
releaseFlowTest steps = do
  debug <- tmuxLiftM brickDebug
  handlers <- tmuxLiftM (handlersFlowTest defaultTestConfig debug)
  withAsync (tmuxLiftM (void (release handlers flowTestConfig flowTestContext))) \ _ ->
    for_ steps (runStep debug)

test_releaseFlow :: TestT IO ()
test_releaseFlow =
  tmuxTest True 10 (releaseFlowTest basicFlowSteps)

-- | Interactive flow: toggle local2 off in dist-targets, accept everything else.
interactiveFlowSteps :: [FlowStep]
interactiveFlowSteps =
  [
    FlowStep {
      label = "version-selection (interactive)",
      expected = versionBox,
      keys = [KEnter]
    },
    FlowStep {
      label = "dist-targets (interactive)",
      expected = distBox,
      keys = [KChar 'j', KChar 't', KEnter]
    },
    FlowStep {
      label = "upload-candidates-sources (interactive)",
      expected = withContext
        ["│○ local1 1.1.1                                                quit q│",
         "│● local2 2.2.2                                                help ?│",
         "└────────────────────────────────────────────────────────────────────┘"]
        (uploadBox1Closed "upload candidates for sources" "1.1.1"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-candidates-docs (interactive)",
      expected = withContext
        (uploadBox1Context "upload candidates for sources" "1.1.1")
        (uploadBox1Closed "upload candidates for docs" "1.1.1"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-publish-sources (interactive)",
      expected = withContext
        (uploadBox1Context "upload candidates for docs" "1.1.1")
        (uploadBox1Closed "publish sources" "1.1.1"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-publish-docs (interactive)",
      expected = withContext
        (uploadBox1Context "publish sources" "1.1.1")
        (uploadBox1Closed "publish docs" "1.1.1"),
      keys = [KEnter]
    }
  ]

test_releaseFlowInteractive :: TestT IO ()
test_releaseFlowInteractive =
  tmuxTest True 10 (releaseFlowTest interactiveFlowSteps)

-- | Version change flow: bump local1's minor version and increment patch.
-- Initial: local1 = 1.1.1, local2 = 2.2.2
-- After j,l,b,l,i: local1 = 1.2.1
versionChangeDistBox :: Text
versionChangeDistBox =
  [exon|│● local1       1.2.1        quit q│
│○ local2       2.2.2        help ?│
└──────────────────────────────────┘
┌────────────────────────────────────────────────────────────────────┐
│██ Select packages for which to create release distributions        │
│                                                                    │
│Flake checks passed.                                        toggle t│
│● local1 1.2.1                                                quit q│
│○ local2 2.2.2                                                help ?│
└────────────────────────────────────────────────────────────────────┘
|]

versionChangeSteps :: [FlowStep]
versionChangeSteps =
  [
    FlowStep {
      label = "version-selection (bump local1)",
      expected = versionBox,
      -- j: select local1, l: move to version, b: bump minor (1.1.1 -> 1.2.0), l: move to patch, i: increment (1.2.0 -> 1.2.1)
      keys = [KChar 'j', KChar 'l', KChar 'b', KChar 'l', KChar 'i', KEnter]
    },
    FlowStep {
      label = "dist-targets (version changed)",
      expected = versionChangeDistBox,
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-candidates-sources",
      expected = withContext
        ["│● local1 1.2.1                                                quit q│",
         "│○ local2 2.2.2                                                help ?│",
         "└────────────────────────────────────────────────────────────────────┘"]
        (uploadBoxClosed "upload candidates for sources" "1.2.1" "2.2.2"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-candidates-docs",
      expected = withContext
        (uploadBoxContext "upload candidates for sources" "1.2.1" "2.2.2")
        (uploadBoxClosed "upload candidates for docs" "1.2.1" "2.2.2"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-publish-sources",
      expected = withContext
        (uploadBoxContext "upload candidates for docs" "1.2.1" "2.2.2")
        (uploadBoxClosed "publish sources" "1.2.1" "2.2.2"),
      keys = [KEnter]
    },
    FlowStep {
      label = "upload-publish-docs",
      expected = withContext
        (uploadBoxContext "publish sources" "1.2.1" "2.2.2")
        (uploadBoxClosed "publish docs" "1.2.1" "2.2.2"),
      keys = [KEnter]
    }
  ]

test_releaseFlowVersionChange :: TestT IO ()
test_releaseFlowVersionChange =
  tmuxTest True 10 (releaseFlowTest versionChangeSteps)

-- | Poll the tmux pane for up to @maxAttempts@ iterations, checking that the tail matches @target@.
-- On each iteration, captures the pane and checks if the expected lines match the tail of the actual lines.
-- Retries after a short delay if the pane is empty or doesn't match yet.
-- After exhausting retries, performs a hard 'assertTmuxTail' to produce a proper Hedgehog failure.
pollAssertTmuxTail ::
  -- | Maximum number of poll iterations (each ~10ms)
  Word ->
  Text ->
  TmuxTest M ()
pollAssertTmuxTail maxAttempts target =
  go 0
  where
    targetLines = Text.lines (Text.stripEnd target)
    targetLen = length targetLines

    go count = do
      actual <- capturePlain
      let
        actualLines = filter (not . Text.null) (Text.lines actual)
        matched = targetLines == drop (length actualLines - targetLen) actualLines
      if matched
      then unit
      else if count >= maxAttempts
      then assertTmuxTail False target
      else do
        liftIO (threadDelay 10_000)
        go (count + 1)

-- | Reproduce the help corner position bug: on initial render, the key mappings
-- corner appears in the upper right instead of the lower right because 'contentHeight'
-- starts at 0 and is only updated after the first event.
--
-- This test starts the release flow, waits for the Brick app to start, but does NOT
-- send a sync key (unlike 'runStep'), so 'updateContentHeight' is never called.
-- The assertion expects the correct layout ('versionBox') which should fail if the
-- corner is mispositioned.
releaseFlowInitialRenderTest :: TmuxTest M ()
releaseFlowInitialRenderTest = do
  debug <- tmuxLiftM brickDebug
  handlers <- tmuxLiftM (handlersFlowTest defaultTestConfig debug)
  withAsync (tmuxLiftM (void (release handlers flowTestConfig flowTestContext))) \ _ -> do
    tmuxLiftM (waitForApp debug)
    -- Do NOT call syncRender here — that would trigger updateContentHeight and mask the bug.
    -- Poll for up to 3 seconds (300 iterations * 10ms).
    pollAssertTmuxTail 300 versionBox

test_releaseFlowInitialRender :: TestT IO ()
test_releaseFlowInitialRender =
  tmuxTest True 10 releaseFlowInitialRenderTest
