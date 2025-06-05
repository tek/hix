-- | Staged release data types.
--
-- The release flow progresses through stages, with the data type evolving
-- to reflect what information is available at each stage:
--
-- @
-- Pristine -> Selecting -> Prepared -> Uploading
-- @
--
-- The flow status wraps the stage in an outer layer:
--
-- @
-- InProgress Stage | Terminated Stage
-- @
--
-- This two-tier design makes 'Termination' precise: 'Terminated' status always carries a reason,
-- and 'InProgress' status never does.
--
-- The version mode (shared vs individual) is encoded as a type parameter:
-- - Shared mode: Version stored once in constructor, targets have no version field
-- - Individual mode: Each target has its own SelectedVersion
--
-- Type families compute stage-dependent target and failure types.
module Hix.Managed.Release.Data.Staged where

import GHC.TypeError (ErrorMessage (..))

import Hix.Class.Map (nMap)
import Hix.Data.Error (Error)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget)
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseDist)
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..))
import Hix.Managed.Release.Data.UploadResult (PackageResult)

-- | The stages a release progresses through.
data Stage =
  -- | Initial stage.
  Pristine
  |
  -- | After version selection.
  Selecting
  |
  -- | After creating release distributions.
  Prepared
  |
  -- | Ready for uploading (after dist preparation).
  Uploading

-- | The outer status of the release flow, wrapping a 'Stage'.
-- 'InProgress' means the flow is active; 'Terminated' means it was aborted.
data FlowStatus =
  InProgress Stage
  |
  Terminated Stage

-- | Map each stage to its forward successor.
-- Used by 'Hix.Managed.Release.Staged.transition' for the common case of advancing one step.
-- 'Uploading' has no forward successor.
type NextStage :: Stage -> Stage
type family NextStage stage where
  NextStage 'Pristine = 'Selecting
  NextStage 'Selecting = 'Prepared
  NextStage 'Prepared = 'Uploading
  NextStage 'Uploading =
    TypeError ('Text "Attempted to transition the release state from the final stage 'Uploading'")

type SStage :: Stage -> Type
data SStage stage where
  SPristine :: SStage 'Pristine
  SSelecting :: SStage 'Selecting
  SPrepared :: SStage 'Prepared
  SUploading :: SStage 'Uploading

deriving stock instance Show (SStage stage)

type SFlowStatus :: FlowStatus -> Type
data SFlowStatus status where
  SInProgress :: SStage s -> SFlowStatus ('InProgress s)
  STerminated :: SStage s -> SFlowStatus ('Terminated s)

deriving stock instance Show (SFlowStatus status)

type Transition :: FlowStatus -> FlowStatus -> Type
data Transition from to where
  PristineToSelecting :: Transition ('InProgress 'Pristine) ('InProgress 'Selecting)
  SelectingToPrepared :: Transition ('InProgress 'Selecting) ('InProgress 'Prepared)
  PreparedToUploading :: Transition ('InProgress 'Prepared) ('InProgress 'Uploading)
  AnyToTerminate :: SStage s -> Transition ('InProgress s) ('Terminated s)

deriving stock instance Show (Transition from to)

type KnownStage :: Stage -> Constraint
class KnownStage stage where
  stageVal :: SStage stage

instance KnownStage 'Pristine where stageVal = SPristine
instance KnownStage 'Selecting where stageVal = SSelecting
instance KnownStage 'Prepared where stageVal = SPrepared
instance KnownStage 'Uploading where stageVal = SUploading

type KnownFlowStatus :: FlowStatus -> Constraint
class KnownFlowStatus status where
  flowStatusVal :: SFlowStatus status

instance KnownStage s => KnownFlowStatus ('InProgress s) where
  flowStatusVal = SInProgress stageVal

instance KnownStage s => KnownFlowStatus ('Terminated s) where
  flowStatusVal = STerminated stageVal

data VersionMode = Shared | Individual

type ModeMismatch :: VersionMode -> VersionMode -> Type
data ModeMismatch purpose actual where
  MMShared :: ModeMismatch 'Shared 'Individual
  MMIndividual :: ModeMismatch 'Individual 'Shared

deriving stock instance Eq (ModeMismatch purpose actual)
deriving stock instance Show (ModeMismatch purpose actual)

-- | The @purpose@ parameter is fixed by fields in data types that store a version to indicate what the version
-- corresponds to: in a 'Target', the version belongs to a single package, so the field uses 'Individual' for @purpose@.
--
-- The @actual@ parameter is provided by the consumer of this version to indicate what mode the app state is in.
-- The user can toggle between shared and individual mode, which changes the release flow's state to flip the actual
-- mode.
--
-- When @purpose ~ actual@, 'Active' can be used to store a version.
-- Otherwise, 'Inactive' stores a 'ModeMismatch', which does not have a constructor with @purpose ~ actual@, ensuring
-- that this cannot be used instead of 'Active'.
--
-- In order to flip the mode, the release state has to be traversed and each version field updated to match the types.
type StageVersion :: VersionMode -> VersionMode -> Type
data StageVersion purpose actual where
  Active :: SelectedVersion -> StageVersion purpose purpose
  Inactive :: ModeMismatch purpose actual -> StageVersion purpose actual

deriving stock instance Eq (StageVersion purpose actual)
deriving stock instance Show (StageVersion purpose actual)

pattern InactiveShared :: StageVersion 'Shared 'Individual
pattern InactiveShared = Inactive MMShared

pattern InactiveIndividual :: StageVersion 'Individual 'Shared
pattern InactiveIndividual = Inactive MMIndividual

{-# complete Active, InactiveShared, InactiveIndividual #-}

-- | A release target, used bare in 'Selecting' and wrapped in subsequent stages.
-- In shared mode, 'modeVersion' is 'InactiveIndividual'.
-- In individual mode, 'modeVersion' is 'Active'.
type Target :: VersionMode -> Type
data Target mode =
  Target {
    package :: LocalPackage,
    current :: Version,
    modeVersion :: StageVersion 'Individual mode
  }
  deriving stock (Eq, Show, Generic)

-- | A target that failed during dist preparation.
type TargetFailed :: VersionMode -> Type
data TargetFailed mode =
  TargetFailed {
    target :: Target mode,
    failure :: Error
  }
  deriving stock (Eq, Show, Generic)

-- | A target ready for upload.
type TargetPrepared :: VersionMode -> Type
data TargetPrepared mode =
  TargetPrepared {
    target :: Target mode,
    dist :: ReleaseDist
  }
  deriving stock (Eq, Show)

-- | A target that is being uploaded.
type TargetUploading :: VersionMode -> Type
data TargetUploading mode =
  TargetUploading {
    target :: Target mode,
    dist :: ReleaseDist,
    results :: PackageResult
  }
  deriving stock (Eq, Show, Generic)

-- | View type for 'Selected' for performing operations without state update.
data SelectedTargetView =
  SelectedTargetView {
    package :: LocalPackage,
    current :: Version,
    releaseVersion :: Version,
    -- | Whether the version was explicitly specified by the user.
    explicit :: Bool,
    -- | Whether the user explicitly requested keeping the current version.
    keep :: Bool
  }
  deriving stock (Eq, Show)

-- | View type for 'Prepared' for performing operations without state update.
data PreparedTargetView =
  PreparedTargetView {
    package :: LocalPackage,
    current :: Version,
    releaseVersion :: Version,
    dist :: ReleaseDist
  }
  deriving stock (Eq, Show)

-- | View type for 'Uploading' for performing operations without state update.
data UploadingTargetView =
  UploadingTargetView {
    package :: LocalPackage,
    current :: Version,
    releaseVersion :: Version,
    dist :: ReleaseDist,
    results :: PackageResult
  }
  deriving stock (Eq, Show)

-- | Target type associated with each stage.
type StageTargets :: Stage -> VersionMode -> Type
type family StageTargets stage mode where
  StageTargets 'Pristine _ = ()
  StageTargets 'Selecting mode = Target mode
  StageTargets 'Prepared mode = TargetPrepared mode
  StageTargets 'Uploading mode = TargetUploading mode

-- | Target type associated with a flow status, dispatching to the inner stage.
type FlowTargets :: FlowStatus -> VersionMode -> Type
type family FlowTargets status mode where
  FlowTargets ('InProgress s) mode = StageTargets s mode
  FlowTargets ('Terminated s) mode = StageTargets s mode

-- | Failure type associated with each stage.
type StageFailed :: Stage -> VersionMode -> Type
type family StageFailed stage mode where
  StageFailed 'Pristine _ = ()
  StageFailed 'Selecting _ = ()
  StageFailed 'Prepared mode = Packages (TargetFailed mode)
  StageFailed 'Uploading mode = Packages (TargetFailed mode)

-- | Failure type associated with a flow status, dispatching to the inner stage.
type FlowFailed :: FlowStatus -> VersionMode -> Type
type family FlowFailed status mode where
  FlowFailed ('InProgress s) mode = StageFailed s mode
  FlowFailed ('Terminated s) mode = StageFailed s mode

-- | Termination reason, precise by construction.
-- 'InProgress' status has no termination; 'Terminated' always carries a reason.
type Termination :: FlowStatus -> Type
data Termination status where
  NoTermination :: Termination ('InProgress s)
  Termination :: Text -> Termination ('Terminated s)

deriving stock instance Eq (Termination status)
deriving stock instance Show (Termination status)

-- | Release data that evolves through stages.
type ReleaseState :: VersionMode -> FlowStatus -> Type
data ReleaseState mode status =
  ReleaseState {
    version :: StageVersion 'Shared mode,
    targets :: Packages (FlowTargets status mode),
    failed :: FlowFailed status mode,
    excluded :: Packages ConfiguredTarget,
    termination :: Termination status
  }
  deriving stock (Generic)

deriving stock instance (
    Show (FlowTargets status mode),
    Show (FlowFailed status mode)
  ) => Show (ReleaseState mode status)

emptyReleaseData :: ReleaseState 'Individual ('InProgress 'Pristine)
emptyReleaseData =
  ReleaseState {
    version = InactiveShared,
    targets = mempty,
    failed = mempty,
    excluded = mempty,
    termination = NoTermination
  }

viewSelected ::
  StageVersion 'Shared mode ->
  StageVersion 'Individual mode ->
  SelectedVersion
viewSelected = \cases
  (Active sv) (Inactive _) -> sv
  (Inactive _) (Active sv) -> sv

-- | Extract the release version from either the shared version or the individual mode version, based on which is
-- 'Active'.
modeReleaseVersion :: StageVersion 'Shared mode -> StageVersion 'Individual mode -> Version
modeReleaseVersion s i = (viewSelected s i).version

-- | Whether the version was explicitly specified by the user.
modeExplicitVersion :: StageVersion 'Shared mode -> StageVersion 'Individual mode -> Bool
modeExplicitVersion s i = (viewSelected s i).explicit

-- | Whether the package should keep its current version.
modeKeepVersion :: StageVersion 'Shared mode -> StageVersion 'Individual mode -> Bool
modeKeepVersion s i = (viewSelected s i).keep

-- | Get selected targets as views at the 'Selecting' stage.
selectedTargetViews :: ReleaseState mode ('InProgress 'Selecting) -> Packages SelectedTargetView
selectedTargetViews ReleaseState {version, targets} =
  nMap (toView version) targets
  where
    toView sharedVer Target {package, current, modeVersion} =
      SelectedTargetView {
        package,
        current,
        releaseVersion = modeReleaseVersion sharedVer modeVersion,
        explicit = modeExplicitVersion sharedVer modeVersion,
        keep = modeKeepVersion sharedVer modeVersion
      }

-- | Get prepared targets as views at the 'Prepared' stage.
preparedTargetViews :: ReleaseState mode ('InProgress 'Prepared) -> Packages PreparedTargetView
preparedTargetViews ReleaseState {version, targets} =
  nMap (toView version) targets
  where
    toView sharedVer TargetPrepared {target = Target {package, current, modeVersion}, dist} =
      PreparedTargetView {
        package,
        current,
        releaseVersion = modeReleaseVersion sharedVer modeVersion,
        dist
      }

-- | Get uploading targets as views at the 'Uploading' stage.
uploadingTargetViews :: ReleaseState mode ('InProgress 'Uploading) -> Packages UploadingTargetView
uploadingTargetViews ReleaseState {version, targets} =
  nMap (toView version) targets
  where
    toView sharedVer TargetUploading {target = Target {package, current, modeVersion}, dist, results} =
      UploadingTargetView {
        package,
        current,
        releaseVersion = modeReleaseVersion sharedVer modeVersion,
        dist,
        results
      }
