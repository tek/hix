module Hix.Managed.Maint.MaintResult where

import qualified Data.Set as Set
import Exon (exon)

import Hix.Class.Map (nTo)
import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Data.OutputTarget (OutputTarget)
import Hix.Managed.Data.BuildOutput (DepChanges (..), ModifiedId (..))
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.Mutable (depName)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Maint.Data.MaintEnv (MaintEnv (..))
import Hix.Managed.Maint.Data.MaintResult (
  ChangedPackage (..),
  FailedPackage (..),
  MaintOutput (..),
  MaintResult (..),
  MaintStatus (..),
  NoPublishReason (..),
  UnchangedReason (..),
  UpdateAction (..),
  )
import Hix.Managed.Maint.Data.MaintTarget (MaintTarget (..))
import Hix.Monad (clientError)
import Hix.OutputWriter (OutputSpecial (..), writeOutput)

bumpResult :: MaintEnv -> DepChanges -> MaintStatus
bumpResult env changes
  | not (null changes.failed)
  , env.config.noFailures
  = BumpFailure
  | Just modified <- nonEmpty changes.modified
  = UpdatedVersions updateAction modified
  | otherwise
  = NoChanges
  where
    updateAction | null directDeps = OnlyCommit NoDirectDepUpdates
                 | not env.config.revision = OnlyCommit PublishDisabled
                 | any hasRangeUpdate directDeps = PublishRevision
                 | otherwise = OnlyCommit NoRangeUpdates

    directDeps =
      flip filter changes.modified \ ModifiedId {package} -> Set.member (depName package) env.target.deps

    hasRangeUpdate ModifiedId {range} = isJust range

maintOutput :: Packages MaintResult -> MaintOutput
maintOutput packages =
  MaintOutput {changes, failures}
  where
    (changes, failures) = fmap catMaybes $ partitionEithers $ nTo packages \ package -> \case
      Unchanged reason
        | CannotProceed message <- reason
        -> Right (Just FailedPackage {..})
        | otherwise
        -> Right Nothing
      Changed {branch, baseBranch} -> Left ChangedPackage {message = "changed", ..}

writeMaintOutput :: OutputTarget -> OutputFormat -> MaintOutput -> M ()
writeMaintOutput =
  writeOutput $ const \case
    OutputSpecialCommitMsg -> unsupported "commit-msg"
    OutputSpecialGaPr -> unsupported "ga-pr"
    OutputSpecialGithubOutputKey -> pure "maint"
  where
    unsupported :: ∀ a . Text -> M a
    unsupported format =
      clientError [exon|Output format #{Color.yellow format} isn't supported for #{Color.green @Text "maint"}|]

outputResults :: OutputTarget -> OutputFormat -> Packages MaintResult -> M ()
outputResults target format results =
  writeMaintOutput target format (maintOutput results)
