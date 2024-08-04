module Hix.Managed.BuildOutput where

import Data.List.Extra (nubSortOn)
import Data.These.Combinators (justThere)

import Hix.Data.Monad (M)
import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Data.OutputTarget (OutputTarget (..))
import Hix.Managed.BuildOutput.CommitMsg (formatCommit, formatMutableDeps, modifiedIdNames, commit)
import Hix.Managed.BuildOutput.GithubActionsPr (githubActionsPr)
import qualified Hix.Managed.Data.BuildOutput
import Hix.Managed.Data.BuildOutput (
  BuildOutput (BuildOutput),
  DepChanges (..),
  DepChangesNames (..),
  ModifiedId (ModifiedId),
  )
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.ProjectResult (ProjectResult)
import qualified Hix.Managed.EnvResult
import Hix.Managed.EnvResult (
  DepModification (DepAdded, DepUpdated),
  DepResult,
  DepResultDetail (DepModified),
  DepResults (DepResults),
  )
import qualified Hix.Managed.ProjectResult as ProjectResult
import qualified Hix.OutputWriter
import Hix.OutputWriter (OutputWriter, writeOutput, OutputSpecial (..))

writeCommit :: BuildOutput -> OutputWriter -> M ()
writeCommit output writer =
  traverse_ writer.text (formatCommit output.changes)

outputResult :: OutputTarget -> OutputFormat -> BuildOutput -> M ()
outputResult =
  writeOutput \ output -> \case
    OutputSpecialCommitMsg -> pure (commit output.changes)
    OutputSpecialGaPr -> githubActionsPr output.changes
    OutputSpecialGithubOutputKey -> pure "build"

modifiedId :: DepResult -> ModifiedId
modifiedId result =
  ModifiedId {package = result.package, version = result.version, range}
  where
    range = case result.detail of
      DepModified (DepUpdated (justThere -> Just _)) -> Just result.bounds
      DepModified (DepAdded _) -> Just result.bounds
      _ -> Nothing

depChanges :: ProjectResult -> DepChanges
depChanges result =
  DepChanges {
    modified = modifiedId <$> nubSortOn (.package) (added ++ updated),
    unmodified = (.package) <$> unmodified,
    failed = (.package) <$> ProjectResult.failures result
  }
  where
    DepResults {..} = ProjectResult.grouped result

depChangesNames :: DepChanges -> DepChangesNames
depChangesNames DepChanges {..} =
  DepChangesNames {
    modifiedNames = modifiedIdNames modified,
    unmodifiedNames = formatMutableDeps unmodified,
    failedNames = formatMutableDeps failed
  }

buildOutput :: DepChanges -> BuildOutput
buildOutput changes =
  BuildOutput {
    changes,
    names = depChangesNames changes
  }
