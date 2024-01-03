module Hix.Managed.BuildOutput where

import qualified Data.Aeson as Aeson
import Data.List.Extra (nubSortOn)
import qualified Data.Text as Text
import Data.These.Combinators (justThere)

import Hix.Data.Monad (M)
import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Data.OutputTarget (OutputTarget (..))
import Hix.Data.PackageName (PackageName (PackageName))
import Hix.Managed.BuildOutput.CommitMsg (formatCommit)
import Hix.Managed.BuildOutput.GithubActionsPr (githubActionsPr)
import qualified Hix.Managed.Data.BuildOutput
import Hix.Managed.Data.BuildOutput (BuildOutput (BuildOutput), ModifiedId (ModifiedId))
import Hix.Managed.Data.Mutable (MutableDep, depName)
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
import Hix.OutputWriter (outputWriterGlobal)

outputResult :: BuildOutput -> OutputTarget -> OutputFormat -> M ()
outputResult output target = \case
  OutputNone -> unit
  OutputJson -> writer.bytes (toStrict (Aeson.encode output))
  OutputCommitMsg -> traverse_ writer.text (formatCommit output)
  OutputGaPr -> githubActionsPr output target
  where
    writer = outputWriterGlobal target

buildOutputFromLists :: [ModifiedId] -> [MutableDep] -> [MutableDep] -> BuildOutput
buildOutputFromLists modified unmodified failed =
  BuildOutput {
    modified,
    unmodified,
    failed,
    modifiedNames = comma ((.package) <$> modified),
    unmodifiedNames = comma unmodified,
    failedNames = comma failed
  }
  where
    comma = fmap (Text.intercalate ", " . fmap (coerce . depName) . toList) . nonEmpty

modifiedId :: DepResult -> ModifiedId
modifiedId result =
  ModifiedId {package = result.package, version = result.version, range}
  where
    range = case result.detail of
      DepModified (DepUpdated (justThere -> Just _)) -> Just result.bounds
      DepModified DepAdded -> Just result.bounds
      _ -> Nothing

buildOutput :: ProjectResult -> BuildOutput
buildOutput result =
  buildOutputFromLists modified ((.package) <$> unmodified) failed
  where
    modified = modifiedId <$> nubSortOn (.package) (added ++ updated)
    failed = (.package) <$> ProjectResult.failures result
    DepResults {..} = ProjectResult.grouped result
