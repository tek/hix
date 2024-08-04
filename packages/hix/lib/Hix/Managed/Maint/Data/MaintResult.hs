module Hix.Managed.Maint.Data.MaintResult where

import Data.Aeson (ToJSON (toJSON))
import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import Text.PrettyPrint (brackets, hang, parens, text, vcat, (<+>))

import Hix.Data.PackageName (LocalPackage (..))
import Hix.Managed.Cabal.Data.Revision (Revision (..))
import Hix.Managed.Data.BuildOutput (ModifiedId (..))
import Hix.Managed.Git (BranchName)
import Hix.Pretty (hpretty)

data NoPublishReason =
  NoDirectDepUpdates
  |
  NoRangeUpdates
  |
  PublishDisabled
  deriving stock (Eq, Show)

instance Pretty NoPublishReason where
  pretty = \case
    NoDirectDepUpdates -> text "no direct dep updates"
    NoRangeUpdates -> text "no range updates"
    PublishDisabled -> text "publishing disabled by config"

data UpdateAction =
  OnlyCommit NoPublishReason
  |
  PublishRevision
  deriving stock (Eq, Show)

instance Pretty UpdateAction where
  pretty = \case
    OnlyCommit reason -> text "only commit" <+> parens (pretty reason)
    PublishRevision -> text "publish revision"

data MaintStatus =
  BumpFailure
  |
  NoChanges
  |
  UpdatedVersions UpdateAction (NonEmpty ModifiedId)
  deriving stock (Eq, Show)

instance Pretty MaintStatus where
  pretty = \case
    BumpFailure -> text "bump failure"
    NoChanges -> text "no changes"
    UpdatedVersions action modified ->
      hang (text "updated versions:") 2 (vcat [text "action:" <+> pretty action, "modified:" <+> hpretty modified])

data UnchangedReason =
  NoTags
  |
  NoUpdates
  |
  CannotProceed Text
  deriving stock (Eq, Show)

instance Pretty UnchangedReason where
  pretty = \case
    NoTags -> "No release tags found for this package"
    NoUpdates -> "All dependencies are up to date"
    CannotProceed msg -> [exon|Error: #{text (toString msg)}|]

data MaintChanged =
  Modified NoPublishReason
  |
  Published Revision
  deriving stock (Eq, Show, Generic)

instance ToJSON MaintChanged where
  toJSON = \case
    Modified _ -> toJSON ("modified" :: Text)
    Published _ -> toJSON ("published" :: Text)

instance Pretty MaintChanged where
  pretty = \case
    Modified reason -> text "Modified:" <+> pretty reason
    Published Revision {number} -> text "Published revision" <+> hpretty number

data MaintResult =
  Unchanged UnchangedReason
  |
  Changed {
    branch :: BranchName,
    baseBranch :: BranchName,
    resolution :: MaintChanged
  }
  deriving stock (Eq, Show, Generic)

instance Pretty MaintResult where
  pretty = \case
    Unchanged reason ->
      text "Unchanged:" <+> pretty reason
    Changed {..} ->
      pretty resolution <+> brackets (pretty branch)

data ChangedPackage =
  ChangedPackage {
    package :: LocalPackage,
    branch :: BranchName,
    baseBranch :: BranchName,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data FailedPackage =
  FailedPackage {
    package :: LocalPackage,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data MaintOutput =
  MaintOutput {
    changes :: [ChangedPackage],
    failures :: [FailedPackage]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
