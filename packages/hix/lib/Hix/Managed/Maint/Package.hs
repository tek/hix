module Hix.Managed.Maint.Package where

import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, addExtension, parseRelFile, (</>))
import Text.PrettyPrint (nest, text, ($$), (<+>))

import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage (..), localPackageName)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.Revision (Revision (number), RevisionPublished (..))
import Hix.Managed.Cabal.PackageDescription (parseCabal, parseCabalFile)
import qualified Hix.Managed.Cabal.Upload as Upload
import Hix.Managed.Cabal.Upload (latestRevision, revisionCabalFile)
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Git (BranchName, MaintBranch (..), Tag (..))
import Hix.Managed.Handlers.HackageClient (HackageClient (..))
import Hix.Managed.Handlers.Maint (MaintHandlers (..))
import Hix.Managed.Handlers.Revision (RevisionHandlers (..))
import Hix.Managed.Maint.Data.MaintEnv (MaintEnv (..))
import Hix.Managed.Maint.Data.MaintPrep (MaintPrep (..))
import Hix.Managed.Maint.Data.MaintResult (
  MaintChanged (..),
  MaintResult (..),
  MaintStatus (..),
  UnchangedReason (..),
  UpdateAction (..),
  )
import Hix.Managed.Maint.Data.MaintTarget (MaintTarget (..))
import Hix.Managed.Maint.Git (GitMaint (..))
import Hix.Managed.Maint.MaintResult (bumpResult)
import Hix.Managed.Maint.Prep (maintPrep)
import Hix.Managed.Maint.RevisionPlan (RevisionTarget (..))
import Hix.Monad (eitherFatal)
import Hix.Pretty (hpretty, showP)

packageCabalFile :: LocalPackage -> Path Rel Dir -> M (Path Abs File)
packageCabalFile package path = do
  root <- appRes.root
  cabalFileName <- eitherFatal $ first nameParseError do
    name <- parseRelFile (toString package)
    addExtension ".cabal" name
  pure (root </> path </> cabalFileName)
  where
    nameParseError err = [exon|Package name '##{package}' is not a valid file path: #{show err}|]

publishRevision ::
  MaintEnv ->
  M MaintChanged
publishRevision env = do
  cabalFile <- packageCabalFile env.target.package env.target.path
  revision <- Upload.publishRevision env.handlers.publishHackages pid cabalFile
  pure (Published revision)
  where
    pid = PackageId {name = localPackageName package, version}
    MaintTarget {package, version} = env.target

processUpdateAction :: MaintEnv -> UpdateAction -> M MaintChanged
processUpdateAction env = \case
  OnlyCommit reason -> pure (Modified reason)
  PublishRevision -> publishRevision env

processStatus ::
  MaintEnv ->
  MaintBranch ->
  BranchName ->
  MaintStatus ->
  M MaintResult
processStatus env maintBranch baseBranch = \case
  BumpFailure ->
    pure (Unchanged (CannotProceed "Some dependency updates failed to build."))
  NoChanges ->
    pure (Unchanged NoUpdates)
  UpdatedVersions action modified -> do
    traceBump modified
    branch <-
      if env.config.commit
      then env.git.commitBump maintBranch env.target.package modified
      else pure baseBranch
    resolution <- processUpdateAction env action
    pure Changed {..}
  where
    traceBump modified =
      Log.traceP $
        text "Bumped package:" <+> pretty env.target.package $$
        nest 2 (
          text "deps:" <+> hpretty env.target.deps $$
          text "modified:" <+> hpretty modified
        )

maintPackage ::
  MaintHandlers ->
  MaintConfig ->
  GitMaint ->
  [Tag] ->
  MaintTarget ->
  M MaintResult
maintPackage handlers config git tags target =
  maintPrep env tags target.package target.version >>= \case
    PrepBranch {..} -> do
      status <- bumpResult env <$> env.handlers.runBump env.target.env
      Log.debug [exon|Maint status: #{showP status}|]
      processStatus env targetBranch workBranch status
    PrepNoTags ->
      pure (Unchanged NoTags)
  where
    env = MaintEnv {handlers, target, git, config}

publishRevisionIfChangedTo :: RevisionHandlers -> RevisionTarget -> HackageClient -> M (Maybe RevisionPublished)
publishRevisionIfChangedTo handlers target hackage = do
  latest <- latestRevision hackage pid
  oldCabal <- revisionCabalFile hackage pid latest.number
  oldPkgDesc <- eitherFatal (parseCabal "hackage" (encodeUtf8 oldCabal))
  cabalFile <- packageCabalFile target.package target.path
  newPkgDesc <- parseCabalFile cabalFile
  if oldPkgDesc == newPkgDesc
  then pure Nothing
  else do
    revision <- Upload.publishRevision handlers.publishHackages pid cabalFile
    pure (Just RevisionPublished {revision, destination = hackage.description})
  where
    pid = PackageId {name = target.package.name, version = target.version}

publishRevisionIfChanged ::
  RevisionHandlers ->
  RevisionTarget ->
  M [RevisionPublished]
publishRevisionIfChanged handlers target =
  catMaybes . toList <$> traverse (publishRevisionIfChangedTo handlers target) handlers.publishHackages
