module Hix.Managed.Handlers.Release where

import Hix.Data.Error (Error)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Cabal.Data.UploadStage (UploadStage)
import Hix.Managed.Data.UploadResult (ArtifactResult)
import Hix.Managed.Git (GitApi)
import Hix.Managed.Handlers.Context (ContextHandlers)
import Hix.Managed.Handlers.HackageClient (HackageClient)
import Hix.Managed.Handlers.Project (ProjectHandlers)
import Hix.Managed.Handlers.ReleaseUi (ReleaseUi)
import Hix.Managed.Handlers.Upload (UploadHandlers)
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseDist)
import Hix.Managed.Release.Data.Staged (SelectedTargetView)
import Hix.Managed.Release.Git (GitExtraArgs, GitRelease)

data ReleaseHandlers =
  ReleaseHandlers {
    -- | Regenerate Cabal files and overrides after version state changes.
    -- Runs @nix run .#gen-quiet@ in production, which generates hpack-based @.cabal@ files and @gen-overrides@ (when
    -- enabled).
    genCabal :: M (),
    -- | Run flake checks. Returns 'Nothing' on success, 'Just reasons' on failure.
    runChecks :: M (Maybe [Text]),
    releaseDist :: SelectedTargetView -> M (Either Error ReleaseDist),
    uploadArtifact :: Text -> UploadStage -> LocalPackage -> ReleaseDist -> M ArtifactResult,
    git :: GitExtraArgs -> GitApi GitRelease,
    context :: ContextHandlers,
    project :: ProjectHandlers,
    upload :: UploadHandlers,
    publishHackages :: NonEmpty HackageClient,
    ui :: ReleaseUi
  }
