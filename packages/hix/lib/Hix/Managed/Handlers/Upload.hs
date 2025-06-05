module Hix.Managed.Handlers.Upload where

import Path (Abs, File, Path)

import Hix.Data.Monad (M)
import Hix.Managed.Cabal.Data.UploadStage (UploadStage)
import Hix.Managed.Data.UploadResult (ArtifactResult)

data UploadHandlers =
  UploadHandlers {
    upload :: UploadStage -> Path Abs File -> M ArtifactResult
  }

handlersNull :: UploadHandlers
handlersNull =
  UploadHandlers {
    upload = \ _ _ -> pure mempty
  }
