module Hix.Managed.Cabal.Data.UploadStage where

import Distribution.Client.Setup (IsCandidate (..))
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint ((<+>))

data ArtifactSort =
  ArtifactSources
  |
  ArtifactDocs
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance Pretty ArtifactSort where
  pretty = \case
    ArtifactDocs -> "docs"
    ArtifactSources -> "sources"

data UploadMutability =
  UploadCandidate
  |
  UploadPublish
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance Pretty UploadMutability where
  pretty = \case
    UploadCandidate -> "candidate"
    UploadPublish -> "published"

isCandidate :: UploadMutability -> IsCandidate
isCandidate = \case
  UploadCandidate -> IsCandidate
  UploadPublish -> IsPublished

-- | Ordered by mutability first (candidates before publish), then artifact (docs before sources).
data UploadStage =
  UploadStage {
    mutability :: UploadMutability,
    artifact :: ArtifactSort
  }
  deriving stock (Eq, Ord, Show, Generic, Bounded)

instance Enum UploadStage where
  fromEnum UploadStage {mutability, artifact} =
    fromEnum mutability * 2 + fromEnum artifact
  toEnum n
    | Just (q, r) <- n `quotRem` 2 = UploadStage (toEnum q) (toEnum r)
    | otherwise = error "UploadStage.toEnum: division by zero (impossible)"

instance Pretty UploadStage where
  pretty UploadStage {..} = pretty mutability <+> pretty artifact

candidateSources :: UploadStage
candidateSources = UploadStage {mutability = UploadCandidate, artifact = ArtifactSources}

candidateDocs :: UploadStage
candidateDocs = UploadStage {mutability = UploadCandidate, artifact = ArtifactDocs}

publishSources :: UploadStage
publishSources = UploadStage {mutability = UploadPublish, artifact = ArtifactSources}

publishDocs :: UploadStage
publishDocs = UploadStage {mutability = UploadPublish, artifact = ArtifactDocs}
