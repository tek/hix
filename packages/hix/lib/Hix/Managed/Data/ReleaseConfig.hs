module Hix.Managed.Data.ReleaseConfig where

import Data.Aeson (ToJSON)

import Hix.Data.Version (Version)
import Hix.Managed.Data.VersionIncrement (VersionIncrement)
import Hix.Managed.Release.Data.TargetSpec (TargetSpec)

data CandidatesSpec =
  CandidatesAuto
  |
  CandidatesSelected {
    sources :: Bool,
    docs :: Bool
  }
  deriving stock (Eq, Show)

data ArtifactConfig =
  ArtifactConfig {
    sources :: Bool,
    docs :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

noArtifacts :: ArtifactConfig
noArtifacts =
  ArtifactConfig {sources = False, docs = False}

bothArtifacts :: ArtifactConfig
bothArtifacts =
  ArtifactConfig {sources = True, docs = True}

data ReleaseVersion =
  ConcreteVersion Version
  |
  KeepVersion
  |
  VersionIncrement VersionIncrement
  deriving stock (Eq, Show)

data ReleaseConfig =
  ReleaseConfig {
    targets :: Maybe (NonEmpty TargetSpec),
    version :: Maybe ReleaseVersion,
    publish :: ArtifactConfig,
    candidates :: ArtifactConfig,
    commit :: Bool,
    tag :: Bool,
    push :: Bool,
    -- | Immediately terminate when uploading docs fails.
    fatalDocs :: Bool,
    -- | Continue the flow (publish after candidates, git after publish) with successful packages when some fail.
    partial :: Bool,
    interactive :: Bool,
    -- | Skip version validation (allows releasing current versions, versions smaller than current, etc.).
    forceVersion :: Bool,
    -- | Run Nix flake checks before uploading. Defaults to False; must be explicitly enabled with --check.
    check :: Bool,
    -- | Merge the release branch back into the initial branch after successful uploads.
    --   A temporary release branch is always created. Use --merge to merge it back.
    merge :: Bool,
    -- | Use the global Cabal configuration file instead of hermetic @/dev/null@.
    --   This allows using credentials configured in @~\/.cabal\/config@ but sacrifices reproducibility.
    globalCabalConfig :: Bool,
    -- | Keep the Brick UI screens rendered on the terminal after they are completed.
    --   When 'False' (default), the cursor is reset to overwrite the UI and a log message is printed instead.
    persistentUi :: Bool
  }
  deriving stock (Eq, Show)

instance Default ReleaseConfig where
  def =
    ReleaseConfig {
      targets = Nothing,
      version = Nothing,
      publish = noArtifacts,
      candidates = bothArtifacts,
      commit = False,
      tag = False,
      push = False,
      fatalDocs = False,
      partial = False,
      interactive = False,
      forceVersion = False,
      check = False,
      merge = False,
      globalCabalConfig = False,
      persistentUi = False
    }
