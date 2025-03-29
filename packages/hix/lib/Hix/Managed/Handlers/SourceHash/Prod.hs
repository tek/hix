module Hix.Managed.Handlers.SourceHash.Prod where

import Data.IORef (newIORef)

import Hix.Data.Version (SourceHash)
import Hix.Hackage.Hash (fetchHashHackageCached)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName, HackageRepo)
import qualified Hix.Managed.Handlers.SourceHash
import Hix.Managed.Handlers.SourceHash (SourceHashHandlers (SourceHashHandlers))

handlersProdWith ::
  MonadIO m =>
  Map Text (SourceHash, HackageName) ->
  NonEmpty HackageRepo ->
  m SourceHashHandlers
handlersProdWith initial repos = do
  hashCache <- liftIO (newIORef initial)
  pure SourceHashHandlers {
    fetchHash = fetchHashHackageCached hashCache repos
  }

handlersProd ::
  MonadIO m =>
  NonEmpty HackageRepo ->
  m SourceHashHandlers
handlersProd = handlersProdWith mempty
