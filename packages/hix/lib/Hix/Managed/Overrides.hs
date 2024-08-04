module Hix.Managed.Overrides where

import Hix.Class.Map (nForAssoc)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import qualified Hix.Managed.Handlers.SourceHash
import Hix.Managed.Handlers.SourceHash (SourceHashHandlers)

packageOverride ::
  SourceHashHandlers ->
  PackageId ->
  M Override
packageOverride handlers package@PackageId {version} = do
  (hash, repo) <- handlers.fetchHash package
  pure Override {..}

packageOverrides ::
  SourceHashHandlers ->
  [PackageId] ->
  M Overrides
packageOverrides handlers versions =
  nForAssoc versions \ pid -> do
    o <- packageOverride handlers pid
    pure (pid.name, o)
