module Hix.Managed.Overrides where

import Hix.Class.Map (nForAssoc)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (LocalPackage, isLocalPackage)
import qualified Hix.Managed.Handlers.SourceHash
import Hix.Managed.Handlers.SourceHash (SourceHashHandlers)
import Hix.Monad (fatalError)

-- | Fetch an override's hash from the given sources, which is the set of configured Hackages in production.
-- If the package wasn't found anywhere, and it is part of the local build, assume that it hasn't been published yet and
-- force it to be built from local sources by returning 'Local'.
packageOverride ::
  SourceHashHandlers ->
  Set LocalPackage ->
  PackageId ->
  M Override
packageOverride handlers localUnavailable package@PackageId {version} = do
  handlers.fetchHash package >>= \case
    Right (hash, repo) -> pure Override {..}
    Left err | isLocalPackage localUnavailable package.name -> pure Local
             | otherwise -> fatalError err

packageOverrides ::
  SourceHashHandlers ->
  Set LocalPackage ->
  [PackageId] ->
  M Overrides
packageOverrides handlers localUnavailable versions =
  nForAssoc versions \ pid -> do
    o <- packageOverride handlers localUnavailable pid
    pure (pid.name, o)
