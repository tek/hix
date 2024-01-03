module Hix.Managed.Overrides where

import qualified Data.Map.Strict as Map

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides (Overrides))
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (PackageName)
import qualified Hix.Managed.Handlers.Hackage
import Hix.Managed.Handlers.Hackage (HackageHandlers)

packageOverride ::
  HackageHandlers ->
  PackageId ->
  M (PackageName, Override)
packageOverride handlers package@PackageId {name, version} = do
  hash <- handlers.fetchHash package
  pure (name, Override {..})

packageOverrides ::
  HackageHandlers ->
  [PackageId] ->
  M Overrides
packageOverrides handlers versions =
  Overrides . Map.fromList <$> traverse (packageOverride handlers) versions
