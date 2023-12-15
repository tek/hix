module Hix.Managed.Overrides where

import qualified Data.Map.Strict as Map

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides (Overrides))
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (PackageName)
import qualified Hix.Managed.Handlers.Hackage
import Hix.Managed.Handlers.Hackage (HackageHandlers)

newVersionOverride ::
  HackageHandlers ->
  PackageId ->
  M (PackageName, Override)
newVersionOverride handlers package@PackageId {name, version} = do
  hash <- handlers.fetchHash package
  pure (name, Override {..})

newVersionOverrides ::
  HackageHandlers ->
  [PackageId] ->
  M Overrides
newVersionOverrides handlers versions =
  Overrides . Map.fromList <$> traverse (newVersionOverride handlers) versions
