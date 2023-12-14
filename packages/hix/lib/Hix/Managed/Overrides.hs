module Hix.Managed.Overrides where

import qualified Data.Map.Strict as Map

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides (Overrides))
import qualified Hix.Data.Package
import Hix.Data.Package (Package (Package), PackageName)
import qualified Hix.Managed.Handlers.Hackage
import Hix.Managed.Handlers.Hackage (HackageHandlers)

newVersionOverride ::
  HackageHandlers ->
  Package ->
  M (PackageName, Override)
newVersionOverride handlers Package {..} = do
  hash <- handlers.fetchHash name version
  pure (name, Override {..})

newVersionOverrides ::
  HackageHandlers ->
  [Package] ->
  M Overrides
newVersionOverrides handlers versions =
  Overrides . Map.fromList <$> traverse (newVersionOverride handlers) versions
