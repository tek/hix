module Hix.Managed.Overrides where

import qualified Data.Map.Strict as Map

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Override (..), Overrides (Overrides))
import Hix.Data.Package (PackageName)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import qualified Hix.Managed.Handlers.Hackage
import Hix.Managed.Handlers.Hackage (HackageHandlers)

newVersionOverride ::
  HackageHandlers ->
  NewVersion ->
  M (PackageName, Override)
newVersionOverride handlers NewVersion {..} = do
  hash <- handlers.fetchHash package version
  pure (package, Override {..})

newVersionOverrides ::
  HackageHandlers ->
  [NewVersion] ->
  M Overrides
newVersionOverrides handlers versions =
  Overrides . Map.fromList <$> traverse (newVersionOverride handlers) versions
