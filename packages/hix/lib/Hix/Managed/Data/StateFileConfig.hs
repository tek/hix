module Hix.Managed.Data.StateFileConfig where

import Path (File, SomeBase (Rel), relfile)

import Distribution.Pretty (Pretty (pretty))
import Hix.Data.PathSpec (PathSpec (PathConcrete))
import Hix.Pretty (prettyMap, field)

data StateFileConfig =
  StateFileConfig {
    file :: PathSpec File
  }
  deriving stock (Eq, Show, Generic)

instance Pretty StateFileConfig where
  pretty StateFileConfig {..} =
    prettyMap "state file" [
      field "file" file
    ]

instance Default StateFileConfig where
  def =
    StateFileConfig {
      file = PathConcrete $ Rel [relfile|ops/managed.nix|]
    }
