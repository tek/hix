module Hix.Managed.Data.StateFileConfig where

import Path (File, Path, Rel, relfile)
import Distribution.Pretty (Pretty (pretty))
import Hix.Pretty (prettyMap, field)

data StateFileConfig =
  StateFileConfig {
    file :: Path Rel File
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
      file = [relfile|ops/managed.nix|]
    }
