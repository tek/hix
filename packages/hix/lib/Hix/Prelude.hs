module Hix.Prelude where

import Data.Generics.Labels ()
import Data.List.Extra (firstJust)
import qualified Distribution.ModuleName as ModuleName
import Distribution.ModuleName (ModuleName)
import Distribution.PackageDescription (ModuleRenaming (..), unPackageName)
import Distribution.Types.IncludeRenaming (IncludeRenaming (..))
import Distribution.Types.Mixin (Mixin (..))
import qualified Exon

data Prelude =
  Prelude {
    preludePackage :: String,
    preludeModule :: String,
    local :: Bool
  }
  deriving stock (Eq, Show)

preludeRenaming :: [(b, ModuleName)] -> Maybe b
preludeRenaming =
  firstJust \case
    (real, "Prelude") -> Just real
    _ -> Nothing

pattern PreludeRenaming :: ModuleName -> ModuleRenaming
pattern PreludeRenaming p <- ModuleRenaming (preludeRenaming -> Just p)

pattern PreludeInclude :: ModuleName -> IncludeRenaming
pattern PreludeInclude p <- IncludeRenaming {includeProvidesRn = PreludeRenaming p}

findPrelude :: [Mixin] -> Maybe Prelude
findPrelude =
  firstJust \case
    Mixin {mixinIncludeRenaming = PreludeInclude real, ..} ->
      let
        preludePackage = unPackageName mixinPackageName
        preludeModule = Exon.intercalate "." (ModuleName.components real)
      in Just Prelude {local = False, ..}
    _ -> Nothing
