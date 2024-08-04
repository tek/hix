module Hix.Data.Overrides where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Distribution.Pretty (Pretty (pretty))
import Distribution.Types.Version (Version)
import GHC.Exts (IsList)
import Text.PrettyPrint (brackets, hcat, text, (<+>))

import Hix.Class.EncodeNix (EncodeNix (..))
import Hix.Class.Map (LookupMaybe, NMap, nPretty)
import Hix.Data.Json (JsonParsec (JsonParsec))
import Hix.Data.NixExpr (Expr (ExprAttrs), ExprAttr (..), ExprKey)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (SourceHash)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName (..))
import Hix.Pretty (hpretty)

data Override =
  Override {
    version :: Version,
    hash :: SourceHash,
    repo :: Maybe HackageName
  }
  deriving stock (Eq, Show, Generic)

instance EncodeNix Override where
  encodeNix Override {..} =
    ExprAttrs (static <> foldMap (pure . assoc "repo") repo)
    where
      static = [assoc "version" version, assoc "hash" hash]

      assoc :: EncodeNix a => ExprKey -> a -> ExprAttr
      assoc name a = ExprAttr {name, value = encodeNix a}

override :: Version -> SourceHash -> Override
override version hash =
  Override {repo = Nothing, ..}

instance FromJSON Override where
  parseJSON =
    withObject "Override" \ o -> do
      JsonParsec version <- o .: "version"
      hash <- o .: "hash"
      repo <- o .:? "repo"
      pure Override {..}

instance Pretty Override where
  pretty Override {..} =
    pretty version <+> brackets (pretty hash <> foldMap renderRepo repo)
    where
      renderRepo (HackageName name) =
        hcat [text ",", hpretty name]

-- | Overrides can be either for mutable (direct, nonlocal) deps, or for transitive deps, so they must use
-- 'PackageName'.
newtype Overrides =
  Overrides (Map PackageName Override)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

instance NMap Overrides PackageName Override LookupMaybe where

instance Pretty Overrides where
  pretty = nPretty
