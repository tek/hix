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

data IsRevision =
  IsRevision
  |
  IsNotRevision
  deriving stock (Eq, Show)

isRevision :: IsRevision -> Bool
isRevision = \case
  IsRevision -> True
  IsNotRevision -> False

toIsRevision :: Bool -> IsRevision
toIsRevision = \case
  True -> IsRevision
  False -> IsNotRevision

instance FromJSON IsRevision where
  parseJSON v =
    toIsRevision <$> parseJSON v

instance EncodeNix IsRevision where
  encodeNix = encodeNix . isRevision

data Override =
  Override {
    version :: Version,
    hash :: SourceHash,
    repo :: Maybe HackageName,
    revision :: Maybe IsRevision
  }
  |
  Jailbreak
  |
  Local
  deriving stock (Eq, Show, Generic)

instance EncodeNix Override where
  encodeNix = \case
    Override {..} ->
      ExprAttrs (static <> foldMap (pure . assoc "repo") repo <> foldMap (pure . assoc "revision") revision)
      where
        static = [assoc "version" version, assoc "hash" hash]

        assoc :: EncodeNix a => ExprKey -> a -> ExprAttr
        assoc name a = ExprAttr {name, value = encodeNix a}
    Jailbreak ->
      ExprAttrs [ExprAttr {name = "jailbreak", value = encodeNix True}]
    Local ->
      ExprAttrs [ExprAttr {name = "local", value = encodeNix True}]

override :: Version -> SourceHash -> Override
override version hash =
  Override {repo = Nothing, revision = Nothing, ..}

instance FromJSON Override where
  parseJSON =
    withObject "Override" \ o ->
      regular o <|> jailbreak o <|> local o
    where
      regular o = do
        JsonParsec version <- o .: "version"
        hash <- o .: "hash"
        repo <- o .:? "repo"
        revision <- o .:? "revision"
        pure Override {..}

      jailbreak o = do
        flag <- o .: "jailbreak"
        guard flag
        pure Jailbreak

      local o = do
        flag <- o .: "local"
        guard flag
        pure Local

instance Pretty Override where
  pretty = \case
    Override {..} ->
      pretty version <+> brackets (pretty hash <> foldMap renderRepo repo <> foldMap renderRevision revision)
    Jailbreak -> "jailbreak"
    Local -> "local"
    where
      renderRepo (HackageName name) =
        hcat [text ",", hpretty name]

      renderRevision = \case
        IsRevision -> ",rev"
        IsNotRevision -> mempty

-- | Overrides can be either for mutable (direct, nonlocal) deps, or for transitive deps, so they must use
-- 'PackageName'.
newtype Overrides =
  Overrides (Map PackageName Override)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

instance NMap Overrides PackageName Override LookupMaybe where

instance Pretty Overrides where
  pretty = nPretty
