module Hix.Managed.Cabal.Data.HackageRepo where

import Data.Aeson (FromJSON (parseJSON), FromJSONKey)
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Parsec (Parsec (parsec))
import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import Text.PrettyPrint (brackets, text, (<+>))

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Data.Json (jsonParsec)
import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation)
import Hix.Pretty (prettyL1, prettyText)

newtype HackageName =
  HackageName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey, EncodeNix)

instance Pretty HackageName where
  pretty = prettyText . coerce

centralName :: HackageName
centralName = "hackage.haskell.org"

newtype HackageDescription =
  HackageDescription Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, FromJSON, EncodeNix)

instance Pretty HackageDescription where
  pretty = prettyText . coerce

newtype HackageIndexState =
  HackageIndexState Timestamp
  deriving stock (Eq, Show, Generic)

instance Parsec HackageIndexState where
  parsec = HackageIndexState <$> parsec

instance FromJSON HackageIndexState where
  parseJSON = fmap jsonParsec . parseJSON

instance Pretty HackageIndexState where
  pretty (HackageIndexState ts) = pretty ts

data HackageRepo =
  HackageRepo {
    name :: HackageName,
    description :: HackageDescription,
    location :: HackageLocation,
    enable :: Bool,
    secure :: Maybe Bool,
    keys :: Maybe (NonEmpty Text),
    solver :: Bool,
    publish :: Bool,
    indexState :: Maybe HackageIndexState
  }
  deriving stock (Eq, Show, Generic)

instance Pretty HackageRepo where
  pretty HackageRepo {..} =
    [exon|#{pretty name}:#{pretty location}|] <+> flags
    where
      flags = foldMap (brackets . prettyL1) (nonEmpty (catMaybes flagValues))

      flagValues = [
        flag "disable" (not enable),
        flag "secure" (fromMaybe False secure),
        flag "solver" solver,
        flag "publish" publish
        ]

      flag desc state = if state then Just (text desc) else Nothing
