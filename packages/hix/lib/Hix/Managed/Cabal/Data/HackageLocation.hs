module Hix.Managed.Cabal.Data.HackageLocation where

import Data.Aeson (FromJSON)
import Distribution.Pretty (Pretty (pretty))
import Exon (exon)

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Pretty (hpretty, prettyNt)

newtype HackageHost =
  HackageHost Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON)

instance Pretty HackageHost where
  pretty = prettyNt

data HackageTls =
  TlsOn
  |
  TlsOff
  deriving stock (Eq, Show)

instance Pretty HackageTls where
  pretty = \case
    TlsOn -> "on"
    TlsOff -> "off"

hackageTls :: Bool -> HackageTls
hackageTls = \case
  True -> TlsOn
  False -> TlsOff

hackageTlsBool :: HackageTls -> Bool
hackageTlsBool = \case
  TlsOn -> True
  TlsOff -> False

hackageTlsScheme :: IsString a => HackageTls -> a
hackageTlsScheme = \case
  TlsOn -> "https"
  TlsOff -> "http"

newtype HackagePort =
  HackagePort Word16
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance Pretty HackagePort where
  pretty (HackagePort p) = hpretty p

parseHackagePort :: String -> Maybe HackagePort
parseHackagePort = fmap HackagePort . readMaybe

newtype HackageUser =
  HackageUser Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, EncodeNix)

instance Pretty HackageUser where
  pretty = prettyNt

newtype HackageSecret =
  HackageSecret { text :: Text }
  deriving stock (Eq)
  deriving newtype (IsString, Ord, FromJSON)

instance Show HackageSecret where
  showsPrec d _ = showParen (d > 10) (showString "HackagePassword <password>")

instance Pretty HackageSecret where
  pretty _ = "<password>"

newtype HackagePassword =
  HackagePassword { secret :: HackageSecret }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, FromJSON)

newtype HackageToken =
  HackageToken { secret :: HackageSecret }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, FromJSON)

data HackageAuth =
  HackageAuthPassword { user :: HackageUser, password :: HackagePassword }
  |
  HackageAuthToken { token :: HackageToken }
  deriving stock (Eq, Show)

data HackageLocation =
  HackageLocation {
    host :: HackageHost,
    tls :: HackageTls,
    port :: Maybe HackagePort,
    auth :: Maybe HackageAuth
  }
  deriving stock (Eq, Show, Generic)

hackageLocation :: HackageHost -> HackageTls -> HackageLocation
hackageLocation host tls =
  HackageLocation {
    host,
    tls,
    port = Nothing,
    auth = Nothing
  }

central :: HackageLocation
central = hackageLocation "hackage.haskell.org" TlsOn

instance Pretty HackageLocation where
  pretty HackageLocation {..} =
    [exon|#{hackageTlsScheme tls}://#{hpretty host}#{foldMap renderPort port}|]
    where
      renderPort p = [exon|:#{pretty p}|]
