module Hix.Managed.Cabal.Data.ContextHackageRepo where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import Text.PrettyPrint

import Hix.Class.EncodeNix (EncodeNix (encodeNix))
import Hix.Data.NixExpr (Expr (..), ExprAttr (..))
import Hix.Managed.Cabal.Data.HackageLocation (HackageSecret (..), HackageUser)
import Hix.Managed.Cabal.Data.HackageRepo (HackageDescription, HackageIndexState, HackageName)
import Hix.NixExpr (mkAttrs, single, singleOpt)
import Hix.Pretty (field, prettyFieldsV, prettyText)

newtype ContextHackageLocation =
  ContextHackageLocation Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, FromJSON, EncodeNix)

instance Pretty ContextHackageLocation where
  pretty = prettyText . coerce

data ContextHackageSecret =
  -- | Password was intended to be printed, most likely in a test.
  SecretUnobscured HackageSecret
  |
  SecretPlain HackageSecret
  |
  SecretEnvVar Text
  |
  SecretExec Text
  deriving stock (Eq, Show)

instance Pretty ContextHackageSecret where
  pretty = \case
    SecretUnobscured (HackageSecret pw) -> prettyText pw
    SecretPlain _ -> "<password>"
    SecretEnvVar var -> prettyText var <+> brackets (text "env-var")
    SecretExec exe -> prettyText exe <+> brackets (text "exec")

instance EncodeNix ContextHackageSecret where
  encodeNix = \case
    SecretUnobscured (HackageSecret pw) -> ExprString pw
    SecretPlain _ -> ExprString "<password>"
    SecretEnvVar var -> structured "env-var" var
    SecretExec exe -> structured "exec" exe
    where
      structured t value =
        ExprAttrs [
          ExprAttr "type" (ExprString t),
          ExprAttr {name = "value", value = ExprString value}
        ]

instance FromJSON ContextHackageSecret where
  parseJSON v =
    withText "ContextHackageSecret" plain v
    <|>
    withObject "ContextHackageSecret" typed v
    where
      typed o = do
        value <- o .: "value"
        o .: "type" >>= \case
          ("plain" :: Text) -> plain value
          "env-var" -> pure (SecretEnvVar value)
          "exec" -> pure (SecretExec value)
          t -> fail [exon|Invalid value for Hackage password type: ##{t}|]

      plain = pure . SecretPlain . HackageSecret

newtype ContextHackagePassword =
  ContextHackagePassword { secret :: ContextHackageSecret }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Pretty, EncodeNix, FromJSON)

newtype ContextHackageToken =
  ContextHackageToken { secret :: ContextHackageSecret }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Pretty, EncodeNix, FromJSON)

data ContextHackageRepo =
  ContextHackageRepo {
    name :: HackageName,
    description :: Maybe HackageDescription,
    enable :: Maybe Bool,
    location :: Maybe ContextHackageLocation,
    user :: Maybe HackageUser,
    password :: Maybe ContextHackagePassword,
    token :: Maybe ContextHackageToken,
    secure :: Maybe Bool,
    keys :: Maybe (NonEmpty Text),
    indexState :: Maybe HackageIndexState,
    solver :: Maybe Bool,
    publish :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance Pretty ContextHackageRepo where
  pretty ContextHackageRepo {..} =
    prettyFieldsV [
      field "name" name,
      field "description" description,
      field "enable" enable,
      field "location" location,
      field "user" user,
      field "password" password,
      field "token" token,
      field "secure" secure,
      field "keys" keys,
      field "indexState" indexState,
      field "solver" solver,
      field "publish" publish
    ]

instance EncodeNix ContextHackageRepo where
  encodeNix =
    ExprAttrs . mkAttrs [
      single "name" (.name),
      singleOpt "description" (.description),
      singleOpt "enable" (.enable),
      singleOpt "location" (.location),
      singleOpt "user" (.user),
      singleOpt "password" (.password),
      singleOpt "secure" (.secure),
      singleOpt "keys" (.keys),
      singleOpt "indexState" (.indexState),
      singleOpt "solver" (.solver),
      singleOpt "publish" (.publish)
    ]

contextHackageRepo :: HackageName -> ContextHackageRepo
contextHackageRepo name =
  ContextHackageRepo {
    name,
    description = Nothing,
    enable = Nothing,
    location = Nothing,
    user = Nothing,
    password = Nothing,
    token = Nothing,
    secure = Nothing,
    keys = Nothing,
    indexState = Nothing,
    solver = Nothing,
    publish = Nothing
  }
