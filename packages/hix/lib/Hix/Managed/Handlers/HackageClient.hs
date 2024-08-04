module Hix.Managed.Handlers.HackageClient where

import Data.Aeson (FromJSON)
import Exon (exon)
import Type.Reflection (typeRep)

import Hix.Data.Monad (M)
import Hix.Managed.Cabal.Data.Config (HackagePurpose)
import Hix.Managed.Cabal.Data.HackageRepo (HackageDescription)

type HackageResponse :: Type -> Type
data HackageResponse a where
  HackageResponseJson :: (FromJSON a, Typeable a) => HackageResponse a
  HackageResponseHtml :: HackageResponse Text
  HackageNoResponse :: HackageResponse ()

instance Eq (HackageResponse a) where
  HackageResponseJson == HackageResponseJson = True
  HackageResponseHtml == HackageResponseHtml = True
  HackageNoResponse == HackageNoResponse = True
  _ == _ = False

instance Show (HackageResponse a) where
  showsPrec d = \case
    HackageResponseJson -> showParen (d > 10) [exon|HackageResponseJson #{showsPrec 11 (typeRep @a)}|]
    HackageResponseHtml -> showString "HackageResponseHtml"
    HackageNoResponse -> showString "HackageNoResponse"

data HackageRequest a =
  HackageRequest {
    method :: Text,
    path :: Text,
    body :: Maybe (Either (NonEmpty (Text, Text)) LByteString),
    query :: Maybe (NonEmpty (ByteString, ByteString)),
    accept :: HackageResponse a
  }
  deriving stock (Eq, Show, Generic)

data HackageError =
  HackageNotFound
  |
  HackageFatal Text
  |
  HackageParseError Text
  deriving stock (Eq, Show, Generic)

data HackageClient =
  HackageClient {
    description :: HackageDescription,
    request :: ∀ a . HackageRequest a -> M (Either HackageError a)
  }

data HackageApi =
  HackageApi {
    request :: ∀ a . HackagePurpose -> HackageRequest a -> M (NonEmpty (Either HackageError a))
  }
