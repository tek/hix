module Hix.Class.EncodeNix where

import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import Distribution.Utils.ShortText (ShortText, fromShortText)
import Distribution.Version (Version, VersionRange, simplifyVersionRange)
import Generics.SOP (All2, K (K), SListI2, hcmap, hcollapse)
import Text.PrettyPrint (Doc)

import Hix.Class.SOP (Field (Field), FieldK (FieldK), ToFields (toFields))
import Hix.Data.NixExpr (Expr (..), ExprAttr (ExprAttr), ViaPretty (ViaPretty), exprBool, exprShow)

type EncodeField :: FieldK -> Constraint
class EncodeField field where
  encodeField :: Field field -> ExprAttr

instance (
    KnownSymbol name,
    EncodeNix a
  ) => EncodeField ('FieldK name a) where
    encodeField (Field a) =
      ExprAttr (toText (symbolVal (Proxy @name))) (encodeNix a)

class EncodeProd a where
  encodeProd :: a -> Expr

instance (
    ToFields a fields,
    All2 EncodeField fields,
    SListI2 fields
  ) => EncodeProd a where
    encodeProd =
      ExprAttrs .
      hcollapse .
      hcmap (Proxy @EncodeField) (K . encodeField) .
      toFields

class EncodeNixKey a where
  encodeNixKey :: a -> Text

instance EncodeNixKey Text where
  encodeNixKey = id

class EncodeNix a where
  encodeNix :: a -> Expr
  default encodeNix :: EncodeProd a => a -> Expr
  encodeNix = encodeProd

instance EncodeNix Expr where
  encodeNix = id

instance EncodeNix Bool where
  encodeNix = exprBool

instance EncodeNix Int where
  encodeNix = exprShow

instance EncodeNix Version where
  encodeNix = encodeNix . pretty

instance EncodeNix VersionRange where
  encodeNix = encodeNix . pretty . simplifyVersionRange

instance EncodeNix a => EncodeNix [a] where
  encodeNix = ExprList . fmap encodeNix

instance EncodeNix a => EncodeNix (NonEmpty a) where
  encodeNix = ExprList . toList . fmap encodeNix

instance (
    EncodeNixKey k,
    EncodeNix v
  ) => EncodeNix (Map k v) where
    encodeNix = ExprAttrs . fmap (uncurry ExprAttr . bimap encodeNixKey encodeNix) . Map.toList

instance EncodeNix Doc where
  encodeNix = ExprString . show

instance EncodeNix Text where
  encodeNix = ExprString

instance EncodeNix ShortText where
  encodeNix = encodeNix . toText . fromShortText

instance Pretty a => EncodeNix (ViaPretty a) where
  encodeNix (ViaPretty a) = encodeNix (pretty a)
