module Hix.NixCode where

import Exon (ToSegment (..), exonWith)
import Language.Haskell.TH.Quote (QuasiQuoter)

import Hix.Class.EncodeNix (EncodeNix (encodeNix))
import Hix.NixExpr (renderRootExpr)

newtype NixCode =
  NixCode { code :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, Semigroup, Monoid)

nixCode :: NixCode -> Text
nixCode NixCode {code} = code

nixon :: QuasiQuoter
nixon = exonWith (Just ([e|NixCode|], [e|id|])) True False

instance EncodeNix a => ToSegment a NixCode where
  toSegment = NixCode . renderRootExpr . encodeNix
