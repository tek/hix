module Hix.NixExpr where

import Data.List.NonEmpty ((<|))
import qualified Data.Text as Text
import Exon (exon)

import Hix.Class.EncodeNix (EncodeNix (encodeNix))
import Hix.Data.NixExpr (Expr (..), ExprAttr (ExprAttr, ExprAttrNil))

indent ::
  Functor t =>
  Int ->
  t Text ->
  t Text
indent n =
  fmap (Text.replicate n " " <>)

withSemicolon :: NonEmpty Text -> NonEmpty Text
withSemicolon = \case
  e :| [] ->
    [e <> ";"]
  h :| h1 : t -> h <| withSemicolon (h1 :| t)

renderAttrs :: Int -> [ExprAttr] -> [Text]
renderAttrs ind attrs =
  attrs >>= \case
    ExprAttr k v ->
      case renderExpr ind v of
        e :| [] -> [[exon|#{k} = #{e};|]]
        h :| (h1 : t) -> [exon|#{k} = #{h}|] : toList (withSemicolon (h1 :| t))
    ExprAttrNil ->
      []

renderExpr :: Int -> Expr -> NonEmpty Text
renderExpr ind = \case
  ExprString s -> indent ind [[exon|"#{Text.replace "\"" "\\\"" s}"|]]
  ExprLit e -> [e]
  ExprList l -> "[" :| (indent (ind + 2) (toList . renderExpr ind =<< l)) ++ ["]"]
  ExprAttrs a -> case renderAttrs ind a of
    [] -> ["{}"]
    as -> "{" :| indent (ind + 2) as ++ ["}"]
  ExprPrefix p (renderExpr ind -> h :| t) ->
    [exon|#{p} #{h}|] :| t

renderRootExpr :: Expr -> Text
renderRootExpr =
  Text.unlines . toList . renderExpr 0

checkEmpty ::
  Text ->
  Expr ->
  ExprAttr
checkEmpty key = \case
  ExprString value | Text.null value ->
    ExprAttrNil
  ExprList value | null value ->
    ExprAttrNil
  value ->
    ExprAttr key value

singleOpt ::
  EncodeNix a =>
  Text ->
  (e -> Maybe a) ->
  e ->
  ExprAttr
singleOpt key get entity =
  maybe ExprAttrNil (checkEmpty key . encodeNix) (get entity)

single ::
  EncodeNix a =>
  Text ->
  (e -> a) ->
  e ->
  ExprAttr
single key get =
  singleOpt key (Just . get)

multiOpt ::
  EncodeNix a =>
  Text ->
  (e -> Maybe [a]) ->
  e ->
  ExprAttr
multiOpt key get entity =
  maybe ExprAttrNil (checkEmpty key . encodeNix) (get entity)

multi ::
  EncodeNix a =>
  Text ->
  (e -> [a]) ->
  e ->
  ExprAttr
multi key get =
  multiOpt key (Just . get)

multiOrSingle ::
  ∀ a e .
  EncodeNix a =>
  Text ->
  (e -> [a]) ->
  e ->
  ExprAttr
multiOrSingle key get entity =
  check (get entity)
  where
    check :: [a] -> ExprAttr
    check [] = ExprAttrNil
    check [sing] = ExprAttr key (encodeNix sing)
    check values = ExprAttr key (encodeNix values)

mkAttrs :: [e -> ExprAttr] -> e -> [ExprAttr]
mkAttrs a e =
  (fmap ($ e) a)

notNil :: ExprAttr -> Bool
notNil = \case
  ExprAttrNil -> False
  _ -> True

nonEmptyAttrs :: Text -> [ExprAttr] -> ExprAttr
nonEmptyAttrs key =
  filter notNil >>> \case
    [] -> ExprAttrNil
    as -> ExprAttr key (ExprAttrs as)
