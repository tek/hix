module Hix.NixExpr where

import Data.List.NonEmpty ((<|))
import qualified Data.Text as Text
import Distribution.Pretty (Pretty, pretty)
import Distribution.Utils.ShortText (ShortText, fromShortText)
import Exon (exon)

data ExprAttr =
  ExprAttr {
    name :: Text,
    value :: Expr
  }
  |
  ExprAttrNil
  deriving stock (Eq, Show, Generic)

data Expr =
  ExprString Text
  |
  ExprLit Text
  |
  ExprList [Expr]
  |
  ExprAttrs [ExprAttr]
  |
  ExprPrefix Text Expr
  deriving stock (Eq, Show, Generic)

exprShow :: Show a => a -> Expr
exprShow =
  ExprLit . show

exprBool :: Bool -> Expr
exprBool =
  ExprLit . \case
    True -> "true"
    False -> "false"

exprStrings :: [Text] -> Expr
exprStrings =
  ExprList . fmap ExprString

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

class RenderNix a where
  renderNix :: a -> Text

instance {-# overlappable #-} Pretty a => RenderNix a where
  renderNix = show . pretty

instance RenderNix ShortText where
  renderNix = toText . fromShortText

instance RenderNix String where
  renderNix = toText

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
  RenderNix a =>
  Text ->
  (e -> Maybe a) ->
  e ->
  ExprAttr
singleOpt key get entity =
  maybe ExprAttrNil (checkEmpty key . ExprString) (renderNix <$> get entity)

single ::
  RenderNix a =>
  Text ->
  (e -> a) ->
  e ->
  ExprAttr
single key get =
  singleOpt key (Just . get)

multiOpt ::
  RenderNix a =>
  Text ->
  (e -> Maybe [a]) ->
  e ->
  ExprAttr
multiOpt key get entity =
  maybe ExprAttrNil (checkEmpty key . exprStrings) (fmap renderNix <$> get entity)

multi ::
  RenderNix a =>
  Text ->
  (e -> [a]) ->
  e ->
  ExprAttr
multi key get =
  multiOpt key (Just . get)

multiOrSingle ::
  ∀ a e .
  RenderNix a =>
  Text ->
  (e -> [a]) ->
  e ->
  ExprAttr
multiOrSingle key get entity =
  check (renderNix <$> get entity)
  where
    check :: [Text] -> ExprAttr
    check [] = ExprAttrNil
    check [sing] = ExprAttr key (ExprString sing)
    check values = ExprAttr key (exprStrings values)

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
