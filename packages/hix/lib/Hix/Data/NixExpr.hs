module Hix.Data.NixExpr where

data ExprAttr =
  ExprAttr {
    name :: Text,
    value :: Expr
  }
  |
  ExprAttrNil
  deriving stock (Eq, Show, Generic)

data Expr =
  ExprNull
  |
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

newtype ViaPretty a =
  ViaPretty  a
  deriving stock (Eq, Show, Generic)
