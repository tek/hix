module Hix.Data.NixExpr where

newtype ExprKey =
  ExprKey Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, Semigroup, Monoid)

data ExprAttr =
  ExprAttr {
    name :: ExprKey,
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

exprAttrs :: [(ExprKey, Expr)] -> Expr
exprAttrs = ExprAttrs . fmap (uncurry ExprAttr)

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
