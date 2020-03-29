{-# LANGUAGE LambdaCase #-}

module Language.Frut.Syntax.Precedence
  ( Precedence,
    prec,
    Associativity (..),
    assoc,
  )
where

import qualified Language.Frut.Syntax.AST as AST

type Precedence = Natural

data Associativity
  = NonAssoc
  | LeftAssoc
  | RightAssoc
  | FullAssoc
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | https://en.wikipedia.org/wiki/Operator_associativity
assoc :: AST.InfixOp -> Associativity
assoc = \case
  AST.InfixPlus -> FullAssoc
  AST.InfixMinus -> LeftAssoc
  AST.InfixTimes -> FullAssoc
  AST.InfixDiv -> LeftAssoc
  AST.InfixPow -> RightAssoc

prec :: AST.InfixOp -> Precedence
prec = \case
  AST.InfixPlus -> 1
  AST.InfixMinus -> 1
  AST.InfixTimes -> 2
  AST.InfixDiv -> 2
  AST.InfixPow -> 3

-- |
-- >>> reassoc :{
--       ExprInfixOp
--         InfixPlus
--         ExprLiteral (LiteralDecimal 1)
--         ExprInfixOp
--           InfixPlus
--           (ExprLiteral (LiteralDecimal 2))
--           (ExprLiteral (LiteralDecimal 3))
-- :}
-- ExprInfixOp
--   InfixPlus
--   ExprInfixOp
--     InfixPlus
--     (ExprLiteral (LiteralDecimal 2))
--     (ExprLiteral (LiteralDecimal 3))
--   ExprLiteral (LiteralDecimal 1)
reassoc :: AST.Expr -> AST.Expr
reassoc = undefined
