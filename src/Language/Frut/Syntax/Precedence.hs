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
assoc :: AST.Operator -> Associativity
assoc = \case
  AST.OperatorPlus -> FullAssoc
  AST.OperatorMinus -> LeftAssoc
  AST.OperatorTimes -> FullAssoc
  AST.OperatorDiv -> LeftAssoc
  AST.OperatorPow -> RightAssoc

prec :: AST.Operator -> Precedence
prec = \case
  AST.OperatorPlus -> 1
  AST.OperatorMinus -> 1
  AST.OperatorTimes -> 2
  AST.OperatorDiv -> 2
  AST.OperatorPow -> 3

-- |
-- >>> reassoc :{
--       ExprOperator
--         OperatorPlus
--         ExprLiteral (LiteralDecimal 1)
--         ExprOperator
--           OperatorPlus
--           (ExprLiteral (LiteralDecimal 2))
--           (ExprLiteral (LiteralDecimal 3))
-- :}
-- ExprOperator
--   OperatorPlus
--   ExprOperator
--     OperatorPlus
--     (ExprLiteral (LiteralDecimal 2))
--     (ExprLiteral (LiteralDecimal 3))
--   ExprLiteral (LiteralDecimal 1)
reassoc :: AST.ExpX ξ -> AST.ExpX ξ
reassoc = undefined
