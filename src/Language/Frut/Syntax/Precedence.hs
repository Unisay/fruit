{-# LANGUAGE LambdaCase #-}

module Language.Frut.Syntax.Precedence
  ( prec,
  )
where

import qualified Language.Frut.Syntax.AST as AST

data Precedence = P1 | P2
  deriving (Eq, Ord, Show)

prec :: AST.Expr -> Natural
prec = \case
  AST.ExprLiteral {} -> 1
  AST.ExprInfixOp {} -> 2
