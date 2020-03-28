{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Frut.Syntax.AST where

import Language.Frut.Data.Ident (Ident)

-- | This is the fundamental unit of parsing -
-- it represents the contents of one source file.
newtype SourceFile
  = SourceFile Module
  deriving (Eq, Ord, Show)

type QualifiedName = NonEmpty Ident

data Module
  = Module QualifiedName Exports [Import]
  deriving (Eq, Ord, Show)

newtype Exports
  = Exports (NonEmpty Ident)
  deriving (Eq, Ord, Show)

data Import
  = Import QualifiedName [Ident]
  deriving (Eq, Ord, Show)

data Expr
  = ExprLiteral Literal
  | ExprInfixOp InfixOp Expr Expr
  deriving (Eq, Ord, Show)

newtype Literal
  = LiteralDecimal Integer
  deriving stock (Eq, Ord, Show)

data InfixOp
  = InfixPlus
  | InfixMinus
  | InfixTimes
  | InfixDiv
  deriving (Eq, Ord, Show)
