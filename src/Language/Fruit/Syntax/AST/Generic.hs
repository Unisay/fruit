{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Fruit.Syntax.AST.Generic where

import Language.Fruit.Data.Ident (Ident)
import Language.Fruit.Data.Span (Spanned)

-- | This is the fundamental unit of parsing -
-- it represents the contents of one source file.
newtype SourceFile
  = SourceFile Module
  deriving (Eq, Ord, Show)

type QualifiedName = NonEmpty (Spanned Ident)

data Module
  = Module QualifiedName Exports [Import]
  deriving (Eq, Ord, Show)

newtype Exports
  = Exports (NonEmpty (Spanned Ident))
  deriving (Eq, Ord, Show)

data Import
  = Import QualifiedName [Spanned Ident]
  deriving (Eq, Ord, Show)

-- | Najd S., Jones S.P.: Trees that Grow
data ExpX ξ
  = AppX (XApp ξ) (ExpX ξ) (ExpX ξ)
  | LitX (XLit ξ) Literal
  | VarX (XVar ξ) Var
  | LetX (XLet ξ) Var (ExpX ξ) (ExpX ξ)
  | OpX (XOp ξ) Operator (ExpX ξ) (ExpX ξ)
  | ScopeX (XScope ξ) (ExpX ξ)
  | ExpX !(XExp ξ)

type family XApp ξ

type family XLit ξ

type family XVar ξ

type family XLet ξ

type family XOp ξ

type family XScope ξ

type family XExp ξ

failMatch :: Text -> a
failMatch s =
  error $ "Uniplate call with unexpected number of list elements for " <> s

mkLitX :: Literal -> ExpX ξ
mkLitX = LitX (error "Attempt to evaluate void (LitX)")

mkOpX :: Operator -> ExpX ξ -> ExpX ξ -> ExpX ξ
mkOpX = OpX (error "Attempt to evaluate void (OpX)")

data Literal
  = LitInteger Integer
  | LitFloating Double
  deriving stock (Eq, Ord, Show)

data Var
  = VarQualified QualifiedName
  | VarUnqualified Ident
  deriving (Eq, Ord, Show)

data Operator
  = OperatorPlus
  | OperatorMinus
  | OperatorTimes
  | OperatorDiv
  | OperatorPow
  deriving (Eq, Ord, Show)
