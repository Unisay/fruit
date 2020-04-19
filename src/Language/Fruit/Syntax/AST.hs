{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Fruit.Syntax.AST where

import Data.Generics.Uniplate (Uniplate (..), para)
import qualified Language.Fruit.Core as Core
import Language.Fruit.Data.Ident (Ident (..))
import Language.Fruit.Data.Span (Span, Spanned)

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

data Term
  = TermApp Term Term
  | TermLam Span Var Term
  | TermLit Span Lit
  | TermVar Span Var
  | TermLet Span Var Term Term
  | TermFun Span Fun Term Term
  | TermScope Span Term
  deriving (Eq, Show, Generic)

instance Uniplate Term where
  uniplate = \case
    x@(TermApp a b) ->
      ([a, b], \case [a', b'] -> TermApp a' b'; _ -> x)
    x@(TermScope sp a) ->
      ([a], \case [a'] -> TermScope sp a'; _ -> x)
    x@(TermFun sp fun a b) ->
      ([a, b], \case [a', b'] -> TermFun sp fun a' b'; _ -> x)
    x@(TermLet sp var a b) ->
      ([a, b], \case [a', b'] -> TermLet sp var a' b'; _ -> x)
    x@(TermLam sp var a) ->
      ([a], \case [a'] -> TermLam sp var a'; _ -> x)
    -- Case per leaf node in order not to disable exhaustveness checker
    x@TermLit {} -> (mempty, const x)
    x@TermVar {} -> (mempty, const x)

data Lit
  = LitInteger Integer
  | LitFloating Double
  deriving stock (Eq, Ord, Show)

newtype Var = Var Ident
  deriving newtype (Eq, Ord)
  deriving stock (Show)

data Fun
  = Plus
  | Minus
  | Mul
  | Div
  | Pow
  deriving (Eq, Ord, Show)

translateToCore :: Term -> Core.Term
translateToCore = para \case
  TermApp {} ->
    \case
      [t1, t2] -> Core.App t1 t2
      _ -> err "Core.TermApp"
  TermLam _ (Var Ident {name}) _ ->
    \case
      [t] -> Core.mkLam name t
      _ -> err "Core.TermLam"
  TermLit _ lit ->
    const case lit of
      LitInteger i -> Core.LitInteger i
      LitFloating f -> Core.LitFloating f
  TermVar _ (Var Ident {name}) ->
    const (Core.mkVar name)
  TermLet _ (Var Ident {name}) _ _ ->
    \case
      [t1, t2] -> Core.mkLet name t1 t2
      _ -> err "Core.TermLam"
  TermFun _ fun _ _ ->
    foldl' Core.App . Core.mkVar $ case fun of
      Plus -> "plus"
      Minus -> "minus"
      Mul -> "mul"
      Div -> "div"
      Pow -> "pow"
  TermScope {} ->
    \case
      [t] -> t
      _ -> err "AST.TermScope"
  where
    err :: Text -> a
    err = error . ("Unexpected number of sub-terms: " <>)
