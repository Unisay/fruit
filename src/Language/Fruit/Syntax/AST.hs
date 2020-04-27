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
import Language.Fruit.Data.Span ((#), Located (..), Span, Spanned)
import qualified Unbound.LocallyNameless as UB

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

data Definition
  = Definition Span Var Term
  deriving (Eq, Show, Generic)

data Term
  = TermApp Term Term
  | TermLam Span Var Term
  | TermLit Span Lit
  | TermVar Span Var
  | TermLet Span Var Term Term
  | TermITE Span Term Term Term
  | TermFun Span Fun Term Term
  | TermScope Span Term
  deriving (Eq, Show, Generic)

instance Located Term where
  spanOf = \case
    TermApp a b -> a # b
    TermLam sp _ _ -> sp
    TermLit sp _ -> sp
    TermVar sp _ -> sp
    TermLet sp _ _ _ -> sp
    TermITE sp _ _ _ -> sp
    TermFun sp _ _ _ -> sp
    TermScope sp _ -> sp

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
    x@(TermITE sp a b c) ->
      ([a, b, c], \case [a', b', c'] -> TermITE sp a' b' c'; _ -> x)
    x@(TermLam sp var a) ->
      ([a], \case [a'] -> TermLam sp var a'; _ -> x)
    -- Case per leaf node in order not to disable exhaustveness checker
    x@TermLit {} -> (mempty, const x)
    x@TermVar {} -> (mempty, const x)

data Lit
  = LitInteger Integer
  | LitFloating Double
  | LitBoolean Bool
  deriving stock (Eq, Ord, Show)

newtype Var = Var Ident
  deriving newtype (Eq, Ord, ToText, IsString)
  deriving stock (Show)

data Fun
  = Plus
  | Minus
  | Mul
  | Div
  | Pow
  deriving (Eq, Ord, Show)

translateDefinitionToCore :: Definition -> Core.Term -> Core.Term
translateDefinitionToCore (Definition _ (Var Ident {name}) term) =
  Core.Let (translateTermToCore term)
    . UB.bind (UB.string2Name (toString name))

translateTermToCore :: Term -> Core.Term
translateTermToCore = para \case
  TermApp {} ->
    \case
      [t1, t2] -> Core.App t1 t2
      _ -> err 2 "Core.TermApp"
  TermLam _ (Var Ident {name}) _ ->
    \case
      [t] -> Core.mkLam name t
      _ -> err 1 "Core.TermLam"
  TermLit _ lit ->
    const case lit of
      LitInteger i -> Core.LitInteger i
      LitFloating f -> Core.LitFloating f
      LitBoolean b -> Core.LitBoolean b
  TermVar _ (Var Ident {name}) ->
    const (Core.mkVar name)
  TermLet _ (Var Ident {name}) _ _ ->
    \case
      [t1, t2] -> Core.mkLet name t1 t2
      _ -> err 2 "Core.TermLam"
  TermITE {} ->
    \case
      [i, t, e] -> Core.mkIte i t e
      _ -> err 3 "Core.TermITE"
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
      _ -> err 1 "AST.TermScope"
  where
    err :: Int -> Text -> a
    err n c =
      error $
        "Language.Fruit.Syntax.AST.translateTermToCore: \
        \unexpected number of sub-terms received (/= "
          <> show n
          <> ") while traversing "
          <> c
          <> " constructor"
