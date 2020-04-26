{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Js.Syntax where

import Data.Generics.Uniplate (Uniplate (..))
import qualified Data.Map as Map

data Lexeme
  = LexemeOperator
  | LexemeLiteral
  | LexemeIdentifier
  | LexemeKeyword
  deriving (Eq, Show)

data Term
  = TermVar Var
  | TermLit Lit
  | TermOperator Operator Term Term
  | TermConditional Term Term Term
  | TermLambda Pattern Term
  | TermFunction Var [Var] Term
  | TermCall Term [Term]
  | TermLet (Map Var Term)
  | TermBlock [Term]
  deriving (Eq, Show, Generic)

instance Uniplate Term where
  uniplate = \case
    x@TermVar {} ->
      (mempty, const x)
    x@TermLit {} ->
      (mempty, const x)
    TermOperator op a b ->
      ( [a, b],
        \case
          [a', b'] -> TermOperator op a' b'
          _ -> err 2 "TermOperator"
      )
    TermConditional a b c ->
      ( [a, b, c],
        \case
          [a', b', c'] -> TermConditional a' b' c'
          _ -> err 3 "TermConditional"
      )
    TermLambda pat a ->
      ( [a],
        \case
          [a'] -> TermLambda pat a'
          _ -> err 1 "TermLambda"
      )
    TermFunction var args a ->
      ( [a],
        \case
          [a'] -> TermFunction var args a'
          _ -> err 1 "TermFunction"
      )
    TermCall a b ->
      ( a : b,
        \case
          a' : b' -> TermCall a' b'
          _ -> err 2 "TermCall"
      )
    TermLet binds ->
      let (vars, terms) = unzip (Map.toList binds)
       in (terms, TermLet . Map.fromList . zip vars)
    TermBlock terms ->
      (terms, TermBlock)
    where
      err :: Int -> Text -> a
      err n c =
        error $
          "Language.Fruit.Js.Syntax.uniplate: \
          \unexpected number of sub-terms received (/= "
            <> show n
            <> ") while traversing "
            <> c
            <> " constructor"

data Operator
  = Plus
  | Minus
  | Mul
  | Div
  | Pow
  deriving (Eq, Show)

newtype Name a = Name Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToText, Hashable)

newtype Var = Var Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToText, Hashable)

data Lit = LitNumber Double | LitBigInt Integer | LitBoolean Bool
  deriving (Eq, Show)

newtype Pattern = PatternVar Var
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToText, Hashable)
