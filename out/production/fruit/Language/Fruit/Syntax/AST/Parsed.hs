{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Fruit.Syntax.AST.Parsed where

import Data.Generics.Uniplate (Uniplate (..))
import Language.Fruit.Data.Span (Span)
import Language.Fruit.Syntax.AST.Generic

data Parsed

type instance XLit Parsed = Span

type instance XOp Parsed = Span

type instance XScope Parsed = Span

type instance XExp Parsed = Void

type ExpParsed = ExpX Parsed

deriving instance Eq ExpParsed

deriving instance Show ExpParsed

instance Uniplate ExpParsed where
  uniplate = \case
    ScopeX sp a ->
      ( [a],
        \case
          [a'] -> ScopeX sp a'
          _ -> failMatch "ScopeX"
      )
    OpX xop op a b ->
      ( [a, b],
        \case
          [a', b'] -> OpX xop op a' b'
          _ -> failMatch "OpX"
      )
    -- Case per leaf node in order not to disable exhaustveness checker
    x@(LitX _ _) -> (mempty, const x)

pattern LitParsed :: Span -> Literal -> ExpParsed
pattern LitParsed span i <-
  LitX span i
  where
    LitParsed span i = LitX span i

pattern OpParsed :: Span -> Operator -> ExpParsed -> ExpParsed -> ExpParsed
pattern OpParsed span op e1 e2 <-
  OpX span op e1 e2
  where
    OpParsed span op e1 e2 = OpX span op e1 e2

pattern ScopeParsed :: Span -> ExpParsed -> ExpParsed
pattern ScopeParsed span e <-
  ScopeX span e
  where
    ScopeParsed span e = ScopeX span e
