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

type instance XApp Parsed = ()

type instance XLit Parsed = Span

type instance XVar Parsed = Span

type instance XLet Parsed = Span

type instance XOp Parsed = Span

type instance XScope Parsed = Span

type instance XExp Parsed = Void

type ExpParsed = ExpX Parsed

deriving instance Eq ExpParsed

deriving instance Show ExpParsed

instance Uniplate ExpParsed where
  uniplate = \case
    AppX sp a b ->
      ( [a, b],
        \case
          [a', b'] -> AppX sp a' b'
          _ -> failMatch "AppX"
      )
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
    LetX sp var a b ->
      ( [a, b],
        \case
          [a', b'] -> LetX sp var a' b'
          _ -> failMatch "LetX"
      )
    -- Case per leaf node in order not to disable exhaustveness checker
    x@(LitX _ _) -> (mempty, const x)
    x@(VarX _ _) -> (mempty, const x)

pattern AppParsed :: ExpParsed -> ExpParsed -> ExpParsed
pattern AppParsed e1 e2 <-
  AppX _ e1 e2
  where
    AppParsed e1 e2 = AppX () e1 e2

pattern LitParsed :: Span -> Literal -> ExpParsed
pattern LitParsed span i <-
  LitX span i
  where
    LitParsed span i = LitX span i

pattern VarParsed :: Span -> Var -> ExpParsed
pattern VarParsed span i <-
  VarX span i
  where
    VarParsed span i = VarX span i

pattern LetParsed :: Span -> Var -> ExpParsed -> ExpParsed -> ExpParsed
pattern LetParsed span var e1 e2 <-
  LetX span var e1 e2
  where
    LetParsed span var e1 e2 = LetX span var e1 e2

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
