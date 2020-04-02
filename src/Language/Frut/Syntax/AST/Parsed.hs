{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Frut.Syntax.AST.Parsed where

import Language.Frut.Data.Span (Span)
import Language.Frut.Syntax.AST.Generic

data Parsed

type instance XLit Parsed = Span

type instance XOp Parsed = Span

type instance XExp Parsed = Void

type ExpParsed = ExpX Parsed

deriving instance Eq ExpParsed

deriving instance Show ExpParsed

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
