{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Frut.Syntax.AST.Parsed where

import Data.Generics.Uniplate (Uniplate (..))
import Language.Frut.Data.Span (Span)
import Language.Frut.Syntax.AST.Generic

data Parsed

type instance XLit Parsed = Span

type instance XOp Parsed = Span

type instance XExp Parsed = Void

type ExpParsed = ExpX Parsed

deriving instance Eq ExpParsed

deriving instance Show ExpParsed

instance Uniplate ExpParsed where
  uniplate (OpX xop op a b) =
    ( [a, b],
      \case
        [a', b'] -> OpX xop op a' b'
        _ ->
          error
            "Uniplate call with unexpected number of list elements \
            \for ExpParsed (OpX)"
    )
  -- Case per leaf node in order not to disable exhaustveness checker
  uniplate x@(ExpX _) = (mempty, const x)
  uniplate x@(LitX _ _) = (mempty, const x)

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
