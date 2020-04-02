{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Frut.Syntax.AST.Vanilla where

import Language.Frut.Syntax.AST.Generic
import Unsafe.Coerce (unsafeCoerce)

data Vanilla

type instance XLit Vanilla = ()

type instance XOp Vanilla = ()

type instance XExp Vanilla = ()

type ExpVanilla = ExpX Vanilla

deriving instance Eq ExpVanilla

deriving instance Show ExpVanilla

pattern LitVanilla :: Literal -> ExpVanilla
pattern LitVanilla i <-
  LitX _ i
  where
    LitVanilla i = LitX () i

pattern OpVanilla :: Operator -> ExpVanilla -> ExpVanilla -> ExpVanilla
pattern OpVanilla op e1 e2 <-
  OpX _ op e1 e2
  where
    OpVanilla op e1 e2 = OpX () op e1 e2

toVanilla :: ExpX ξ -> ExpVanilla
toVanilla = unsafeCoerce -- TODO: proper conversion
