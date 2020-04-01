{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Frut.Syntax.AST.Undecorated where

import Language.Frut.Syntax.AST.Generic

data Undecorated

type instance XLit Undecorated = Void

type instance XOp Undecorated = Void

type instance XExp Undecorated = Void

type ExpUD = ExpX Undecorated

pattern LitUD :: Literal -> ExpUD
pattern LitUD i <-
  LitX _ i
  where
    LitUD i = LitX (error "Attempt to evaluate void") i

pattern OpUD :: Operator -> ExpUD -> ExpUD -> ExpUD
pattern OpUD op e1 e2 <-
  OpX _ op e1 e2
  where
    OpUD op e1 e2 = OpX (error "Attempt to evaluate void") op e1 e2
