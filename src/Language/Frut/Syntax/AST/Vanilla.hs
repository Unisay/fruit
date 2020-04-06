{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Frut.Syntax.AST.Vanilla where

import Data.Generics.Uniplate (Uniplate (..))
import Language.Frut.Syntax.AST.Generic
import Unsafe.Coerce (unsafeCoerce)

data Vanilla

type instance XLit Vanilla = ()

type instance XOp Vanilla = ()

type instance XScope Vanilla = ()

type instance XExp Vanilla = ()

type ExpVanilla = ExpX Vanilla

deriving instance Eq ExpVanilla

deriving instance Show ExpVanilla

instance Uniplate ExpVanilla where
  uniplate = \case
    ScopeX _ e ->
      ([e], \case [a'] -> ScopeVanilla a'; _ -> failMatch "ScopeVanilla")
    OpX _ op a b ->
      ([a, b], \case [a', b'] -> OpVanilla op a' b'; _ -> failMatch "OpVanilla")
    -- Case per leaf node in order not to disable exhaustveness checker
    x@ExpX {} -> (mempty, const x)
    x@LitX {} -> (mempty, const x)

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

pattern ScopeVanilla :: ExpVanilla -> ExpVanilla
pattern ScopeVanilla e <-
  ScopeX _ e
  where
    ScopeVanilla e = ScopeX () e

toVanilla :: ExpX ξ -> ExpVanilla
toVanilla = unsafeCoerce -- TODO: proper conversion
