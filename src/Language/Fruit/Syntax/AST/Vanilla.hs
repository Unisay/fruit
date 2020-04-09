{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Fruit.Syntax.AST.Vanilla where

import Data.Generics.Uniplate (Uniplate (..))
import Language.Fruit.Syntax.AST.Generic
import Unsafe.Coerce (unsafeCoerce)

data Vanilla

type instance XLit Vanilla = ()

type instance XVar Vanilla = ()

type instance XLet Vanilla = ()

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
    LetX _ var a b ->
      ( [a, b],
        \case
          [a', b'] -> LetVanilla var a' b'
          _ -> failMatch "LetVanilla"
      )
    -- Case per leaf node in order not to disable exhaustveness checker
    x@LitX {} -> (mempty, const x)
    x@VarX {} -> (mempty, const x)
    x@ExpX {} -> (mempty, const x)

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

pattern LetVanilla :: Var -> ExpVanilla -> ExpVanilla -> ExpVanilla
pattern LetVanilla var e1 e2 <-
  LetX _ var e1 e2
  where
    LetVanilla var e1 e2 = LetX () var e1 e2

pattern ScopeVanilla :: ExpVanilla -> ExpVanilla
pattern ScopeVanilla e <-
  ScopeX _ e
  where
    ScopeVanilla e = ScopeX () e

toVanilla :: ExpX ξ -> ExpVanilla
toVanilla = unsafeCoerce -- TODO: proper conversion
