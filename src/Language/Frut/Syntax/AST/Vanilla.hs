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

type instance XExp Vanilla = ()

type ExpVanilla = ExpX Vanilla

deriving instance Eq ExpVanilla

deriving instance Show ExpVanilla

instance Uniplate ExpVanilla where
  uniplate (OpX _ op a b) =
    ( [a, b],
      \case
        [a', b'] -> OpX () op a' b'
        _ ->
          error
            "Uniplate call with unexpected number of list elements \
            \for ExpVanilla (OpX)"
    )
  -- Case per leaf node in order not to disable exhaustveness checker
  uniplate x@(ExpX _) = (mempty, const x)
  uniplate x@(LitX _ _) = (mempty, const x)

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
