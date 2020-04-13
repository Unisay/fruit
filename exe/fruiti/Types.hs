{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import qualified Language.Fruit.Syntax.AST as AST

newtype Var = Var Text
  deriving newtype (Eq, Ord, Show)

type Env = Map Var AST.Term
