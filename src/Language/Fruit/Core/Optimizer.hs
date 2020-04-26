{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Core.Optimizer where

import Data.Foldable (foldl1)
import Data.Generics.Uniplate (rewrite)
import Language.Fruit.Core.Types
import Unbound.LocallyNameless (runFreshM, unbind)

type Reduction = Term -> Maybe Term

optimize :: Term -> Term
optimize = rewrite . foldl1 (liftA2 (<|>)) $ etaReduce :| []

etaReduce :: Reduction
etaReduce = \case
  Let term bind
    | (p, Var p') <- runFreshM (unbind bind), p == p' -> Just term
  _ -> Nothing
