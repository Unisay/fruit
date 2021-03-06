{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Core
  ( module Reexport,
    eval,
  )
where

import Language.Fruit.Core.Optimizer as Reexport
import Language.Fruit.Core.Types as Reexport
import Unbound.LocallyNameless (FreshM, runFreshM, subst, unbind)

eval :: Term -> Term
eval x = runFreshM (tc step x)
  where
    tc :: (Monad m) => (a -> MaybeT m a) -> (a -> m a)
    tc f a =
      runMaybeT (f a) >>= \case
        Just a' -> tc f a'
        Nothing -> return a
    step :: Term -> MaybeT FreshM Term
    step (Var _) = mzero
    step (Lam _) = mzero
    step (LitInteger _) = mzero
    step (LitFloating _) = mzero
    step (LitBoolean _) = mzero
    step (Let arg bnd) =
      step (App (Lam bnd) arg)
    step (App (Lam bnd) arg) = do
      (p, ubnd) <- unbind bnd
      return $ subst p ubnd arg
    step (App t1 t2) =
      App <$> step t1 <*> pure t2
        <|> App t1 <$> step t2
