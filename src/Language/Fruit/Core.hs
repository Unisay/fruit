{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Fruit.Core where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus (mzero))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Generics.Uniplate (Uniplate (..))
import Unbound.LocallyNameless

type Nam = Name Term

data Term
  = Lit Integer
  | Var Nam
  | App Term Term
  | Lam (Bind Nam Term)
  deriving (Show)

instance Alpha Term

instance Subst Term Term where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing

instance Uniplate Term where
  uniplate = \case
    x@Lit {} ->
      ([], const x)
    x@Var {} ->
      ([], const x)
    x@(App t1 t2) ->
      ([t1, t2], \case [t1', t2'] -> App t1' t2'; _ -> x)
    x@(Lam b) -> runFreshM do
      (p, t) <- unbind b
      return ([t], \case [t'] -> Lam (bind p t'); _ -> x)

lam :: String -> Term -> Term
lam x t = Lam $ bind (string2Name x) t

var :: String -> Term
var = Var . string2Name

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
    step (Lit _) = mzero
    step (App (Lam b) t2) = do
      (p, t1) <- unbind b
      return $ subst p t2 t1
    step (App t1 t2) =
      App <$> step t1 <*> pure t2
        <|> App t1 <$> step t2

$(derive [''Term])
