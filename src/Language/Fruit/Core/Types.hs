{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Fruit.Core.Types where

import Data.Generics.Uniplate (Uniplate (..))
import Unbound.LocallyNameless

type Nam = Name Term

instance IsString Nam where
  fromString = string2Name

data Term
  = LitInteger Integer
  | LitFloating Double
  | LitBoolean Bool
  | Var Nam
  | App Term Term
  | Lam (Bind Nam Term)
  | Let Term (Bind Nam Term)
  deriving (Show)

instance Alpha Term

instance Subst Term Term where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing

instance Uniplate Term where
  uniplate = \case
    x@LitInteger {} ->
      ([], const x)
    x@LitFloating {} ->
      ([], const x)
    x@LitBoolean {} ->
      ([], const x)
    x@Var {} ->
      ([], const x)
    App a b ->
      ( [a, b],
        \case
          [a', b'] -> App a' b'
          _ -> err 2 "App"
      )
    Lam b -> runFreshM do
      (p, t) <- unbind b
      return
        ( [t],
          \case
            [t'] -> Lam (bind p t')
            _ -> err 1 "Lam"
        )
    Let a t -> runFreshM do
      (p, b) <- unbind t
      return
        ( [a, b],
          \case
            [a', b'] -> Let a' (bind p b')
            _ -> err 2 "Let"
        )
    where
      err :: Int -> Text -> a
      err n c =
        error $
          "Language.Fruit.Core.Types.Term.uniplate: \
          \unexpected number of sub-terms received (/= "
            <> show n
            <> ") while traversing "
            <> c
            <> " constructor"

mkLam :: String -> Term -> Term
mkLam = mkLamNam . string2Name

mkLamNam :: Nam -> Term -> Term
mkLamNam = (Lam .) . bind

mkLet :: String -> Term -> Term -> Term
mkLet x t = Let t . bind (string2Name x)

mkApp2 :: Term -> Term -> Term -> Term
mkApp2 = (App .) . App

mkApp3 :: Term -> Term -> Term -> Term -> Term
mkApp3 = ((App .) .) . mkApp2

mkIte :: Term -> Term -> Term -> Term
mkIte = mkApp3 (mkVar "IF")

mkVar :: String -> Term
mkVar = Var . string2Name

$(derive [''Term])
