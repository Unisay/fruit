{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Frut.Optimizer where

import Data.Generics.Uniplate (rewrite)
import Language.Frut.Syntax.AST
import Language.Frut.Syntax.Precedence (associatesLeft, associatesRight, prec)

removeRedundantParens :: ExpParsed -> ExpParsed
removeRedundantParens = rewrite \case
  OpX x op e1 (ScopeX _ (OpX x' op' e1' e2'))
    | prec op <= prec op' && associatesRight op && associatesRight op' ->
      Just $ OpX x op e1 (OpX x' op' e1' e2')
  OpX x op (ScopeX _ (OpX x' op' e1' e2')) e2
    | prec op <= prec op' && associatesLeft op && associatesLeft op' ->
      Just $ OpX x op (OpX x' op' e1' e2') e2
  _ -> Nothing
