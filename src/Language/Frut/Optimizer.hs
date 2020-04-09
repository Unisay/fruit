{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Frut.Optimizer where

import Data.Foldable (foldl1)
import Data.Generics.Uniplate (rewrite)
import Language.Frut.Syntax.AST
import Language.Frut.Syntax.Precedence (associatesLeft, associatesRight, prec)

optimize :: ExpParsed -> ExpParsed
optimize =
  rewrite . foldl1 (liftA2 (<|>)) $
    removeRedundantParens :| [foldConstants]

removeRedundantParens :: ExpParsed -> Maybe ExpParsed
removeRedundantParens = \case
  OpX x op e1 (ScopeX _ (OpX x' op' e1' e2'))
    | prec op <= prec op' && associatesRight op && associatesRight op' ->
      Just $ OpX x op e1 (OpX x' op' e1' e2')
  OpX x op (ScopeX _ (OpX x' op' e1' e2')) e2
    | prec op <= prec op' && associatesLeft op && associatesLeft op' ->
      Just $ OpX x op (OpX x' op' e1' e2') e2
  ScopeX _ (LitParsed x' lit) ->
    Just $ LitParsed x' lit
  _ -> Nothing

foldConstants :: ExpParsed -> Maybe ExpParsed
foldConstants = \case
  OpX spanOp op (LitX _ (LitInteger l)) (LitX _ (LitInteger r)) ->
    LitX spanOp
      <$> case op of
        OperatorPlus -> Just . LitInteger $ l + r
        OperatorMinus -> Just . LitInteger $ l - r
        OperatorTimes -> Just . LitInteger $ l * r
        OperatorPow -> Nothing -- Grows to fast
        OperatorDiv -> Nothing
  OpX spanOp op (LitX _ (LitFloating l)) (LitX _ (LitFloating r)) ->
    LitX spanOp
      <$> case op of
        OperatorPlus -> Just . LitFloating $ l + r
        OperatorMinus -> Just . LitFloating $ l - r
        OperatorTimes -> Just . LitFloating $ l * r
        OperatorPow -> Nothing
        OperatorDiv -> Just . LitFloating $ l / r
  _ -> Nothing
