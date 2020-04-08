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
  OpX spanOp op (LitX _ (Literal l)) (LitX _ (Literal r)) ->
    LitX spanOp <$> case op of
      OperatorPlus -> Just . Literal $ l + r
      OperatorMinus -> Just . Literal $ l - r
      OperatorTimes -> Just . Literal $ l * r
      OperatorPow -> Just . Literal $ l ^ r
      OperatorDiv -> Nothing
  _ -> Nothing
