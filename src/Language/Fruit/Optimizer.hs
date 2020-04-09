{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Optimizer where

import Data.Foldable (foldl1)
import Data.Generics.Uniplate (rewrite)
import Language.Fruit.Syntax.AST
import Language.Fruit.Syntax.Precedence
  ( associatesLeft,
    associatesRight,
    prec,
  )

type Reduction = ExpParsed -> Maybe ExpParsed

optimize :: ExpParsed -> ExpParsed
optimize =
  rewrite . foldl1 (liftA2 (<|>)) $
    removeRedundantParens
      :| [ replaceSubtractionBySummation,
           foldConstants,
           zeroLaws,
           oneLaws
         ]

removeRedundantParens :: Reduction
removeRedundantParens = \case
  OpX x op e1 (ScopeX _ (OpX x' op' e1' e2'))
    | prec op <= prec op' && associatesRight op && associatesRight op' ->
      Just $ OpX x op e1 (OpX x' op' e1' e2')
  OpX x op (ScopeX _ (OpX x' op' e1' e2')) e2
    | prec op <= prec op' && associatesLeft op && associatesLeft op' ->
      Just $
        OpX x op (OpX x' op' e1' e2') e2
  ScopeX _ (LitParsed x lit) -> Just $ LitParsed x lit
  ScopeX _ (VarParsed x var) -> Just $ VarParsed x var
  _ -> Nothing

replaceSubtractionBySummation :: Reduction
replaceSubtractionBySummation = \case
  OpX sp OperatorMinus e (LitX sp' (LitInteger i)) ->
    Just $ OpX sp OperatorPlus e (LitX sp' (LitInteger (negate i)))
  OpX sp OperatorMinus (LitX sp' (LitInteger i)) e ->
    Just $ OpX sp OperatorPlus (LitX sp' (LitInteger (negate i))) e
  OpX sp OperatorMinus e (LitX sp' (LitFloating i)) ->
    Just $ OpX sp OperatorPlus e (LitX sp' (LitFloating (negate i)))
  OpX sp OperatorMinus (LitX sp' (LitFloating i)) e ->
    Just $ OpX sp OperatorPlus (LitX sp' (LitFloating (negate i))) e
  _ -> Nothing

zeroLaws :: Reduction
zeroLaws = \case
  OpX _ OperatorPlus (LitX _ (LitInteger 0)) e -> Just e
  OpX _ OperatorPlus e (LitX _ (LitInteger 0)) -> Just e
  OpX _ OperatorPlus (LitX _ (LitFloating 0)) e -> Just e
  OpX _ OperatorPlus e (LitX _ (LitFloating 0)) -> Just e
  OpX _ OperatorTimes z@(LitX _ (LitInteger 0)) _ -> Just z
  OpX _ OperatorTimes _ z@(LitX _ (LitInteger 0)) -> Just z
  OpX _ OperatorTimes z@(LitX _ (LitFloating 0)) _ -> Just z
  OpX _ OperatorTimes _ z@(LitX _ (LitFloating 0)) -> Just z
  _ -> Nothing

oneLaws :: Reduction
oneLaws = \case
  OpX _ OperatorTimes (LitX _ (LitInteger 1)) e -> Just e
  OpX _ OperatorTimes e (LitX _ (LitInteger 1)) -> Just e
  OpX _ OperatorTimes (LitX _ (LitFloating 1)) e -> Just e
  OpX _ OperatorTimes e (LitX _ (LitFloating 1)) -> Just e
  OpX _ OperatorDiv e (LitX _ (LitInteger 1)) -> Just e
  OpX _ OperatorDiv e (LitX _ (LitFloating 1)) -> Just e
  _ -> Nothing

foldConstants :: Reduction
foldConstants = \case
  OpX spanOp op (LitX _ (LitInteger l)) (LitX _ (LitInteger r)) ->
    LitX spanOp <$> case op of
      OperatorPlus -> Just . LitInteger $ l + r
      OperatorMinus -> Just . LitInteger $ l - r
      OperatorTimes -> Just . LitInteger $ l * r
      OperatorPow -> Nothing -- Grows too fast
      OperatorDiv -> Nothing
  OpX spanOp op (LitX _ (LitFloating l)) (LitX _ (LitFloating r)) ->
    LitX spanOp <$> case op of
      OperatorPlus -> Just . LitFloating $ l + r
      OperatorMinus -> Just . LitFloating $ l - r
      OperatorTimes -> Just . LitFloating $ l * r
      OperatorPow -> Nothing
      OperatorDiv -> Just . LitFloating $ l / r
  -- (e + c) + c  ->  e + c
  OpX sp op (OpX sp' op' e (LitX _ (LitInteger i'))) (LitX _ (LitInteger i))
    | op == op' && op == OperatorPlus ->
      Just $ OpX (sp' <> sp) OperatorPlus e $
        LitX (sp' <> sp) (LitInteger (i + i'))
  -- (c + e) + c  ->  e + c
  OpX sp op (OpX sp' op' (LitX _ (LitInteger i')) e) (LitX _ (LitInteger i))
    | op == op' && op == OperatorPlus ->
      Just $ OpX (sp' <> sp) OperatorPlus e $
        LitX (sp' <> sp) (LitInteger (i + i'))
  -- c + (e + c)  ->  e + c
  OpX sp op (LitX _ (LitInteger i)) (OpX sp' op' e (LitX _ (LitInteger i')))
    | op == op' && op == OperatorPlus ->
      Just $ OpX (sp' <> sp) OperatorPlus e $
        LitX (sp' <> sp) (LitInteger (i + i'))
  -- c + (c + e)  ->  e + c
  OpX sp op (LitX _ (LitInteger i)) (OpX sp' op' (LitX _ (LitInteger i')) e)
    | op == op' && op == OperatorPlus ->
      Just $ OpX (sp' <> sp) OperatorPlus e $
        LitX (sp' <> sp) (LitInteger (i + i'))
  _ -> Nothing


