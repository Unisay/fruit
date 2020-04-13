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

type Reduction = Term -> Maybe Term

optimize :: Term -> Term
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
  TermFun x fun e1 (TermScope _ (TermFun x' fun' e1' e2'))
    | prec fun <= prec fun' && associatesRight fun && associatesRight fun' ->
      Just $ TermFun x fun e1 (TermFun x' fun' e1' e2')
  TermFun x fun (TermScope _ (TermFun x' fun' e1' e2')) e2
    | prec fun <= prec fun' && associatesLeft fun && associatesLeft fun' ->
      Just $
        TermFun x fun (TermFun x' fun' e1' e2') e2
  TermScope _ (TermLit x lit) -> Just $ TermLit x lit
  TermScope _ (TermVar x var) -> Just $ TermVar x var
  _ -> Nothing

replaceSubtractionBySummation :: Reduction
replaceSubtractionBySummation = \case
  TermFun sp Minus e (TermLit sp' (LitInteger i)) ->
    Just $ TermFun sp Plus e (TermLit sp' (LitInteger (negate i)))
  TermFun sp Minus (TermLit sp' (LitInteger i)) e ->
    Just $ TermFun sp Plus (TermLit sp' (LitInteger (negate i))) e
  TermFun sp Minus e (TermLit sp' (LitFloating i)) ->
    Just $ TermFun sp Plus e (TermLit sp' (LitFloating (negate i)))
  TermFun sp Minus (TermLit sp' (LitFloating i)) e ->
    Just $ TermFun sp Plus (TermLit sp' (LitFloating (negate i))) e
  _ -> Nothing

zeroLaws :: Reduction
zeroLaws = \case
  TermFun _ Plus (TermLit _ (LitInteger 0)) e -> Just e
  TermFun _ Plus e (TermLit _ (LitInteger 0)) -> Just e
  TermFun _ Plus (TermLit _ (LitFloating 0)) e -> Just e
  TermFun _ Plus e (TermLit _ (LitFloating 0)) -> Just e
  TermFun _ Times z@(TermLit _ (LitInteger 0)) _ -> Just z
  TermFun _ Times _ z@(TermLit _ (LitInteger 0)) -> Just z
  TermFun _ Times z@(TermLit _ (LitFloating 0)) _ -> Just z
  TermFun _ Times _ z@(TermLit _ (LitFloating 0)) -> Just z
  _ -> Nothing

oneLaws :: Reduction
oneLaws = \case
  TermFun _ Times (TermLit _ (LitInteger 1)) e -> Just e
  TermFun _ Times e (TermLit _ (LitInteger 1)) -> Just e
  TermFun _ Times (TermLit _ (LitFloating 1)) e -> Just e
  TermFun _ Times e (TermLit _ (LitFloating 1)) -> Just e
  TermFun _ Div e (TermLit _ (LitInteger 1)) -> Just e
  TermFun _ Div e (TermLit _ (LitFloating 1)) -> Just e
  _ -> Nothing

foldConstants :: Reduction
foldConstants = \case
  TermFun spanOp fun (TermLit _ (LitInteger l)) (TermLit _ (LitInteger r)) ->
    TermLit spanOp <$> case fun of
      Plus -> Just . LitInteger $ l + r
      Minus -> Just . LitInteger $ l - r
      Times -> Just . LitInteger $ l * r
      Pow -> Nothing -- Grows too fast
      Div -> Nothing
  TermFun spanOp fun (TermLit _ (LitFloating l)) (TermLit _ (LitFloating r)) ->
    TermLit spanOp <$> case fun of
      Plus -> Just . LitFloating $ l + r
      Minus -> Just . LitFloating $ l - r
      Times -> Just . LitFloating $ l * r
      Pow -> Nothing
      Div -> Just . LitFloating $ l / r
  -- (e + c) + c  ->  e + c
  TermFun
    sp
    fun
    (TermFun sp' fun' e (TermLit _ (LitInteger i')))
    (TermLit _ (LitInteger i))
      | fun == fun' && fun == Plus ->
        Just $ TermFun (sp' <> sp) Plus e $
          TermLit (sp' <> sp) (LitInteger (i + i'))
  -- (c + e) + c  ->  e + c
  TermFun
    sp
    fun
    (TermFun sp' fun' (TermLit _ (LitInteger i')) e)
    (TermLit _ (LitInteger i))
      | fun == fun' && fun == Plus ->
        Just $ TermFun (sp' <> sp) Plus e $
          TermLit (sp' <> sp) (LitInteger (i + i'))
  -- c + (e + c)  ->  e + c
  TermFun
    sp
    fun
    (TermLit _ (LitInteger i))
    (TermFun sp' fun' e (TermLit _ (LitInteger i')))
      | fun == fun' && fun == Plus ->
        Just $ TermFun (sp' <> sp) Plus e $
          TermLit (sp' <> sp) (LitInteger (i + i'))
  -- c + (c + e)  ->  e + c
  TermFun
    sp
    fun
    (TermLit _ (LitInteger i))
    (TermFun sp' fun' (TermLit _ (LitInteger i')) e)
      | fun == fun' && fun == Plus ->
        Just $ TermFun (sp' <> sp) Plus e $
          TermLit (sp' <> sp) (LitInteger (i + i'))
  _ -> Nothing
