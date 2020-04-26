{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Js.Optimizer where

import Data.Foldable (foldl1)
import Data.Generics.Uniplate (rewrite)
import Language.Fruit.Js.Syntax

type Reduction = Term -> Maybe Term

optimize :: Term -> Term
optimize =
  rewrite . foldl1 (liftA2 (<|>)) $
    rewriteInfixOps :| [rewriteConditionalOperator]

rewriteInfixOps :: Reduction
rewriteInfixOps = \case
  TermCall (TermCall (TermVar var) [arg1]) [arg2]
    | Just op <- asOperator var ->
      Just $ TermOperator op arg1 arg2
  TermCall (TermCall (TermVar var) [arg]) args
    | Just builtIn <- asBuiltIn var ->
      Just $ TermCall (TermVar builtIn) (arg : args)
  _ -> Nothing
  where
    asOperator :: Var -> Maybe Operator
    asOperator (Var var) =
      case var of
        "plus" -> Just Plus
        "minus" -> Just Minus
        "mul" -> Just Mul
        "div" -> Just Div
        "pow" -> Just Pow
        _ -> Nothing
    asBuiltIn :: Var -> Maybe Var
    asBuiltIn (Var var) =
      case var of
        "pow" -> Just $ Var "Math.pow"
        _ -> Nothing

rewriteConditionalOperator :: Reduction
rewriteConditionalOperator = \case
  TermCall
    ( TermCall
        (TermCall (TermVar (Var "IF")) [predicateTerm])
        [thenTerm]
      )
    [elseTerm] -> Just (TermConditional predicateTerm thenTerm elseTerm)
  _ -> Nothing
