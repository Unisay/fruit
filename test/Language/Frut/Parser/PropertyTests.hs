{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Frut.Parser.PropertyTests where

import Data.Generics.Uniplate (rewrite)
import qualified Generators.Language.Frut.AST.Parsed as Gen
import Hedgehog
import qualified Language.Frut.Data.InputStream as InputStream
import qualified Language.Frut.Optimizer as Opt
import qualified Language.Frut.Parser as Parser
import qualified Language.Frut.Pretty.Printer as Printer
import qualified Language.Frut.Syntax.AST as AST
import Language.Frut.Syntax.Precedence (associatesLeft, isRightAssociative, prec)

group :: Group
group = $$(discover)

prop_PrintParseRoundtrip :: Property
prop_PrintParseRoundtrip = property do
  expr <- forAll Gen.exp
  let expr' = reassoc . AST.toVanilla . Opt.optimize $ expr
  annotateShow expr'
  tripping
    expr'
    (toString . Printer.renderExpr)
    ( fmap AST.toVanilla
        . Parser.parse @AST.ExpParsed
        . InputStream.fromString
    )

prop_BalancedParens :: Property
prop_BalancedParens = property do
  expr <- forAll Gen.exp
  let printed = toString (Printer.renderExpr expr)
  annotateShow printed
  isBalanced printed === True
  where
    isBalanced :: String -> Bool
    isBalanced string = go string 0
      where
        go :: String -> Integer -> Bool
        go ('(' : ss) n = go ss (n + 1)
        go (')' : ss) n | n > 0 = go ss (n -1)
        go (')' : _) n | n < 1 = False
        go (_ : ss) n = go ss n
        go "" 0 = True
        go "" _ = False

reassoc :: AST.ExpVanilla -> AST.ExpVanilla
reassoc = rewrite \case
  -- ∀ left-assoc ops: e1 `op` (e2 `op` e3) ---> (e1 `op` e2) `op` e3
  AST.OpVanilla op1 e1 (AST.OpVanilla op2 e2 e3)
    | prec op1 == prec op2 && associatesLeft op1 && associatesLeft op2 ->
      Just $ AST.OpVanilla op1 (AST.OpVanilla op2 e1 e2) e3
  -- ∀ right-assoc ops: (e1 `op` e2) `op` e3 ---> e1 `op` (e2 `op` e3)
  AST.OpVanilla op1 (AST.OpVanilla op2 e1 e2) e3
    | prec op1 == prec op2 && isRightAssociative op1 && isRightAssociative op2 ->
      Just $ AST.OpVanilla op1 e1 (AST.OpVanilla op2 e2 e3)
  _ -> Nothing
