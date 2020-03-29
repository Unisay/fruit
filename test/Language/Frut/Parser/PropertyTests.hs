{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Frut.Parser.PropertyTests where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Frut.Data.InputStream as InputStream
import Language.Frut.Parser (parse)
import qualified Language.Frut.Pretty.Printer as PP
import qualified Language.Frut.Syntax.AST as AST

group :: Group
group = $$(discover)

prop_PrintParseRoundtrip :: Property
prop_PrintParseRoundtrip = property do
  expr <- forAll genExpr
  tripping expr PP.renderExpr (parse @AST.Expr . InputStream.fromString)

prop_BalancedParens :: Property
prop_BalancedParens = property do
  expr <- forAll genExpr
  let printed = PP.renderExpr expr
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

-- Generators:

genExpr :: Gen AST.Expr
genExpr = Gen.frequency [(10, genExprLiteral), (3, genExprInfixOp)]

genExprInfixOp :: Gen AST.Expr
genExprInfixOp =
  AST.ExprInfixOp
    <$> genInfixOp
    <*> genExpr
    <*> genExpr

genLiteralDecimal :: Gen AST.Literal
genLiteralDecimal =
  AST.LiteralDecimal . fromIntegral
    <$> Gen.int64 (Range.exponentialFrom 0 (minBound @Int64) (maxBound @Int64))

genExprLiteral :: Gen AST.Expr
genExprLiteral = AST.ExprLiteral <$> genLiteral

genLiteral :: Gen AST.Literal
genLiteral = Gen.choice [genLiteralDecimal]

genInfixOp :: Gen AST.InfixOp
genInfixOp =
  Gen.choice
    [ pure AST.InfixPlus,
      pure AST.InfixMinus,
      pure AST.InfixTimes,
      pure AST.InfixDiv,
      pure AST.InfixPow
    ]
