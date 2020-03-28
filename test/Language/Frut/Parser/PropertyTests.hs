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
prop_PrintParseRoundtrip = property $ do
  expr <- forAll genExpr
  tripping expr PP.renderExpr (parse @AST.Expr . InputStream.fromString)

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
      pure AST.InfixDiv
    ]
