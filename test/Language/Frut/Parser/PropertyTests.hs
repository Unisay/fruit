{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Frut.Parser.PropertyTests where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Frut.Data.InputStream as InputStream
import Language.Frut.LexerTest (lexString)
import Language.Frut.Parser (parse)
import qualified Language.Frut.Syntax.AST as AST

group :: Group
group = $$(discover)

prop_ExpressionLiteral :: Property
prop_ExpressionLiteral =
  property $ do
    literal <- forAll genLiteral
    let expected = AST.ExprLiteral literal
    roundtripExpr (show literal) expected

prop_ExpressionInfixOp :: Property
prop_ExpressionInfixOp = property $ do
  (literal1, literal2) <- forAll $ (,) <$> genLiteral <*> genLiteral
  infixOp <- forAll genInfixOp
  let source = toString $ unwords [show literal1, show infixOp, show literal2]
      expected =
        AST.ExprInfixOp
          infixOp
          (AST.ExprLiteral literal1)
          (AST.ExprLiteral literal2)
  roundtripExpr source expected

roundtripExpr :: String -> AST.Expr -> PropertyT IO ()
roundtripExpr source expected = do
  annotateShow source
  annotateShow $ lexString source
  annotateShow expected
  actual <- evalEither $ parse @AST.Expr (InputStream.fromString source)
  annotateShow actual
  actual === expected

genLiteralDecimal :: Gen AST.Literal
genLiteralDecimal =
  AST.LiteralDecimal . fromIntegral
    <$> Gen.int64 (Range.exponential (minBound @Int64) (maxBound @Int64))

genLiteral :: Gen AST.Literal
genLiteral = Gen.choice [genLiteralDecimal]

genInfixOp :: Gen AST.InfixOp
genInfixOp = Gen.choice [pure AST.InfixPlus, pure AST.InfixMinus]
