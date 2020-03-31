{-# LANGUAGE BlockArguments #-}

module Language.Frut.PrinterSpec
  ( printerSpec,
  )
where

import qualified Language.Frut.Pretty.Printer as PP
import Language.Frut.Syntax.AST
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty
import Prelude hiding (div)

printerSpec :: Spec
printerSpec =
  describe "--- Printer renders expressions properly ---" do
    describe "different precedence" do
      it "2 + (4 * 8)  ->  2 + 4 * 8" $
        let e = _2 `plus` (_4 `times` _8)
         in PP.renderExpr e `shouldBe` "2 + 4 * 8"
      it "(4 * 8) + 2  ->  4 * 8 + 2" $
        let e = (_4 `times` _8) `plus` _2
         in PP.renderExpr e `shouldBe` "4 * 8 + 2"
      it "2 * (4 + 8)  ->  2 * (4 + 8)" $
        let e = _2 `times` (_4 `plus` _8)
         in PP.renderExpr e `shouldBe` "2 * (4 + 8)"
      it "(4 + 8) * 2  ->  (4 + 8) * 2" $
        let e = (_4 `plus` _8) `times` _2
         in PP.renderExpr e `shouldBe` "(4 + 8) * 2"
    describe "same precedence" do
      describe "(FullAssoc, FullAssoc)" do
        it "(8 + 4) + 2 -> 8 + 4 + 2" $
          let e = (_8 `plus` _4) `plus` _2
           in PP.renderExpr e `shouldBe` "8 + 4 + 2"
        it "8 + (4 + 2) -> 8 + 4 + 2" $
          let e = _8 `plus` (_4 `plus` _2)
           in PP.renderExpr e `shouldBe` "8 + 4 + 2"
      describe "(FullAssoc, LeftAssoc)" do
        it "(8 - 4) + 2 -> 8 - 4 + 2" $
          let e = (_8 `minus` _4) `plus` _2
           in PP.renderExpr e `shouldBe` "8 - 4 + 2"
        it "8 + (4 - 2) -> 8 + 4 - 2" $
          let e = _8 `plus` (_4 `minus` _2)
           in PP.renderExpr e `shouldBe` "8 + 4 - 2"
      describe "(LeftAssoc, FullAssoc)" do
        it "8 - (4 + 2) -> 8 - 4 + 2" $
          let e = _8 `minus` (_4 `plus` _2)
           in PP.renderExpr e `shouldBe` "8 - 4 + 2"
        it "(4 + 2) - 8 -> 4 + 2 - 8" $
          let e = (_4 `plus` _2) `minus` _8
           in PP.renderExpr e `shouldBe` "4 + 2 - 8"
      describe "(RightAssoc, RightAssoc)" do
        it "(8 ^ 4) ^ 2 -> (8 ^ 4) ^ 2" $
          let e = (_8 `pow` _4) `pow` _2
           in PP.renderExpr e `shouldBe` "(8 ^ 4) ^ 2"
        it "8 ^ (4 ^ 2) -> 8 ^ 4 ^ 2" $
          let e = _8 `pow` (_4 `pow` _2)
           in PP.renderExpr e `shouldBe` "8 ^ 4 ^ 2"
      describe "(LeftAssoc, LeftAssoc)" do
        it "(8 - 4) - 2 -> 8 - 4 - 2" $
          let e = (_8 `minus` _4) `minus` _2
           in PP.renderExpr e `shouldBe` "8 - 4 - 2"
        it "8 - (4 - 2) -> 8 - (4 - 2)" $
          let e = _8 `minus` (_4 `minus` _2)
           in PP.renderExpr e `shouldBe` "8 - (4 - 2)"

_2 :: Expr
_2 = ExprLiteral (LiteralDecimal 2)

_4 :: Expr
_4 = ExprLiteral (LiteralDecimal 4)

_8 :: Expr
_8 = ExprLiteral (LiteralDecimal 8)

plus :: Expr -> Expr -> Expr
plus = ExprInfixOp InfixPlus

minus :: Expr -> Expr -> Expr
minus = ExprInfixOp InfixMinus

times :: Expr -> Expr -> Expr
times = ExprInfixOp InfixTimes

pow :: Expr -> Expr -> Expr
pow = ExprInfixOp InfixPow
