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
  describe "━━━ Printer renders expressions properly ━━━" do
    describe "different precedence" do
      it "2 + (4 * 8)  ->  2 + 4 * 8" $
        let e = _2 `plus` parens (_4 `times` _8)
         in PP.renderExpr e `shouldBe` "2 + (4 * 8)"
      it "(4 * 8) + 2  ->  4 * 8 + 2" $
        let e = parens (_4 `times` _8) `plus` _2
         in PP.renderExpr e `shouldBe` "(4 * 8) + 2"
      it "2 * (4 + 8)  ->  2 * (4 + 8)" $
        let e = _2 `times` parens (_4 `plus` _8)
         in PP.renderExpr e `shouldBe` "2 * (4 + 8)"
      it "(4 + 8) * 2  ->  (4 + 8) * 2" $
        let e = parens (_4 `plus` _8) `times` _2
         in PP.renderExpr e `shouldBe` "(4 + 8) * 2"

parens :: ExpVanilla -> ExpVanilla
parens = ScopeVanilla

_2 :: ExpVanilla
_2 = LitVanilla (LitInteger 2)

_4 :: ExpVanilla
_4 = LitVanilla (LitInteger 4)

_8 :: ExpVanilla
_8 = LitVanilla (LitInteger 8)

plus :: ExpVanilla -> ExpVanilla -> ExpVanilla
plus = OpVanilla OperatorPlus

times :: ExpVanilla -> ExpVanilla -> ExpVanilla
times = OpVanilla OperatorTimes
