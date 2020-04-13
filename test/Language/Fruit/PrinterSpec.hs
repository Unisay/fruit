{-# LANGUAGE BlockArguments #-}

module Language.Fruit.PrinterSpec
  ( printerSpec,
  )
where

import Language.Fruit.Syntax.AST
import qualified Language.Fruit.Syntax.Printer as PP
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty
import Prelude hiding (div)

printerSpec :: Spec
printerSpec =
  describe "━━━ Printer renders expressions properly ━━━" do
    describe "different precedence" do
      it "2 + (4 * 8)  ->  2 + 4 * 8" $
        let e = _2 `plus` parens (_4 `times` _8)
         in PP.renderTerm e `shouldBe` "2 + (4 * 8)"
      it "(4 * 8) + 2  ->  4 * 8 + 2" $
        let e = parens (_4 `times` _8) `plus` _2
         in PP.renderTerm e `shouldBe` "(4 * 8) + 2"
      it "2 * (4 + 8)  ->  2 * (4 + 8)" $
        let e = _2 `times` parens (_4 `plus` _8)
         in PP.renderTerm e `shouldBe` "2 * (4 + 8)"
      it "(4 + 8) * 2  ->  (4 + 8) * 2" $
        let e = parens (_4 `plus` _8) `times` _2
         in PP.renderTerm e `shouldBe` "(4 + 8) * 2"

parens :: Term -> Term
parens = TermScope mempty

_2 :: Term
_2 = TermLit mempty (LitInteger 2)

_4 :: Term
_4 = TermLit mempty (LitInteger 4)

_8 :: Term
_8 = TermLit mempty (LitInteger 8)

plus :: Term -> Term -> Term
plus = TermFun mempty Plus

times :: Term -> Term -> Term
times = TermFun mempty Times
