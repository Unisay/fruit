{-# LANGUAGE TypeApplications #-}

module Generators.Printed where

import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc ((<+>), Doc)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

term :: Gen (Doc a)
term = do
  scoped <- Gen.bool
  termUnscoped <&> if scoped then Doc.parens else identity

termUnscoped :: Gen (Doc a)
termUnscoped =
  Gen.frequency
    [ (4, termLit),
      (1, termFun),
      (1, termApp),
      (1, termLet),
      (1, termLam)
    ]

termLit :: Gen (Doc a)
termLit = Doc.pretty <$> Gen.int64 (Range.exponentialFrom 0 minBound maxBound)

termFun :: Gen (Doc a)
termFun =
  ((<+>) .) . (<+>) <$> term <*> fun <*> term

fun :: Gen (Doc a)
fun = Gen.element $ Doc.pretty @Text <$> ["+", "-", "*", "/", "^"]

termFloat :: Gen (Doc a)
termFloat = Doc.pretty <$> Gen.double floatingRange
  where
    floatingRange = Range.exponentialFloatFrom 0 (-100000) 100000

termApp :: Gen (Doc a)
termApp = (<+>) <$> term <*> term

termVar :: Gen (Doc a)
termVar = Doc.pretty <$> Gen.lower

termLet :: Gen (Doc a)
termLet = do
  var <- termVar
  term1 <- term
  term2 <- term
  return $ Doc.hsep ["let", var, "=", term1, "in", term2]

termLam :: Gen (Doc a)
termLam = do
  var <- termVar
  term1 <- term
  return $ Doc.hcat ["λ", var, ".", term1]
