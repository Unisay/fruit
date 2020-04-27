{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Generators.Printed where

import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc ((<+>), Doc)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

term :: forall a. Gen (Doc a)
term = Gen.sized \size ->
  Gen.frequency
    [ (300, termLit),
      (300, termVar),
      (200, termScoping <*> termLam),
      (100 + unSize size, termScoping <*> termFun),
      (100 + unSize size, termScoping <*> termApp),
      (80 + unSize size, termScoping <*> termLet),
      (80 + unSize size, termScoping <*> termIte)
    ]

termScoping :: Gen (Doc a -> Doc a)
termScoping =
  Gen.frequency
    [ (1, pure Doc.parens),
      (3, pure identity)
    ]

termLit :: Gen (Doc a)
termLit = Gen.frequency [(3, termLitInt), (2, termLitBool)]

termLitInt :: Gen (Doc a)
termLitInt = Doc.pretty <$> Gen.int64 (Range.exponentialFrom 0 minBound maxBound)

termLitBool :: Gen (Doc a)
termLitBool = Doc.pretty <$> Gen.bool

termFun :: Gen (Doc a)
termFun =
  ((<+>) .) . (<+>)
    <$> Gen.small term
    <*> fun
    <*> Gen.small term

fun :: Gen (Doc a)
fun = Gen.element $ Doc.pretty @Text <$> ["+", "-", "*", "/", "^"]

termFloat :: Gen (Doc a)
termFloat = Doc.pretty <$> Gen.double floatingRange
  where
    floatingRange = Range.exponentialFloatFrom 0 (-100000) 100000

termApp :: Gen (Doc a)
termApp = (<+>) <$> Gen.small termFun <*> Gen.small term

termVar :: Gen (Doc a)
termVar = Doc.pretty <$> Gen.lower

termLet :: Gen (Doc a)
termLet = do
  var <- termVar
  term1 <- Gen.small term
  term2 <- Gen.small term
  return $ Doc.hsep ["let", var, "=", term1, "in", term2]

termIte :: Gen (Doc a)
termIte = do
  term1 <- Gen.small term
  term2 <- Gen.small term
  term3 <- Gen.small term
  return $ Doc.hsep ["if", term1, "then", term2, "else", term3]

termLam :: Gen (Doc a)
termLam = do
  var <- termVar
  term1 <- Gen.small term
  return $ Doc.hcat ["λ", var, ".", term1]
