{-# LANGUAGE TypeApplications #-}

module Generators.Language.Frut.AST.Generic where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Frut.Syntax.AST as AST

literal :: Gen AST.Literal
literal = Gen.choice [litInt, litFloating]

litInt :: Gen AST.Literal
litInt =
  AST.LitInteger . fromIntegral
    <$> Gen.int64 (Range.exponentialFrom 0 (minBound @Int64) (maxBound @Int64))

litFloating :: Gen AST.Literal
litFloating =
  AST.LitFloating
    <$> Gen.double (Range.exponentialFloatFrom 0 (-10000) 10000)

op :: Gen AST.Operator
op =
  Gen.choice
    [ pure AST.OperatorPlus,
      pure AST.OperatorMinus,
      pure AST.OperatorTimes,
      pure AST.OperatorDiv,
      pure AST.OperatorPow
    ]
