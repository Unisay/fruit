{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Generators where

import qualified Generators.Basic as Gen
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Fruit.Data.Position (Position (Position))
import qualified Language.Fruit.Data.Position as Pos
import Language.Fruit.Data.Span (Span (..))
import qualified Language.Fruit.Syntax.AST as AST

term :: Gen AST.Term
term =
  Gen.frequency
    [ (2, termLit),
      (1, termFun)
    ]

termLit :: Gen AST.Term
termLit = AST.TermLit <$> span <*> literal

termFun :: Gen AST.Term
termFun = f <$> span <*> fun <*> term <*> term
  where
    f sp op e1 e2 = AST.TermScope sp (AST.TermFun sp op e1 e2)

scope :: Gen AST.Term
scope = AST.TermScope <$> span <*> term

span :: Gen Span
span = do
  lo <- position
  let hi = Pos.moveForw 10 lo
  return Span {lo, hi}

position :: Gen Position
position = Position <$> Gen.natural <*> Gen.natural <*> Gen.natural

literal :: Gen AST.Lit
literal = Gen.choice [litInt, litFloating]

litInt :: Gen AST.Lit
litInt =
  AST.LitInteger . fromIntegral
    <$> Gen.int64 (Range.exponentialFrom 0 (minBound @Int64) (maxBound @Int64))

litFloating :: Gen AST.Lit
litFloating =
  AST.LitFloating
    <$> Gen.double (Range.exponentialFloatFrom 0 (-10000) 10000)

fun :: Gen AST.Fun
fun = Gen.element [AST.Plus, AST.Minus, AST.Mul, AST.Div, AST.Pow]
