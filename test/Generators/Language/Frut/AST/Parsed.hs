{-# LANGUAGE NamedFieldPuns #-}

module Generators.Language.Frut.AST.Parsed where

import qualified Generators.Basic as Gen
import qualified Generators.Language.Frut.AST.Generic as GG
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Language.Frut.Data.Position (Position (Position))
import qualified Language.Frut.Data.Position as Pos
import Language.Frut.Data.Span (Span (..))
import qualified Language.Frut.Syntax.AST as AST
import Prelude hiding (exp)

exp :: Gen AST.ExpParsed
exp =
  Gen.frequency
    [ (2, expLiteral),
      (1, op)
    ]

expLiteral :: Gen AST.ExpParsed
expLiteral = AST.LitParsed <$> span <*> GG.literal

op :: Gen AST.ExpParsed
op = f <$> span <*> GG.op <*> exp <*> exp
  where
    f sp o e1 e2 = AST.ScopeParsed sp (AST.OpParsed sp o e1 e2)

scope :: Gen AST.ExpParsed
scope = AST.ScopeParsed <$> span <*> exp

span :: Gen Span
span = do
  lo <- position
  let hi = Pos.moveForw 10 lo
  return Span {lo, hi}

position :: Gen Position
position = Position <$> Gen.natural <*> Gen.natural <*> Gen.natural
