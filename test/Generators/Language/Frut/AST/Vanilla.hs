module Generators.Language.Frut.AST.Vanilla where

import qualified Generators.Language.Frut.AST.Generic as GG
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Language.Frut.Syntax.AST as AST
import Prelude hiding (exp)

exp :: Gen AST.ExpVanilla
exp =
  Gen.frequency
    [ (2, lit),
      (1, op)
    ]

lit :: Gen AST.ExpVanilla
lit = AST.LitVanilla <$> GG.literal

scope :: Gen AST.ExpVanilla
scope = AST.ScopeVanilla <$> exp

op :: Gen AST.ExpVanilla
op = ((AST.ScopeVanilla .) .) . AST.OpVanilla <$> GG.op <*> exp <*> exp
