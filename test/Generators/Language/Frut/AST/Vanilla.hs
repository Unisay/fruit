module Generators.Language.Frut.AST.Vanilla where

import qualified Generators.Language.Frut.AST.Generic as GG
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Language.Frut.Syntax.AST as AST
import Prelude hiding (exp)

exp :: Gen AST.ExpVanilla
exp = Gen.frequency [(10, lit), (3, op)]

lit :: Gen AST.ExpVanilla
lit = AST.LitVanilla <$> GG.literal

op :: Gen AST.ExpVanilla
op = AST.OpVanilla <$> GG.op <*> exp <*> exp
