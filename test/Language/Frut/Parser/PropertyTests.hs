{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Frut.Parser.PropertyTests where

import qualified Generators.Language.Frut.AST.Vanilla as Gen
import Hedgehog
import qualified Language.Frut.Data.InputStream as InputStream
import Language.Frut.Parser (parse)
import qualified Language.Frut.Pretty.Printer as PP
import qualified Language.Frut.Syntax.AST as AST

group :: Group
group = $$(discover)

prop_PrintParseRoundtrip :: Property
prop_PrintParseRoundtrip = property do
  expr <- forAll Gen.exp
  tripping
    expr
    (toString . PP.renderExpr)
    (fmap AST.toVanilla . parse @AST.ExpParsed . InputStream.fromString)

prop_BalancedParens :: Property
prop_BalancedParens = property do
  expr <- forAll Gen.exp
  let printed = toString (PP.renderExpr expr)
  annotateShow printed
  isBalanced printed === True
  where
    isBalanced :: String -> Bool
    isBalanced string = go string 0
      where
        go :: String -> Integer -> Bool
        go ('(' : ss) n = go ss (n + 1)
        go (')' : ss) n | n > 0 = go ss (n -1)
        go (')' : _) n | n < 1 = False
        go (_ : ss) n = go ss n
        go "" 0 = True
        go "" _ = False
