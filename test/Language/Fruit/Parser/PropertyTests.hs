{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Fruit.Parser.PropertyTests where

import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Generators as Gen
import qualified Generators.Printed as GenPrinted
import Hedgehog
import qualified Language.Fruit.Data.InputStream as InputStream
import qualified Language.Fruit.Parser as Parser
import qualified Language.Fruit.Syntax.AST as AST
import qualified Language.Fruit.Syntax.Printer as Printer

group :: Group
group = $$(discover)

prop_PrintParseRoundtrip :: Property
prop_PrintParseRoundtrip = property do
  term <-
    renderStrict . Doc.layoutPretty Doc.defaultLayoutOptions
      <$> forAll GenPrinted.term
  annotateShow term
  parsed <- parse term
  term === Printer.renderTerm parsed
  where
    parse =
      evalEither
        . Parser.parse @AST.Term
        . InputStream.fromString
        . toString

prop_BalancedParens :: Property
prop_BalancedParens = property do
  term <- forAll Gen.term
  let printed = toString (Printer.renderTerm term)
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
