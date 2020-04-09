{-# LANGUAGE BlockArguments #-}

module Main where

import Hedgehog (checkParallel)
import qualified Language.Fruit.GoldenTests as ParserGoldenTests
import Language.Fruit.LexerSpec (lexerSpec)
import qualified Language.Fruit.Parser.PropertyTests as ParserPropertyTests
import Language.Fruit.PrinterSpec (printerSpec)
import Main.Utf8 (withUtf8)
import Test.Hspec (hspec)
import Test.Tasty (defaultMain)

main :: IO ()
main = withUtf8 do
  putStrLn ""
  hspec $ do printerSpec; lexerSpec
  void $ checkParallel ParserPropertyTests.group
  defaultMain =<< ParserGoldenTests.group
