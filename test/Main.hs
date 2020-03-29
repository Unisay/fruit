module Main where

import Hedgehog (checkParallel)
import qualified Language.Frut.GoldenTests as ParserGoldenTests
import Language.Frut.LexerSpec (lexerSpec)
import qualified Language.Frut.Parser.PropertyTests as ParserPropertyTests
import Language.Frut.PrinterSpec (printerSpec)
import Test.Hspec (hspec)
import Test.Tasty (defaultMain)

main :: IO ()
main = do
  putStrLn ""
  hspec $ do printerSpec; lexerSpec
  _ <- checkParallel ParserPropertyTests.group
  defaultMain =<< ParserGoldenTests.group
