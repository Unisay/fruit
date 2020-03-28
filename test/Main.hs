module Main where

import Hedgehog (checkParallel)
import qualified Language.Frut.Parser.GoldenTests as ParserGoldenTests
import qualified Language.Frut.Parser.PropertyTests as ParserPropertyTests
import Test.Tasty (defaultMain)

main :: IO ()
main = do
  putStrLn ""
  _ <- checkParallel ParserPropertyTests.group
  defaultMain =<< ParserGoldenTests.group
