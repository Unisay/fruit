{-# LANGUAGE TypeApplications #-}

module Language.Frut.ParserTest where

import qualified Language.Frut.Data.InputStream as InputStream
import Language.Frut.Parser (parse)
import qualified Language.Frut.Syntax.AST as AST
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Show.Pretty

test_Parser :: IO TestTree
test_Parser = do
  frutFiles <- findByExtension [".frut"] "test/golden/parser"
  pure . testGroup "Parser" $ do
    frutFile <- frutFiles
    return
      $ goldenVsString
        (takeBaseName frutFile)
        (replaceExtension frutFile ".golden.txt")
      $ encodeUtf8 @String @LByteString . ppShow . parse @AST.SourceFile
        <$> InputStream.readFile frutFile
