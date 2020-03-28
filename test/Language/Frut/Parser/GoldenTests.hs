{-# LANGUAGE TypeApplications #-}

module Language.Frut.Parser.GoldenTests where

import qualified Language.Frut.Data.InputStream as InputStream
import Language.Frut.Parser (parse)
import qualified Language.Frut.Syntax.AST as AST
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Show.Pretty

group :: IO TestTree
group = parseSourceFile

parseSourceFile :: IO TestTree
parseSourceFile = do
  frutFiles <- findByExtension [".frut"] "test/golden/parser/module"
  pure . testGroup "Parse Source File" $ do
    frutFile <- frutFiles
    return
      $ goldenVsString
        (takeBaseName frutFile)
        (replaceExtension frutFile ".golden.txt")
      $ encodeUtf8 @String @LByteString . ppShow . parse @AST.SourceFile
        <$> InputStream.readFile frutFile
