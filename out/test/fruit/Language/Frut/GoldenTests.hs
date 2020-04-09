{-# LANGUAGE TypeApplications #-}

module Language.Fruit.GoldenTests where

import qualified Language.Fruit.Data.InputStream as InputStream
import Language.Fruit.Data.InputStream (InputStream)
import qualified Language.Fruit.Data.Position as Pos
import Language.Fruit.Data.Span (Spanned (..))
import Language.Fruit.Lexer
import Language.Fruit.Parser (parse)
import Language.Fruit.Parser.Monad (ParseFail, execParser)
import qualified Language.Fruit.Syntax.AST as AST
import Language.Fruit.Syntax.Tok (Tok)
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Show.Pretty

group :: IO TestTree
group =
  testGroup "━━━ Golden tests ━━━"
    <$> sequence
      [ parseSourceFile,
        lexSourceFile
      ]

parseSourceFile :: IO TestTree
parseSourceFile = do
  fruitFiles <- findByExtension [".fruit"] "test/golden/parser"
  pure . testGroup "Parse Source File" $ do
    fruitFile <- fruitFiles
    return
      $ goldenVsString
        (takeBaseName fruitFile)
        (replaceExtension fruitFile ".golden.txt")
      $ encodeUtf8 @String @LByteString . ppShow . parse @AST.SourceFile
        <$> InputStream.readFile fruitFile

lexSourceFile :: IO TestTree
lexSourceFile = do
  fruitFiles <- findByExtension [".fruit"] "test/golden/lexer"
  pure . testGroup "Lex Source File" $ do
    fruitFile <- fruitFiles
    return
      $ goldenVsString
        (takeBaseName fruitFile)
        (replaceExtension fruitFile ".golden.txt")
      $ encodeUtf8 @String @LByteString . ppShow . lex
        <$> InputStream.readFile fruitFile
  where
    lex :: InputStream -> Either ParseFail [Spanned Tok]
    lex inputStream = execParser (lexTokens lexToken) inputStream Pos.initial
