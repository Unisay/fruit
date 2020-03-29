{-# LANGUAGE TypeApplications #-}

module Language.Frut.GoldenTests where

import qualified Language.Frut.Data.InputStream as InputStream
import Language.Frut.Data.InputStream (InputStream)
import qualified Language.Frut.Data.Position as Pos
import Language.Frut.Data.Span (Spanned (..))
import Language.Frut.Lexer
import Language.Frut.Parser (parse)
import Language.Frut.Parser.Monad (ParseFail, execParser)
import qualified Language.Frut.Syntax.AST as AST
import Language.Frut.Syntax.Tok (Tok)
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
  frutFiles <- findByExtension [".frut"] "test/golden/parser"
  pure . testGroup "Parse Source File" $ do
    frutFile <- frutFiles
    return
      $ goldenVsString
        (takeBaseName frutFile)
        (replaceExtension frutFile ".golden.txt")
      $ encodeUtf8 @String @LByteString . ppShow . parse @AST.SourceFile
        <$> InputStream.readFile frutFile

lexSourceFile :: IO TestTree
lexSourceFile = do
  frutFiles <- findByExtension [".frut"] "test/golden/lexer"
  pure . testGroup "Lex Source File" $ do
    frutFile <- frutFiles
    return
      $ goldenVsString
        (takeBaseName frutFile)
        (replaceExtension frutFile ".golden.txt")
      $ encodeUtf8 @String @LByteString . ppShow . lex
        <$> InputStream.readFile frutFile
  where
    lex :: InputStream -> Either ParseFail [Spanned Tok]
    lex inputStream = execParser (lexTokens lexToken) inputStream Pos.initial
