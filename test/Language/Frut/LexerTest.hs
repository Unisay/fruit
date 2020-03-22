{-# LANGUAGE TypeApplications #-}

module Language.Frut.LexerTest where

import Language.Frut.Data.InputStream (InputStream)
import qualified Language.Frut.Data.InputStream as InputStream
import Language.Frut.Data.Position (Position (..))
import qualified Language.Frut.Data.Position as Pos
import Language.Frut.Data.Span (Span (..), Spanned (..))
import Language.Frut.Lexer
import Language.Frut.Parser.Monad (ParseFail, execParser)
import qualified Language.Frut.Syntax.Tok as Tok
import Language.Frut.Syntax.Tok (Tok)
import System.FilePath (replaceExtension, takeBaseName)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Show.Pretty

test_golden :: IO TestTree
test_golden = do
  frutFiles <- findByExtension [".frut"] "test/golden/lexer"
  pure . testGroup "Golden tests for Lexer" $ do
    frutFile <- frutFiles
    return
      $ goldenVsString
        (takeBaseName frutFile)
        (replaceExtension frutFile ".golden.txt")
      $ encodeUtf8 @String @LByteString . ppShow . lex
        <$> InputStream.readFile frutFile

spec_Lexer :: Spec
spec_Lexer = do
  describe "recognizes lexemes:" $ do
    it "module" $
      lexString " module "
        `shouldBe` Right [spanned (1, 1, 2) (7, 1, 8) Tok.Module]
    it "imports" $
      lexString " imports "
        `shouldBe` Right [spanned (1, 1, 2) (8, 1, 9) Tok.Imports]
    it "exports" $
      lexString " exports "
        `shouldBe` Right [spanned (1, 1, 2) (8, 1, 9) Tok.Exports]
    it "let" $
      lexString " let "
        `shouldBe` Right [spanned (1, 1, 2) (4, 1, 5) Tok.Let]
    it "in" $
      lexString " in "
        `shouldBe` Right [spanned (1, 1, 2) (3, 1, 4) Tok.In]
    it "'.'" $
      lexString " . "
        `shouldBe` Right [spanned (1, 1, 2) (2, 1, 3) Tok.Dot]
    it "'('" $
      lexString " ( "
        `shouldBe` Right [spanned (1, 1, 2) (2, 1, 3) Tok.LParen]
    it "')'" $
      lexString " ) "
        `shouldBe` Right [spanned (1, 1, 2) (2, 1, 3) Tok.RParen]
  describe "handles significant whitespace" $ do
    it "inserts indent" $ do
      lexString "\n  ident"
        `shouldBe` Right
          [ spanned (1, 2, 1) (3, 2, 3) Tok.Indent,
            spanned (3, 2, 3) (8, 2, 8) (Tok.LowerId "ident")
          ]
    it "inserts nested dedent" $ do
      let s = "\n  a \n    b\nc"
      {-
      1 |n
      2 |  a n
      3 |    bn
      4 |c
      -}
      lexString s
        `shouldBe` Right
          [ spanned (1, 2, 1) (3, 2, 3) Tok.Indent,
            spanned (3, 2, 3) (4, 2, 4) (Tok.LowerId "a"),
            spanned (6, 3, 1) (10, 3, 5) Tok.Indent,
            spanned (10, 3, 5) (11, 3, 6) (Tok.LowerId "b"),
            spanned (12, 4, 1) (12, 4, 1) Tok.Dedent,
            spanned (12, 4, 1) (12, 4, 1) Tok.Dedent,
            spanned (12, 4, 1) (13, 4, 2) (Tok.LowerId "c")
          ]
    it "skips whitespace" $
      lexString "  \n  "
        `shouldBe` Right [spanned (3, 2, 1) (5, 2, 3) Tok.Indent]

-- Test utils:

spanned ::
  (Natural, Natural, Natural) ->
  (Natural, Natural, Natural) ->
  a ->
  Spanned a
spanned from to x = Spanned x (Just span)
  where
    span = Span (pos from) (pos to)
    pos (offset, c, r) = Position offset c r

lex :: InputStream -> Either ParseFail [Spanned Tok]
lex inputStream = execParser (lexTokens lexToken) inputStream Pos.initial

lexString :: String -> Either ParseFail [Spanned Tok]
lexString = lex . InputStream.fromString
