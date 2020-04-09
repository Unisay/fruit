{-# LANGUAGE BlockArguments #-}

module Language.Fruit.LexerSpec
  ( lexerSpec,
  )
where

import Language.Fruit.Data.InputStream (InputStream)
import qualified Language.Fruit.Data.InputStream as InputStream
import Language.Fruit.Data.Position (Position (..))
import qualified Language.Fruit.Data.Position as Pos
import Language.Fruit.Data.Span (Span (..), Spanned (..))
import Language.Fruit.Lexer
import Language.Fruit.Parser.Monad (ParseFail, execParser)
import qualified Language.Fruit.Syntax.Tok as Tok
import Language.Fruit.Syntax.Tok (Tok)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

lexerSpec :: Spec
lexerSpec =
  describe "━━━ Lexer works correctly ━━━" do
    describe "recognizes lexemes:" do
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
    describe "handles whitespace:" do
      it "inserts indent" $
        lexString "\n  ident"
          `shouldBe` Right
            [ spanned (1, 2, 1) (3, 2, 3) Tok.Indent,
              spanned (3, 2, 3) (8, 2, 8) (Tok.LowerId "ident")
            ]
      it "inserts nested dedent" do
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
      it "ignores whitespace" $
        lexString "  \n  "
          `shouldBe` Right [spanned (3, 2, 1) (5, 2, 3) Tok.Indent]
      it "ignores line comments" do
        lexString " -- comment" `shouldBe` Right []
        lexString "---" `shouldBe` Right []
      it "ignores flat block comments" $
        lexString "ok {- \n\n -} cool"
          `shouldBe` Right
            [ spanned (0, 1, 1) (2, 1, 3) (Tok.LowerId "ok"),
              spanned (12, 3, 5) (16, 3, 9) (Tok.LowerId "cool")
            ]
      it "ignores nested comments" $
        lexString "ok {-{- {- {- {- nested -- comment -} -} -} -}-} cool"
          `shouldBe` Right
            [ spanned (0, 1, 1) (2, 1, 3) (Tok.LowerId "ok"),
              spanned (49, 1, 50) (53, 1, 54) (Tok.LowerId "cool")
            ]

-- Test utils:

spanned ::
  (Natural, Natural, Natural) ->
  (Natural, Natural, Natural) ->
  a ->
  Spanned a
spanned from to x = Spanned x span
  where
    span = Span (pos from) (pos to)
    pos (offset, c, r) = Position offset c r

lex :: InputStream -> Either ParseFail [Spanned Tok]
lex inputStream = execParser (lexTokens lexToken) inputStream Pos.initial

lexString :: String -> Either ParseFail [Spanned Tok]
lexString = lex . InputStream.fromString
