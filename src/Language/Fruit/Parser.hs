{-# LANGUAGE FlexibleInstances #-}

module Language.Fruit.Parser
  ( -- * Parsing
    parse,
    parse',
    readSourceFile,
    readTokens,
    Parse (..),
    P,
    execParser,
    execParserTokens,
    Span,

    -- * Lexing
    lexToken,
    lexNonSpace,
    lexTokens,

    -- * Error reporting
    lexicalError,
    parseError,
    ParseFail (..),
  )
where

import Control.Exception (throw)
import Language.Fruit.Data.InputStream (InputStream)
import qualified Language.Fruit.Data.InputStream as InputStream
import Language.Fruit.Data.Position (Position)
import qualified Language.Fruit.Data.Position as Pos
import Language.Fruit.Data.Span (Span, Spanned)
import Language.Fruit.Lexer (lexNonSpace, lexToken, lexTokens, lexicalError)
import Language.Fruit.Parser.Internal
-- import Language.Fruit.Parser.Literals (translateLit)
import Language.Fruit.Parser.Monad
  ( P,
    ParseFail (..),
    execParser,
    parseError,
    pushToken,
  )
import qualified Language.Fruit.Syntax.AST as AST
import Language.Fruit.Syntax.Tok as Tok

-- | Parse something from an input stream (it is assumed the initial position is 'initPos').
--
-- >>> fmap void $ parse @(Term Span) "x + 1"
-- Right (Binary [] AddOp (PathExpr [] Nothing (Path False [PathSegment "x" Nothing ()] ()) ())
--                        (Lit [] (Int Dec 1 Unsuffixed ()) ())
--                        ())
--
-- >>> fmap void $ parse @(Term Span) "x + "
-- Left (parse failure at 1:4 (Syntax error: unexpected `<EOF>' (expected an expression)))
parse :: Parse a => InputStream -> Either ParseFail a
parse is = execParser parser is Pos.initial

-- | Same as 'parse', but throws a 'ParseFail' exception if it cannot parse.
-- This function is intended for situations in which you are already stuck
-- catching exceptions - otherwise you should prefer 'parse'.
parse' :: Parse a => InputStream -> a
parse' = either throw id . parse

-- | Same as 'execParser', but working from a list of tokens instead of an 'InputStream'
execParserTokens :: P a -> [Spanned Tok] -> Position -> Either ParseFail a
execParserTokens p toks =
  execParser
    (pushTokens toks *> p)
    (InputStream.fromString "")
  where
    pushTokens = traverse_ pushToken . reverse

-- | Given a handle to a Fruit source file,
-- read that file and parse it into a 'SourceFile'
readSourceFile :: Handle -> IO AST.SourceFile
readSourceFile hdl = parse' <$> InputStream.readHandle hdl

-- | Given a path pointing to a Fruit source file,
-- read that file and lex it (ignoring whitespace)
readTokens :: Handle -> IO [Spanned Tok]
readTokens hdl = do
  inp <- InputStream.readHandle hdl
  case execParser (lexTokens lexNonSpace) inp Pos.initial of
    Left pf -> throw pf
    Right x -> pure x

class Parse a where
  parser :: P a

instance Parse AST.SourceFile where
  parser = parseSourceFile

instance Parse AST.Term where
  parser = parseTerm
