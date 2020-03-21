{-# LANGUAGE NamedFieldPuns #-}

module Language.Frut.Lexer.Util where

import Control.Monad (when)
import qualified Data.List as List
import qualified Language.Frut.Data.Position as Pos
import Language.Frut.Data.Span (Span (..), Spanned (..))
import qualified Language.Frut.Data.Span as Span
import Language.Frut.Parser.Monad
import qualified Language.Frut.Syntax.Tok as Tok
import Language.Frut.Syntax.Tok (Tok)

type AlexAction = Span -> Int -> String -> P (Spanned Tok)

-- | Make a token.
token :: Tok -> AlexAction
token tok span _ _ =
  pure $ Spanned tok (Just span)

tokenStr :: (String -> Tok) -> AlexAction
tokenStr f span _ str =
  pure $ Spanned (f str) (Just span)

startWhite :: AlexAction
startWhite span@(Span startPos curPos) n _ = do
  let indent = toEnum n
  parserState <- getPState
  case pushedIndents parserState of
    [] -> fail "Empty indentation state"
    indents@(currentIndent : _) -> do
      when (indent > currentIndent) $ do
        setPState
          parserState
            { pushedIndents = indent : indents,
              pushedTokens =
                let startPos' = Pos.moveBack (pred indent) curPos
                 in [Spanned Tok.Indent (Just $ Span startPos' curPos)]
            }
      when (indent < currentIndent) $ do
        case List.span (> indent) indents of
          (pre, []) ->
            fail $
              "Invalid indentation state: "
                <> List.intercalate ":" (show <$> pre)
          (pre, post) ->
            setPState
              parserState
                { pushedIndents = post,
                  pushedTokens =
                    let dedent = Spanned Tok.Dedent Nothing
                     in const dedent <$> pre
                }
  return . Spanned Tok.Newline . Just $
    Span.mapHi (const $ Pos.newline startPos) span
