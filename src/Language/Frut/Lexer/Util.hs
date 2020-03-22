{-# LANGUAGE NamedFieldPuns #-}

module Language.Frut.Lexer.Util where

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
    indents@(currentIndent : _) -> case compare indent currentIndent of
      GT -> do
        setPState parserState {pushedIndents = indent : indents}
        let startPos' = Pos.moveBack (pred indent) curPos
        return $ Spanned Tok.Indent (Just $ Span startPos' curPos)
      LT -> do
        case List.span (> indent) indents of
          (pre, []) ->
            fail $
              "Invalid indentation state: "
                <> List.intercalate ":" (show <$> pre)
          (pre, post) -> do
            let mkDedent :: Natural -> Spanned Tok
                mkDedent _ =
                  let pos = curPos
                   in Spanned Tok.Dedent $ Just $ Span pos pos
            case uncons (mkDedent <$> pre) of
              Nothing -> error "Impossible happened: expected dedent"
              Just (dedent, remainingDedents) -> do
                setPState
                  parserState
                    { pushedIndents = post,
                      pushedTokens = remainingDedents -- TODO: prepend
                    }
                return dedent
      EQ ->
        return . Spanned Tok.Newline . Just $
          Span.mapHi (const $ Pos.newline startPos) span
