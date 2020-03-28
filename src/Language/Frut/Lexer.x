{
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}    
{-# OPTIONS_GHC -fno-warn-deprecations #-}    
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Frut.Lexer where

import Language.Frut.Alex
import Language.Frut.Data.Ident
import Language.Frut.Data.Span (Spanned(..), Span(..))
import Language.Frut.Data.InputStream (peekChars, inputStreamEmpty, takeChar)
import Language.Frut.Lexer.Util
import Language.Frut.Parser.Monad 
import Language.Frut.Syntax.Tok (Tok)
import qualified Language.Frut.Syntax.Tok as Tok
import Prelude hiding (head)
}

$unispace    = \x05 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$nl          = [\n\r\f]
$whitechar   = [$nl\v\ $unispace]
$white_no_nl = $whitechar # \n
$tab         = \t
$ascdigit  = 0-9
$unidigit  = \x03 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$decdigit  = $ascdigit -- for now, should really be $digit (ToDo)
$digit     = [$ascdigit $unidigit]
$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol = \x04 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$symbol    = [$ascsymbol $unisymbol] # [$special \_\"\']
$unilarge  = \x01 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$asclarge  = [A-Z]
$upper     = [$asclarge $unilarge]
$unismall  = \x02 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$ascsmall  = [a-z]
$lower     = [$ascsmall $unismall \_]
$unigraphic = \x06 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$graphic   = [$lower $upper $symbol $digit $special $unigraphic \"\']
$binit     = 0-1
$octit     = 0-7
$hexit     = [$decdigit A-F a-f]
$uniidchar = \x07 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$idchar    = [$lower $upper $digit $uniidchar \']

@commentStart = \{ \-
@commentEnd = \- \}
@lowerId = $lower $idchar*          
@upperId = $upper $idchar*       
@numspc       = _*                    -- numeric spacer
@decimal      = $decdigit(@numspc $decdigit)*
@binary       = $binit(@numspc $binit)*
@octal        = $octit(@numspc $octit)*
@hexadecimal  = $hexit(@numspc $hexit)*
@exponent     = @numspc [eE] [\-\+]? @decimal
@bin_exponent = @numspc [pP] [\-\+]? @decimal

@floating_point = @numspc @decimal \. @decimal @exponent? | @numspc @decimal @exponent
@hex_floating_point = @numspc @hexadecimal \. @hexadecimal @bin_exponent? | @numspc @hexadecimal @bin_exponent
-- normal signed numerical literals can only be explicitly negative,
-- not explicitly positive (contrast @exponent)
@minus = \-
@signed = @minus ?
@gap    = \\ $whitechar+ \\
@string = $graphic # [\"\\] | " " | @gap

frut :-

<0> $nl$white_no_nl* { startWhite }
<0> $white_no_nl+ ;
<0> "--".* ;
<0,comment> @commentStart { beginComment  }
<comment> @commentEnd { endComment }
<comment> [. \n ] ;

-- Reserved words
<0> "module" { token Tok.Module }
<0> "imports" { token Tok.Imports }
<0> "exports" { token Tok.Exports }
<0> "let" { token Tok.Let }
<0> "in" { token Tok.In }

-- Reserved symbols
<0> "-" { token Tok.Dash }
<0> "." { token Tok.Dot }
<0> "," { token Tok.Comma }
<0> "(" { token Tok.LParen }
<0> ")" { token Tok.RParen }
<0> "+" { token Tok.Plus }
<0> "=" { token Tok.Equal }

-- Identifiers
<0> @lowerId { tokenStr (Tok.LowerId . mkIdent) }
<0> @upperId { tokenStr (Tok.UpperId . mkIdent) }

-- Literals
<0> @signed @decimal { tokenDec }

{

-- | Lexer for one 'Token'. The only token this cannot produce is 'Interpolated'.
lexToken :: P (Spanned Tok)
lexToken = popToken >>= \case
  Just spanned@(Spanned _ mbSpan) -> do
    traverse_ (setPosition . hi) mbSpan
    pure spanned
  Nothing -> do
    pos <- getPosition
    inp <- getInput
    sc <- getStartCode
    case alexScan (pos, inp) sc of
      AlexEOF -> pure (Spanned Tok.EOF (Just $ Span pos pos))
      AlexError _ -> lexicalError
      AlexSkip (pos', inp') _ -> 
        setPosition pos' *> setInput inp' *> lexToken
      AlexToken (pos', inp') len action -> do
        setPosition pos'
        setInput inp'
        let top = peekChars len inp
        spannedTok <- action (Span pos pos') len top
        maybe lexToken pure spannedTok

-- | Lexer for one non-whitespace 'Token'. 
-- and 'Space' (which includes comments that aren't doc comments).
lexNonSpace :: P (Spanned Tok)
lexNonSpace = lexToken >>= \case
  Spanned Tok.Space {} _ -> lexNonSpace
  tok -> pure tok

-- | Apply the given lexer repeatedly until (but not including) the 'Eof' token.
-- Meant for debugging purposes - in general this defeats the point of a threaded lexer.
lexTokens :: P (Spanned Tok) -> P [Spanned Tok]
lexTokens lexer = do
  tok <- lexer
  case tok of
    Spanned Tok.EOF _ -> pure []
    _ -> (tok :) <$> lexTokens lexer

-- | Retrieve the next character (if there is one), 
-- without updating the parser state.
peekChar :: P (Maybe Char)
peekChar = do
  inp <- getInput
  if inputStreamEmpty inp
    then pure Nothing
    else
      let (c, _) = takeChar inp
       in pure (Just c)

-- | Signal a lexical error.
lexicalError :: P a
lexicalError = peekChar >>= \case
  Nothing -> 
    fail "Lexical error"
  Just c -> 
    fail ("The character " <> show c <> " does not fit here")

beginComment :: AlexAction
beginComment _ _ _ = do
  depth <- getCommentDepth
  setCommentDepth (succ depth)
  setStartCode comment
  return Nothing

endComment :: AlexAction
endComment _ _ _ = do
  depth <- getCommentDepth
  setCommentDepth (pred depth)
  when (depth == 1) (setStartCode 0)
  return Nothing

}

