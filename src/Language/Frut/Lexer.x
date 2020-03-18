{
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}    
{-# OPTIONS_GHC -fno-warn-deprecations #-}    
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Frut.Lexer where

import Language.Frut.Parser.Monad 
import Language.Frut.Data.Position (Spanned(..), Span(..))
import Language.Frut.Data.InputStream (peekChars, inputStreamEmpty, takeChar)
import Language.Frut.Alex
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
$large     = [$asclarge $unilarge]
$unismall  = \x02 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$ascsmall  = [a-z]
$small     = [$ascsmall $unismall \_]
$unigraphic = \x06 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$graphic   = [$small $large $symbol $digit $special $unigraphic \"\']
$binit     = 0-1
$octit     = 0-7
$hexit     = [$decdigit A-F a-f]
$uniidchar = \x07 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$idchar    = [$small $large $digit $uniidchar \']
$pragmachar = [$small $large $digit]
$docsym    = [\| \^ \* \$]

@varid     = $small $idchar*          -- variable identifiers
@conid     = $large $idchar*          -- constructor identifiers
@varsym    = ($symbol # \:) $symbol*  -- variable (operator) symbol
@consym    = \: $symbol*              -- constructor (operator) symbol
@numspc       = _*                    -- numeric spacer
@decimal      = $decdigit(@numspc $decdigit)*
@binary       = $binit(@numspc $binit)*
@octal        = $octit(@numspc $octit)*
@hexadecimal  = $hexit(@numspc $hexit)*
@exponent     = @numspc [eE] [\-\+]? @decimal
@bin_exponent = @numspc [pP] [\-\+]? @decimal
@qual = (@conid \.)+
@qvarid = @qual @varid
@qconid = @qual @conid
@qvarsym = @qual @varsym
@qconsym = @qual @consym
@floating_point = @numspc @decimal \. @decimal @exponent? | @numspc @decimal @exponent
@hex_floating_point = @numspc @hexadecimal \. @hexadecimal @bin_exponent? | @numspc @hexadecimal @bin_exponent
-- normal signed numerical literals can only be explicitly negative,
-- not explicitly positive (contrast @exponent)
@negative = \-
@signed = @negative ?
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @gap

frut :-

$white+ { pure . Tok.Space Tok.Whitespace }

"module" { token Tok.Module }

{

-- | Make a token.
token :: Tok -> String -> P Tok
token t _ = pure t

-- | Lexer for one 'Token'. The only token this cannot produce is 'Interpolated'.
lexToken :: P (Spanned Tok)
lexToken = popToken >>= \case
  Just tok -> pure tok
  Nothing -> do
    pos <- getPosition
    inp <- getInput
    case alexScan (pos, inp) 0 of
      AlexEOF -> 
        pure (Spanned Tok.EOF (Span pos pos))
      AlexError _ -> 
        fail "lexical error"
      AlexSkip (pos', inp') _ -> 
        setPosition pos' *> 
        setInput inp' *> 
        lexToken
      AlexToken (pos', inp') len action -> do
        setPosition pos'
        setInput inp'
        tok <- action (peekChars len inp)
        tok' <- swapToken tok
        pos'' <- getPosition
        return (Spanned tok' (Span pos pos''))

-- | Lexer for one non-whitespace 'Token'. The only tokens this cannot produce are 'Interpolated'
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

-- | Retrieve the next character (if there is one), without updating the parser state.
peekChar :: P (Maybe Char)
peekChar = do
  inp <- getInput
  if inputStreamEmpty inp 
    then pure Nothing
    else let (c, _) = takeChar inp
         in pure (Just c)

-- | Signal a lexical error.
lexicalError :: P a
lexicalError = do
  c <- peekChar
  fail ("Lexical error: the character " ++ show c ++ " does not fit here")

}

