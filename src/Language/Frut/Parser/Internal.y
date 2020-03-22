{
module Language.Frut.Parser.Internal where

import qualified Language.Frut.Syntax.AST as AST
import qualified Language.Frut.Syntax.Tok as Tok
import Language.Frut.Alex
import Language.Frut.Lexer
import Language.Frut.Data.Position
import Language.Frut.Data.Span
import Language.Frut.Data.Ident
import Language.Frut.Parser.Monad
import Language.Frut.Syntax.Tok
import Relude.Unsafe ((!!))
import qualified Data.List.NonEmpty as NEL
}

%name parseModule
%error { parseError }
%tokentype { Spanned Tok }
%lexer { lexToken >>= } { Spanned Tok.EOF _ }
%monad { P } { >>= } { return }
%expect 1
%token 
  nl       { Spanned Tok.Newline _ }
  '-'      { Spanned Tok.Dash _ }
  '.'      { Spanned Tok.Dot _ }
  ','      { Spanned Tok.Comma _ }
  '('      { Spanned Tok.LParen _ } 
  ')'      { Spanned Tok.RParen _ } 
  indent   { Spanned Tok.Indent _ }
  dedent   { Spanned Tok.Dedent _ }
  module   { Spanned Tok.Module _ }
  imports  { Spanned Tok.Imports _ }
  exports  { Spanned Tok.Exports _ }
  upperId  { Spanned (Tok.UpperId $$) _ }
  lowerId  { Spanned (Tok.LowerId $$) _ }
  -- let      { Spanned Tok.Let _ }
  -- in       { Spanned Tok.In _ }
  eof      { Spanned Tok.EOF _ }
%%

Module :: { AST.Module }
  : module UpperQName Nls Exports Nls Imports ES 
  { AST.Module $2 $4 $6 }

Exports :: { AST.Exports }
  : exports ES NEL(lowerId) { AST.Exports $3 }

Imports :: { [AST.Import] }
  : {- empty -} { [] } -- shift/reduce conflict
  | imports ES NEL(Import) { NEL.toList $3 }

Import :: { AST.Import }
  : UpperQName List(lowerId) { AST.Import $1 $2 }

-- | List
List(e)
  : indent List1(e,'-','-') dedent { reverse $2 }
  | '(' List1(e,ES,',') ')' { reverse $2 }
  | '(' ES ')' { [] }
  | nl { [] }

List1(e, prefix, infix)
  : List1(e, prefix, infix) ES infix e { $4 : $1 }
  | prefix e { pure $2 }

-- | Non-empty list
NEL(e)
  : indent NEL1(e,'-','-') dedent { NEL.reverse $2 }
  | '(' NEL1(e,ES,',') ')' { NEL.reverse $2 }

NEL1(e, prefix, infix)
  : NEL1(e, prefix, infix) ES infix e { NEL.cons $4 $1 }
  | prefix e { pure $2 }

-- | Newlines
Nls : nl {} | nl Nls {}

-- | Empty Space
ES : {- empty -} {} | eof {} | nl ES {}

UpperQName
  : upperId { pure $1 }
  | upperId '.' UpperQName { NEL.cons $1 $3 }

{

parseSourceFile :: P AST.SourceFile
parseSourceFile = AST.SourceFile <$> parseModule

}