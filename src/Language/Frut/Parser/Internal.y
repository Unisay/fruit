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
import Language.Frut.Parser.Reversed
import Language.Frut.Syntax.Tok
import Relude.Unsafe ((!!))
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty(..), (<|))
}

%name parseModule
%name parseExpression Expr
%error { parseError }
%tokentype { Spanned Tok }
%lexer { lexToken >>= } { Spanned Tok.EOF _ }
%monad { P } { >>= } { return }
%expect 1
%token 
  nl       { Spanned Tok.Newline _ }
  '-'      { Spanned Tok.Dash _ }
  '+'      { Spanned Tok.Plus _ }
  '*'      { Spanned Tok.Times _ }
  '/'      { Spanned Tok.Div _ }
  '^'      { Spanned Tok.Pow _ }
  '.'      { Spanned Tok.Dot _ }
  ','      { Spanned Tok.Comma _ }
  '('      { Spanned Tok.LParen _ } 
  ')'      { Spanned Tok.RParen _ } 
  indent   { Spanned Tok.Indent _ }
  dedent   { Spanned Tok.Dedent _ }
  module   { Spanned Tok.Module _ }
  imports  { Spanned Tok.Imports _ }
  exports  { Spanned Tok.Exports _ }
  upperId  { Spanned (Tok.UpperId _) _ }
  lowerId  { Spanned (Tok.LowerId $$) _ }
  decimal  { Spanned (Tok.Decimal _) _ }
  -- let      { Spanned Tok.Let _ }
  -- in       { Spanned Tok.In _ }
  eof      { Spanned Tok.EOF _ }
%left '+' '-'
%left '*' '/'
%right '^'
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

Expr :: { AST.ExpParsed }
  : Expr '+' Expr 
    { AST.OpParsed (spanOf $2) AST.OperatorPlus $1 $3 }
  | Expr '-' Expr 
    { AST.OpParsed (spanOf $2) AST.OperatorMinus $1 $3 }
  | Expr '*' Expr
    { AST.OpParsed (spanOf $2) AST.OperatorTimes $1 $3 }
  | Expr '/' Expr
    { AST.OpParsed (spanOf $2) AST.OperatorDiv $1 $3 }
  | Expr '^' Expr
    { AST.OpParsed (spanOf $2) AST.OperatorPow $1 $3 }
  | Literal 
    { $1 }
  | '(' Expr ')' 
    { $2 }

Literal :: { AST.ExpParsed }
  : decimal 
    { AST.LitParsed (spanOf $1) 
      (let Tok.Decimal d = unspan $1 in AST.Literal d) }

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

UpperQName :: { AST.QualifiedName }
  : upperId 
    { let Tok.UpperId ident = unspan $1 in pure ident }
  | upperId '.' UpperQName 
    { let Tok.UpperId ident = unspan $1 in NEL.cons ident $3 }

-------------
-- Utility --
-------------

-- | One or more occurences of 'p'
some(p) :: { Reversed NonEmpty p }
  : some(p) p             { let Reversed xs = $1 in Reversed ($2 <| xs) }
  | p                     { [$1] }

-- | Zero or more occurences of 'p'
many(p) :: { [ p ] }
  : some(p)               { toList $1 }
  | {- empty -}           { [] }

-- | One or more occurences of 'p', seperated by 'sep'
sep_by1(p,sep) :: { Reversed NonEmpty p }
  : sep_by1(p,sep) sep p  { let Reversed xs = $1 in Reversed ($3 <| xs) }
  | p                     { [$1] }

-- | Zero or more occurrences of 'p', separated by 'sep'
sep_by(p,sep) :: { [ p ] }
  : sep_by1(p,sep)        { toList $1 }
  | {- empty -}           { [] }

-- | One or more occurrences of 'p', seperated by 'sep', 
-- optionally ending in 'sep'
sep_by1T(p,sep) :: { Reversed NonEmpty p }
  : sep_by1(p,sep) sep    { $1 }
  | sep_by1(p,sep)        { $1 }

-- | Zero or more occurences of 'p', seperated by 'sep', 
-- optionally ending in 'sep' (only if there is at least one 'p')
sep_byT(p,sep) :: { [ p ] }
  : sep_by1T(p,sep)       { toList $1 }
  | {- empty -}           { [] }

{

parseSourceFile :: P AST.SourceFile
parseSourceFile = AST.SourceFile <$> parseModule

}