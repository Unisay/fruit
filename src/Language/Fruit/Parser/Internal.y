{
module Language.Fruit.Parser.Internal where

import qualified Language.Fruit.Syntax.AST as AST
import qualified Language.Fruit.Syntax.Tok as Tok
import Language.Fruit.Alex
import Language.Fruit.Lexer
import Language.Fruit.Data.Position
import Language.Fruit.Data.Span
import Language.Fruit.Data.Ident
import Language.Fruit.Parser.Monad
import Language.Fruit.Parser.Reversed
import Language.Fruit.Syntax.Tok
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
  '='      { Spanned Tok.Equal _ }
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
  lowerId  { Spanned (Tok.LowerId _) _ }
  integer  { Spanned (Tok.Integer _) _ }
  floating { Spanned (Tok.Floating _) _ }
  let      { Spanned Tok.Let _ }
  in       { Spanned Tok.In _ }
  eof      { Spanned Tok.EOF _ }
%left '+' '-'
%left '*' '/'
%right '^'
%right let in
%%

Module :: { AST.Module }
  : module UpperQName Nls Exports Nls Imports ES { AST.Module $2 $4 $6 }

Exports :: { AST.Exports }
  : exports ES NEL(LowerId) { AST.Exports $3 }

Imports :: { [AST.Import] }
  : {- empty -} { [] } -- shift/reduce conflict
  | imports ES NEL(Import) { NEL.toList $3 }

Import :: { AST.Import }
  : UpperQName List(LowerId) { AST.Import $1 $2 }

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
  | Variable
    { AST.VarParsed (spanOf $1) (unspan $1) }
  | let Variable '=' Expr in Expr
    { AST.LetParsed ($1 # $3) (unspan $2) $4 $6 }
  | '(' Expr ')' 
    { AST.ScopeParsed ($1 # $3) $2 }
  | indent Expr dedent 
    { AST.ScopeParsed ($1 # $3) $2 }

Literal :: { AST.ExpParsed }
  : integer 
    { let Spanned (Tok.Integer i) span = $1 
       in AST.LitParsed span (AST.LitInteger i) }
  | floating 
    { let Spanned (Tok.Floating d) span = $1 
       in AST.LitParsed span (AST.LitFloating d) }

Variable :: { Spanned AST.Var }
  : LowerId
  	{ Spanned (AST.VarUnqualified (unspan $1)) (spanOf $1) }

LowerId :: { Spanned Ident }
  : lowerId
    { let Spanned (Tok.LowerId ident) span = $1 in Spanned ident span }

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
    { let Spanned (Tok.UpperId ident) span = $1 
       in pure (Spanned ident span) }
  | upperId '.' UpperQName 
    { let Spanned (Tok.UpperId ident) span = $1
       in NEL.cons (Spanned ident span) $3 }

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