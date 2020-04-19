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
import Data.List.NonEmpty (NonEmpty(..))
}

%name parseModule
%name parseTerm Term
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
  'λ'      { Spanned Tok.Lambda _ }
  '/'      { Spanned Tok.Div _ }
  '^'      { Spanned Tok.Pow _ }
  '.'      { Spanned Tok.Dot _ }
  ','      { Spanned Tok.Comma _ }
  '('      { Spanned Tok.LParen _ } 
  ')'      { Spanned Tok.RParen _ } 
  rightArr { Spanned Tok.RightArrow _ }
  leftArr  { Spanned Tok.LeftArrow _ }
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
%right let in
%left '+' '-'
%left '*' '/'
%right '^'
%right 'λ' '.'
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

Term :: { AST.Term }
  : Factor Term 
    { AST.TermApp $1 $2 } 
  | let Variable '=' Term in Term 
    { AST.TermLet ($1 # $3) (unspan $2) $4 $6 }
  | 'λ' Variable '.' Term
    { AST.TermLam ($1 # $3) (unspan $2) $4 }
  | Term1 { $1 }

Term1 :: { AST.Term }
  : Factor '+' Term
    { AST.TermFun (spanOf $2) AST.Plus $1 $3 }
  | Factor '-' Term
    { AST.TermFun (spanOf $2) AST.Minus $1 $3 }
  | Factor '*' Term
    { AST.TermFun (spanOf $2) AST.Mul $1 $3 }
  | Factor '/' Term
    { AST.TermFun (spanOf $2) AST.Div $1 $3 }
  | Factor '^' Term
    { AST.TermFun (spanOf $2) AST.Pow $1 $3 }
  | Factor
    { $1 }

Factor :: { AST.Term }
  : Literal 
    { $1 }
  | Variable
    { AST.TermVar (spanOf $1) (unspan $1) }
  | '(' Term ')' 
    { AST.TermScope ($1 # $3) $2 }
  | indent Term dedent 
    { AST.TermScope ($1 # $3) $2 }

Literal :: { AST.Term }
  : integer 
    { let Spanned (Tok.Integer i) span = $1 
       in AST.TermLit span (AST.LitInteger i) }
  | floating 
    { let Spanned (Tok.Floating d) span = $1 
       in AST.TermLit span (AST.LitFloating d) }

Variable :: { Spanned AST.Var }
  : LowerId
  	{ Spanned (AST.Var (unspan $1)) (spanOf $1) }

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
  : some(p) p             { let Reversed xs = $1 in Reversed (NEL.cons $2 xs) }
  | p                     { Reversed (pure $1) }

-- | Zero or more occurences of 'p'
many(p) :: { [ p ] }
  : some(p)               { toList $1 }
  | {- empty -}           { [] }



{

parseSourceFile :: P AST.SourceFile
parseSourceFile = AST.SourceFile <$> parseModule

}