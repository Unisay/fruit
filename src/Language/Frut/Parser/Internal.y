{
module Language.Frut.Parser.Internal where

import qualified Language.Frut.Syntax.AST as AST
import qualified Language.Frut.Syntax.Tok as Tok
import Language.Frut.Alex
import Language.Frut.Lexer
import Language.Frut.Data.Position
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
%expect 0
%token 
  '.'      { Spanned Tok.Dot _ }
  ws       { Spanned (Tok.Space Tok.Whitespace) _ }
  module   { Spanned Tok.Module _ }
  upperId  { Spanned (Tok.UpperId $$) _ }
  lowerId  { Spanned (Tok.LowerId $$) _ }
  let      { Spanned Tok.Let _ }
  in       { Spanned Tok.In _ }
%%

Module 
  : module ws UpperIds { AST.Module $3 }

UpperIds 
  : upperId                 { pure $1 }
  | upperId '.' UpperIds    { NEL.cons $1 $3 }


{

parseSourceFile :: P AST.SourceFile
parseSourceFile = AST.SourceFile <$> parseModule

}