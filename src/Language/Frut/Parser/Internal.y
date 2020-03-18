{
module Language.Frut.Parser.Internal where

import qualified Language.Frut.Syntax.AST as AST
import qualified Language.Frut.Syntax.Tok as Tok
import Language.Frut.Alex
import Language.Frut.Lexer (lexNonSpace)
import Language.Frut.Data.Position
import Language.Frut.Parser.Monad
import Language.Frut.Syntax.Tok
import qualified Language.Frut.Lexer
import Relude.Unsafe ((!!))
}

%name parseModule 
%error { parseError }
%tokentype { Spanned Tok }
%lexer { lexNonSpace >>= } { Spanned Tok.EOF _ }
%monad { P } { >>= } { return }
%expect 0
%token 
  module { Spanned Tok.Module _ }
  let { Spanned (Tok.Identifier "let") _ }
  in { Spanned (Tok.Identifier "in") _ }
%%

Module : module { AST.Module }

{

parseSourceFile :: P AST.SourceFile
parseSourceFile = AST.SourceFile <$> parseModule

}