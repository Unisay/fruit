{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Language.Frut.Pretty.Printer
  ( renderExpr,
    Ann (..),
  )
where

import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc ((<+>), Doc)
import Language.Frut.Syntax.AST
import Language.Frut.Syntax.Precedence
  ( Associativity (..),
    assoc,
    prec,
  )
import Prelude hiding ((<>))

data Ann
  = InfixOp
  | Literal
  deriving (Eq, Show)

renderExpr :: Expr -> Doc Ann
renderExpr = printExpr Nothing

printExpr :: Maybe (Either InfixOp InfixOp) -> Expr -> Doc Ann
printExpr mbParentOp = \case
  ExprLiteral literal -> printLiteral literal
  ExprInfixOp op e1 e2 ->
    case mbParentOp of
      Just parentOp ->
        case either prec prec parentOp `compare` prec op of
          EQ ->
            -- same precedence
            case (either assoc assoc parentOp, assoc op) of
              (FullAssoc, FullAssoc) -> doc
              (FullAssoc, LeftAssoc) -> doc
              (LeftAssoc, FullAssoc) -> doc
              (LeftAssoc, LeftAssoc) ->
                case parentOp of
                  Left _ -> doc
                  Right _ -> Doc.parens doc
              (RightAssoc, RightAssoc) ->
                case parentOp of
                  Left _ -> Doc.parens doc
                  Right _ -> doc
              _ -> Doc.parens doc
          LT -> doc
          GT -> Doc.parens doc
      Nothing -> doc
    where
      doc :: Doc Ann
      doc =
        printExpr (Just (Left op)) e1
          <+> printInfixOp op
          <+> printExpr (Just (Right op)) e2

printLiteral :: Literal -> Doc Ann
printLiteral = Doc.annotate Literal . \case
  LiteralDecimal i -> Doc.unsafeViaShow i

printInfixOp :: InfixOp -> Doc Ann
printInfixOp = Doc.annotate InfixOp . \case
  InfixPlus -> Doc.pretty '+'
  InfixMinus -> Doc.pretty '-'
  InfixTimes -> Doc.pretty '*'
  InfixDiv -> Doc.slash
  InfixPow -> Doc.pretty '^'
