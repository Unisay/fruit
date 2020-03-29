{-# LANGUAGE LambdaCase #-}

module Language.Frut.Pretty.Printer
  ( renderExpr,
  )
where

import Language.Frut.Syntax.AST
import Language.Frut.Syntax.Precedence
  ( Associativity (..),
    assoc,
    prec,
  )
import Text.PrettyPrint.Annotated
import Prelude hiding ((<>))

renderExpr :: Expr -> String
renderExpr = render . printExpr Nothing

printExpr :: Maybe (Either InfixOp InfixOp) -> Expr -> Doc a
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
                  Right _ -> parens doc
              (RightAssoc, RightAssoc) ->
                case parentOp of
                  Left _ -> parens doc
                  Right _ -> doc
              _ -> parens doc
          LT -> doc
          GT -> parens doc
      Nothing -> doc
    where
      doc :: Doc a
      doc =
        printExpr (Just (Left op)) e1
          <+> printInfixOp op
          <+> printExpr (Just (Right op)) e2

printLiteral :: Literal -> Doc a
printLiteral = \case
  LiteralDecimal i -> integer i

printInfixOp :: InfixOp -> Doc a
printInfixOp = \case
  InfixPlus -> char '+'
  InfixMinus -> char '-'
  InfixTimes -> char '*'
  InfixDiv -> char '/'
  InfixPow -> char '^'
