{-# LANGUAGE LambdaCase #-}

module Language.Frut.Pretty.Printer
  ( renderExpr,
  )
where

import Language.Frut.Syntax.AST
import Language.Frut.Syntax.Precedence (prec)
import Text.PrettyPrint.Annotated
import Prelude hiding ((<>))

renderExpr :: Expr -> String
renderExpr = render . printExpr

printExpr :: Expr -> Doc a
printExpr = \case
  ExprLiteral literal ->
    printLiteral literal
  ExprInfixOp op expr1 expr2 ->
    printExpr expr1
      <+> printInfixOp op
      <+> if prec expr1 > prec expr2
        then printExpr expr2
        else parens (printExpr expr2)

printLiteral :: Literal -> Doc a
printLiteral = \case
  LiteralDecimal i -> integer i

printInfixOp :: InfixOp -> Doc a
printInfixOp = \case
  InfixPlus -> char '+'
  InfixMinus -> char '-'
  InfixTimes -> char '*'
  InfixDiv -> char '/'
