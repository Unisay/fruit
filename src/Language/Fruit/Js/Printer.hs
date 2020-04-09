{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Fruit.Js.Printer where

import Data.Generics.Uniplate (para)
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc ((<+>), Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc
import Language.Fruit.Data.Ident (Ident (..))
import Language.Fruit.Syntax.AST

data Js
  = JsOperator
  | JsLiteral
  | JsIdentifier
  | JsKeyword
  deriving (Eq, Show)

renderExpr :: ExpParsed -> Text
renderExpr =
  Doc.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . printExpr

printExpr :: ExpParsed -> Doc Js
printExpr = para \case
  ScopeX _ _ ->
    Doc.parens . Doc.hsep
  LitX _ literal ->
    const (printLiteral literal)
  VarX _ var ->
    const (printVariable var)
  LetX _ var _ _ ->
    \xs ->
      kwLet
        <+> printVariable var
        <+> Doc.pretty '='
        <+> Doc.concatWith
          (Doc.surround (Doc.surround kwIn Doc.space Doc.space))
          xs
  OpX _ op _ _ ->
    Doc.concatWith
      (Doc.surround (Doc.surround (printOperator op) Doc.space Doc.space))

printLiteral :: Literal -> Doc Js
printLiteral = Doc.annotate JsLiteral . \case
  LitInteger i -> bool identity Doc.parens (i < 0) . Doc.unsafeViaShow $ i
  LitFloating d -> Doc.unsafeViaShow d

printVariable :: Var -> Doc Js
printVariable = Doc.annotate JsIdentifier . \case
  VarQualified idents -> undefined
  VarUnqualified Ident {name} -> Doc.pretty name

printOperator :: Operator -> Doc Js
printOperator = Doc.annotate JsOperator . \case
  OperatorPlus -> Doc.pretty '+'
  OperatorMinus -> Doc.pretty '-'
  OperatorTimes -> Doc.pretty '*'
  OperatorDiv -> Doc.slash
  OperatorPow -> Doc.pretty '^'

kwLet :: Doc Js
kwLet = Doc.annotate JsKeyword "let"

kwIn :: Doc Js
kwIn = Doc.annotate JsKeyword "in"
