{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Js.Printer where

import Data.Generics.Uniplate (para)
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc
import Language.Fruit.Syntax.AST

data Js
  = JsOperator
  | JsLiteral
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
  OpX _ op _ _ ->
    Doc.concatWith
      (Doc.surround (Doc.surround (printOperator op) Doc.space Doc.space))

printLiteral :: Literal -> Doc Js
printLiteral = Doc.annotate JsLiteral . \case
  LitInteger i -> bool identity Doc.parens (i < 0) . Doc.unsafeViaShow $ i
  LitFloating d -> Doc.unsafeViaShow d

printOperator :: Operator -> Doc Js
printOperator = Doc.annotate JsOperator . \case
  OperatorPlus -> Doc.pretty '+'
  OperatorMinus -> Doc.pretty '-'
  OperatorTimes -> Doc.pretty '*'
  OperatorDiv -> Doc.slash
  OperatorPow -> Doc.pretty '^'
