{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Js.Printer where

import Data.Generics.Uniplate (para)
import qualified Data.List.NonEmpty as NEL
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    annotate,
    concatWith,
    defaultLayoutOptions,
    dot,
    equals,
    equals,
    hsep,
    layoutPretty,
    parens,
    pretty,
    slash,
    space,
    surround,
    unsafeViaShow,
  )
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc
import Language.Fruit.Data.Ident (Ident (..))
import Language.Fruit.Data.Span (unspan)
import Language.Fruit.Syntax.AST

data Js
  = JsOperator
  | JsLiteral
  | JsIdentifier
  | JsKeyword
  deriving (Eq, Show)

renderExpr :: ExpParsed -> Text
renderExpr = Doc.renderStrict . layoutPretty defaultLayoutOptions . printExpr

printExpr :: ExpParsed -> Doc Js
printExpr = para \case
  AppX {} ->
    hsep . fmap parens
  ScopeX {} ->
    parens . hsep
  LitX _ literal ->
    const (printLiteral literal)
  VarX _ var ->
    const (printVariable var)
  LetX _ var _ _ ->
    \xs ->
      annotate JsKeyword "let"
        <+> printVariable var
        <+> equals
        <+> concatWith
          (surround (surround (annotate JsKeyword "in") space space))
          xs
  OpX _ op _ _ ->
    concatWith (surround (surround (printOperator op) space space))

printLiteral :: Literal -> Doc Js
printLiteral = annotate JsLiteral . \case
  LitInteger i -> bool identity parens (i < 0) . unsafeViaShow $ i
  LitFloating d -> unsafeViaShow d

printVariable :: Var -> Doc Js
printVariable = annotate JsIdentifier . \case
  VarQualified idents ->
    concatWith (surround dot) . NEL.toList $
      pretty . name . unspan <$> idents
  VarUnqualified ident ->
    pretty . name $ ident

printOperator :: Operator -> Doc Js
printOperator = annotate JsOperator . \case
  OperatorPlus -> pretty '+'
  OperatorMinus -> pretty '-'
  OperatorTimes -> pretty '*'
  OperatorDiv -> slash
  OperatorPow -> pretty '^'
