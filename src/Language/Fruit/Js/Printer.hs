{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Fruit.Js.Printer where

import Data.Generics.Uniplate (para)
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    annotate,
    cat,
    concatWith,
    defaultLayoutOptions,
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
import Language.Fruit.Syntax.AST

data Js
  = JsOperator
  | JsLiteral
  | JsIdentifier
  | JsKeyword
  deriving (Eq, Show)

renderTerm :: Term -> Text
renderTerm = Doc.renderStrict . layoutPretty defaultLayoutOptions . printTerm

printTerm :: Term -> Doc Js
printTerm = para \case
  TermApp {} -> cat . fmap parens
  TermLam _ var _ -> mappend (printVar var) . mappend "=>" . hsep
  TermScope {} -> parens . hsep
  TermLit _ literal -> const (printLit literal)
  TermVar _ var -> const (printVar var)
  TermLet _ var _ _ ->
    \xs ->
      annotate JsKeyword "let"
        <+> printVar var
        <+> equals
        <+> concatWith
          (surround (surround (annotate JsKeyword "in") space space))
          xs
  TermFun _ fun _ _ ->
    concatWith (surround (surround (printFun fun) space space))

printLit :: Lit -> Doc Js
printLit = annotate JsLiteral . \case
  LitInteger i -> bool identity parens (i < 0) . unsafeViaShow $ i
  LitFloating d -> unsafeViaShow d

printVar :: Var -> Doc Js
printVar = annotate JsIdentifier . \case Var Ident {name} -> pretty name

printFun :: Fun -> Doc Js
printFun = annotate JsOperator . \case
  Plus -> pretty '+'
  Minus -> pretty '-'
  Times -> pretty '*'
  Div -> slash
  Pow -> pretty '^'
