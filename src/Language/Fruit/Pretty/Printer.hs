{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Pretty.Printer
  ( renderExpr,
    printExpr,
    Ann (..),
  )
where

import Data.Generics.Uniplate (para)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    annotate,
    concatWith,
    dot,
    equals,
    hcat,
    hsep,
    parens,
    pretty,
    slash,
    space,
    surround,
    unsafeViaShow,
  )
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import Language.Fruit.Data.Ident (Ident (..))
import Language.Fruit.Data.Span (unspan)
import Language.Fruit.Syntax.AST
import Prelude hiding ((<>))

data Ann
  = AnnOperator
  | AnnLiteral
  | AnnIdentifier
  | AnnKeyword
  deriving (Eq, Show)

renderExpr :: ExpX ξ -> Text
renderExpr =
  Ansi.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . Doc.unAnnotate
    . printExpr

printExpr :: ExpX ξ -> Doc Ann
printExpr = toVanilla >>> para \case
  AppX _ _ _ ->
    hsep
  ScopeX _ _ ->
    parens . hsep
  LitX _ literal ->
    const (printLiteral literal)
  VarX _ var ->
    const (printVariable var)
  LetX _ var _ _ ->
    \xs ->
      annotate AnnKeyword "let"
        <+> printVariable var
        <+> equals
        <+> concatWith (surround (surround kwIn space space)) xs
    where
      kwIn :: Doc Ann
      kwIn = annotate AnnKeyword "in"
  OpX _ op _ _ ->
    concatWith (surround (surround (printOperator op) space space))
  ExpX _ ->
    hcat

printLiteral :: Literal -> Doc Ann
printLiteral = annotate AnnLiteral . \case
  LitInteger i -> unsafeViaShow i
  LitFloating d -> unsafeViaShow d

printVariable :: Var -> Doc Ann
printVariable = annotate AnnIdentifier . \case
  VarQualified idents ->
    concatWith (surround dot) . NEL.toList $
      pretty . name . unspan <$> idents
  VarUnqualified ident ->
    pretty . name $ ident

printOperator :: Operator -> Doc Ann
printOperator = annotate AnnOperator . \case
  OperatorPlus -> pretty '+'
  OperatorMinus -> pretty '-'
  OperatorTimes -> pretty '*'
  OperatorDiv -> slash
  OperatorPow -> pretty '^'
