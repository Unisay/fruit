{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Js.Printer where

import Data.Generics.Uniplate (para)
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    align,
    annotate,
    braces,
    concatWith,
    defaultLayoutOptions,
    hsep,
    layoutPretty,
    parens,
    pretty,
    sep,
    slash,
    space,
    surround,
    tupled,
    unsafeViaShow,
  )
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc
import Language.Fruit.Js.Syntax

renderTerm :: Term -> Text
renderTerm = Doc.renderStrict . layoutPretty defaultLayoutOptions . printTerm

printTerm :: Term -> Doc Lexeme
printTerm = para \case
  TermVar var ->
    const (printVar var)
  TermNumBigInt num ->
    const (unsafeViaShow num <> "n")
  TermNumInt num ->
    const (unsafeViaShow num)
  TermNumFloating num ->
    const (unsafeViaShow num)
  TermOperator op _ _ ->
    concatWith (surround (surround (printOperator op) space space))
  TermLambda pat _ ->
    hsep . (printPat pat :) . ("=>" :)
  TermFunction name args _ ->
    hsep
      . ("function" :)
      . (printVar name :)
      . (parens (printArgs args) :)
      . pure
      . braces
      . hsep
  TermCall {} ->
    \case
      f : args -> f <> tupled args
      [] -> mempty
  TermLet binds | parts <- zip (printVar <$> Map.keys binds) ->
    \terms ->
      let bindDocs = sep [k <+> "=" <+> v <> ";" | (k, v) <- parts terms]
       in "let" <+> align bindDocs
  TermBlock {} ->
    braces . hsep

printPat :: Pattern -> Doc Lexeme
printPat (PatternVar var) = printVar var

printVar :: Var -> Doc Lexeme
printVar = annotate LexemeIdentifier . \case Var name -> pretty name

printArgs :: [Var] -> Doc Lexeme
printArgs = parens . hsep . fmap printVar

printOperator :: Operator -> Doc Lexeme
printOperator = annotate LexemeOperator . \case
  Plus -> "+"
  Minus -> "-"
  Mul -> "*"
  Div -> slash
  Pow -> "**"
