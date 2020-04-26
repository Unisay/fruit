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
  TermLit lit ->
    const (printLit lit)
  TermOperator op _ _ ->
    concatWith (surround (surround (printOperator op) space space))
  TermConditional {} ->
    \case
      [i, t, e] -> hsep [i, "?", t, ":", e]
      _ -> err 3 "TermConditional"
  TermLambda pat _ ->
    parens . hsep . (printPat pat :) . ("=>" :)
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
      _ -> err 0 "TermCall"
  TermLet binds | parts <- zip (printVar <$> Map.keys binds) ->
    \terms ->
      let bindDocs = sep [k <+> "=" <+> v <> ";" | (k, v) <- parts terms]
       in "let" <+> align bindDocs
  TermBlock {} ->
    braces . hsep
  where
    err :: Int -> Text -> a
    err n c =
      error $
        "Language.Fruit.Js.Printer.printTerm: \
        \unexpected number of sub-terms received (/= "
          <> show n
          <> ") while traversing "
          <> c
          <> " constructor"

printPat :: Pattern -> Doc Lexeme
printPat (PatternVar var) = printVar var

printVar :: Var -> Doc Lexeme
printVar = annotate LexemeIdentifier . \case
  Var name -> pretty name

printLit :: Lit -> Doc Lexeme
printLit = annotate LexemeLiteral . \case
  LitNumber n -> unsafeViaShow n
  LitBigInt i -> unsafeViaShow i
  LitBoolean True -> "true"
  LitBoolean False -> "false"

printArgs :: [Var] -> Doc Lexeme
printArgs = parens . hsep . fmap printVar

printOperator :: Operator -> Doc Lexeme
printOperator = annotate LexemeOperator . \case
  Plus -> "+"
  Minus -> "-"
  Mul -> "*"
  Div -> slash
  Pow -> "**"
