{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Syntax.Printer
  ( renderTerm,
    printTerm,
    printDefinition,
    Ann (..),
  )
where

import Data.Generics.Uniplate (para)
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    annotate,
    concatWith,
    dot,
    enclose,
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
import Language.Fruit.Syntax.AST
import Prelude hiding ((<>))

data Ann
  = AnnFun
  | AnnLiteral
  | AnnIdentifier
  | AnnKeyword
  deriving (Eq, Show)

renderTerm :: Term -> Text
renderTerm =
  Ansi.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . Doc.unAnnotate
    . printTerm

printDefinition :: Definition -> Doc Ann
printDefinition (Definition _ var term) =
  hcat
    [ printVar var,
      surround "=" space space,
      printTerm term
    ]

printTerm :: Term -> Doc Ann
printTerm = para \case
  TermApp {} -> hsep
  TermLam _ var _ -> mappend (enclose "λ" dot (printVar var)) . hsep
  TermScope {} -> parens . hsep
  TermLit _ literal -> const (printLit literal)
  TermVar _ var -> const (printVar var)
  TermLet _ var _ _ ->
    \xs ->
      annotate AnnKeyword "let"
        <+> printVar var
        <+> equals
        <+> concatWith (surround (surround kwIn space space)) xs
    where
      kwIn :: Doc Ann
      kwIn = annotate AnnKeyword "in"
  TermITE {} -> \case
    [i, t, e] -> hsep ["if", i, "then", t, "else", e]
    _ -> error "Invalid number of terms while printing `if/then/else`"
  TermFun _ fun _ _ ->
    concatWith (surround (surround (printFun fun) space space))

printLit :: Lit -> Doc Ann
printLit = annotate AnnLiteral . \case
  LitInteger i -> unsafeViaShow i
  LitFloating d -> unsafeViaShow d
  LitBoolean b -> unsafeViaShow b

printVar :: Var -> Doc Ann
printVar = annotate AnnIdentifier . \case
  Var ident -> pretty . name $ ident

printFun :: Fun -> Doc Ann
printFun = annotate AnnFun . \case
  Plus -> pretty '+'
  Minus -> pretty '-'
  Mul -> pretty '*'
  Div -> slash
  Pow -> pretty '^'
