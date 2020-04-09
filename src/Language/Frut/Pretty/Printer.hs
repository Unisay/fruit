{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Language.Frut.Pretty.Printer
  ( renderExpr,
    printExpr,
    Ann (..),
  )
where

import Data.Generics.Uniplate (para)
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import Language.Frut.Syntax.AST
import Prelude hiding ((<>))

data Ann
  = AnnOperator
  | AnnLiteral
  deriving (Eq, Show)

renderExpr :: ExpX ξ -> Text
renderExpr =
  Ansi.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . Doc.unAnnotate
    . printExpr

printExpr :: ExpX ξ -> Doc Ann
printExpr = toVanilla >>> para \case
  ScopeX _ _ ->
    Doc.parens . Doc.hsep
  LitX _ literal ->
    const (printLiteral literal)
  OpX _ op _ _ ->
    Doc.concatWith
      (Doc.surround (Doc.surround (printOperator op) Doc.space Doc.space))
  ExpX _ ->
    Doc.hcat

printLiteral :: Literal -> Doc Ann
printLiteral = Doc.annotate AnnLiteral . \case
  LitInteger i -> Doc.unsafeViaShow i
  LitFloating d -> Doc.unsafeViaShow d

printOperator :: Operator -> Doc Ann
printOperator = Doc.annotate AnnOperator . \case
  OperatorPlus -> Doc.pretty '+'
  OperatorMinus -> Doc.pretty '-'
  OperatorTimes -> Doc.pretty '*'
  OperatorDiv -> Doc.slash
  OperatorPow -> Doc.pretty '^'
