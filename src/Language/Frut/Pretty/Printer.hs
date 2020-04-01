{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Language.Frut.Pretty.Printer
  ( renderExpr,
    printExpr,
    Ann (..),
  )
where

import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc ((<+>), Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import Language.Frut.Syntax.AST
import Language.Frut.Syntax.Precedence
  ( Associativity (..),
    assoc,
    prec,
  )
import Prelude hiding ((<>))

data Ann
  = AnnOperator
  | AnnLiteral
  deriving (Eq, Show)

printExpr :: ExpX ξ -> Doc Ann
printExpr = printExpr' Nothing

renderExpr :: ExpX ξ -> Text
renderExpr =
  Ansi.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . Doc.unAnnotate
    . printExpr

printExpr' :: Maybe (Either Operator Operator) -> ExpX ξ -> Doc Ann
printExpr' mbParentOp = \case
  LitX _ literal -> printLiteral literal
  OpX _ op e1 e2 ->
    case mbParentOp of
      Just parentOp ->
        case either prec prec parentOp `compare` prec op of
          EQ ->
            -- same precedence
            case (either assoc assoc parentOp, assoc op) of
              (FullAssoc, FullAssoc) -> doc
              (FullAssoc, LeftAssoc) -> doc
              (LeftAssoc, FullAssoc) -> doc
              (LeftAssoc, LeftAssoc) ->
                case parentOp of
                  Left _ -> doc
                  Right _ -> Doc.parens doc
              (RightAssoc, RightAssoc) ->
                case parentOp of
                  Left _ -> Doc.parens doc
                  Right _ -> doc
              _ -> Doc.parens doc
          LT -> doc
          GT -> Doc.parens doc
      Nothing -> doc
    where
      doc :: Doc Ann
      doc =
        printExpr' (Just (Left op)) e1
          <+> printOperator op
          <+> printExpr' (Just (Right op)) e2
  ExpX _ -> mempty

printLiteral :: Literal -> Doc Ann
printLiteral = Doc.annotate AnnLiteral . \case
  Literal i -> Doc.unsafeViaShow i

printOperator :: Operator -> Doc Ann
printOperator = Doc.annotate AnnOperator . \case
  OperatorPlus -> Doc.pretty '+'
  OperatorMinus -> Doc.pretty '-'
  OperatorTimes -> Doc.pretty '*'
  OperatorDiv -> Doc.slash
  OperatorPow -> Doc.pretty '^'
