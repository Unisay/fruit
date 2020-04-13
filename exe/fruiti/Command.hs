{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Command where

import Control.Monad.Catch (throwM)
import qualified Data.Map as Map
import qualified Data.String as String
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (..))
import Error
import qualified Js.Eval as JS
import qualified Js.Types as Js
import qualified Language.Fruit.Data.InputStream as IS
import qualified Language.Fruit.Js.Printer as JS
import qualified Language.Fruit.Optimizer as Opt
import qualified Language.Fruit.Parser as Parser
import qualified Language.Fruit.Syntax.AST as AST
import qualified Language.Fruit.Syntax.Printer as PP
import ReplM
import System.Console.Repline
import Text.Show.Pretty (ppShow)
import Types (Var (..))

empty :: [String] -> Repl ()
empty args = dontCrash do
  term <- case args of
    ("let" : var : "=" : rest) | not (null rest) -> do
      term' <- parseTerm rest
      modify $ Map.insert (Var $ toText var) term'
      return term'
    _ -> parseTerm args
  formatTerm term
  printTerm term

parse :: [String] -> Repl ()
parse args = dontCrash do
  term <- parseTerm args
  printTerm term

printTerm :: AST.Term -> Repl ()
printTerm term = do
  putTextLn "═════ Before optimizations: ═════"
  putStrLn $ ppShow term
  putTextLn "═════ After optimizations: ═════"
  putStrLn . ppShow $ Opt.optimize term

format :: [String] -> Repl ()
format args = dontCrash do
  term <- parseTerm args
  formatTerm term

formatTerm :: AST.Term -> Repl ()
formatTerm term = do
  let optimized = Opt.optimize term
  putTextLn . renderAnsi . Opt.optimize $ optimized

parseTerm :: [String] -> Repl AST.Term
parseTerm args = do
  let input = IS.fromString . String.unwords $ args
  either (throwM . ErrParser) pure $ Parser.parse @AST.Term input

termToJavascript :: AST.Term -> Js.Code
termToJavascript = Js.Code . JS.renderTerm . Opt.optimize

javaScriptFormat' :: [String] -> Repl Js.Code
javaScriptFormat' = parseTerm >=> pure . termToJavascript

javaScriptFormat :: [String] -> Repl ()
javaScriptFormat args = do
  jsCode <- javaScriptFormat' args
  putTextLn $ "JS » " <> toText jsCode

javaScriptEval :: [String] -> Repl ()
javaScriptEval args = do
  term <- parseTerm args
  env <- get
  evaluated <- JS.evalTerm env termToJavascript term
  putTextLn $ toText evaluated

renderAnsi :: AST.Term -> Text
renderAnsi =
  Ansi.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . Doc.reAnnotate colorScheme
    . PP.printTerm

colorScheme :: PP.Ann -> AnsiStyle
colorScheme = \case
  PP.AnnFun -> Ansi.color Cyan
  PP.AnnLiteral -> Ansi.color Red
  PP.AnnIdentifier -> Ansi.color Magenta
  PP.AnnKeyword -> Ansi.color Green
