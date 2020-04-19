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
import qualified JS
import qualified Language.Fruit.Core as Core
import qualified Language.Fruit.Data.InputStream as IS
import qualified Language.Fruit.Parser as Parser
import qualified Language.Fruit.Syntax.AST as AST
import qualified Language.Fruit.Syntax.Printer as PP
import ReplM
import System.Console.Repline
import Text.Show.Pretty (ppShow)

empty :: [String] -> Repl ()
empty args = dontCrash do
  term <- parseTerm args
  formatTerm term
  printTerm term

define :: [String] -> Repl ()
define (var : args) = dontCrash do
  term <- parseTerm args
  modify $ Map.insert (JS.Var $ toText var) (astToJsCode term)
define _ = putTextLn "Nothing defined"

clear :: [String] -> Repl ()
clear [var] = modify $ Map.delete (JS.Var $ toText var)
clear _ = put mempty

parse :: [String] -> Repl ()
parse args = dontCrash do
  term <- parseTerm args
  printTerm term

printTerm :: AST.Term -> Repl ()
printTerm term = do
  putTextLn "═════ No optimizations: ═════"
  putStrLn $ ppShow term

-- putTextLn "═════ After optimizations: ═════"
-- putStrLn . ppShow $ Opt.optimize term

format :: [String] -> Repl ()
format args = dontCrash do
  term <- parseTerm args
  formatTerm term

formatTerm :: AST.Term -> Repl ()
formatTerm = putTextLn . renderAnsi

parseTerm :: [String] -> Repl AST.Term
parseTerm args = do
  let input = IS.fromString . String.unwords $ args
  either (throwM . ErrParser) pure $ Parser.parse @AST.Term input

astToCore :: AST.Term -> Core.Term
astToCore = Core.optimize . AST.translateToCore

astToJsCode :: AST.Term -> JS.Code
astToJsCode = renderJs . JS.optimize . JS.translateFromCore . astToCore

renderJs :: JS.Term -> JS.Code
renderJs = JS.Code . JS.renderTerm

javaScriptFormat :: [String] -> Repl ()
javaScriptFormat args = do
  ast <- parseTerm args
  putTextLn "═════ Fruit syntax ═════"
  putStrLn $ ppShow ast
  putTextLn "═════ Fruit core ═════"
  let term = astToCore ast
  putStrLn $ ppShow term
  putTextLn "═════ Unoptimized JS syntax ═════"
  let jsTerm = JS.translateFromCore term
  putStrLn $ ppShow jsTerm
  putTextLn "═════ Optimized JS syntax ═════"
  let jsTerm' = JS.optimize jsTerm
  putStrLn $ ppShow jsTerm'
  putTextLn "═════ JS code ═════"
  let jsCode = renderJs jsTerm'
  putTextLn $ toText jsCode

javaScriptEval :: [String] -> Repl ()
javaScriptEval args = do
  term <- parseTerm args
  env <- get
  evaluated <- JS.evalTerm env (astToJsCode term)
  putTextLn "═════ JS result ═════"
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
