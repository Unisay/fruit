{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Language.Fruit.Data.Span (spanOf)
import qualified Language.Fruit.Parser as Parser
import qualified Language.Fruit.Syntax.AST as AST
import qualified Language.Fruit.Syntax.Printer as PP
import ReplM
import System.Console.Repline
import Text.Show.Pretty (ppShow)

definition :: [String] -> Repl ()
definition args = dontCrash do
  defn@(AST.Definition _ var _) <- parseDefinition args
  unless (var == AST.Var "$$") do
    modify $ Map.insert (JS.Var $ toText var) (astToJsCode defn)
  formatDefinition defn
  printDefinition defn

clear :: [String] -> Repl ()
clear [var] = modify $ Map.delete (JS.Var $ toText var)
clear _ = put mempty

parse :: [String] -> Repl ()
parse args = dontCrash $ parseDefinition args >>= printDefinition

printDefinition :: AST.Definition -> Repl ()
printDefinition defn = do
  putTextLn "═════ Unoptimized definition: ═════"
  putStrLn $ ppShow defn

format :: [String] -> Repl ()
format args = dontCrash $ parseDefinition args >>= formatDefinition

formatDefinition :: AST.Definition -> Repl ()
formatDefinition = putTextLn . renderDefinitionAnsi

parseDefinition :: [String] -> Repl AST.Definition
parseDefinition args = do
  let source = IS.fromString . String.unwords $ args
  case Parser.parse @AST.Term source of
    Right term ->
      pure $ AST.Definition (spanOf term) pseudoVar term
    Left _ ->
      case Parser.parse @AST.Definition source of
        Left definitionFail ->
          throwM (ErrParser definitionFail)
        Right defn ->
          pure defn

astToCore :: AST.Definition -> AST.Term -> Core.Term
astToCore defn = Core.optimize <$> AST.translateDefinitionToCore defn

astToJsCode :: AST.Definition -> JS.Code
astToJsCode defn =
  let core = astToCore defn pseudoTerm
   in renderJs . JS.optimize . JS.translateFromCore $ core

renderJs :: JS.Term -> JS.Code
renderJs = JS.Code . JS.renderTerm

formatAsJavaScript :: [String] -> Repl ()
formatAsJavaScript args = do
  defn <- parseDefinition args
  putTextLn "═════ Fruit syntax ═════"
  putStrLn $ ppShow defn
  putTextLn "═════ Fruit core ═════"
  let term = astToCore defn pseudoTerm
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

evalAsJavaScript :: [String] -> Repl ()
evalAsJavaScript args = dontCrash do
  defn <- parseDefinition args
  env <- get
  evaluated <- JS.evalTerm env (astToJsCode defn)
  putTextLn "═════ JS code ═════"
  putTextLn $ toText evaluated

evalJavaScript :: [String] -> Repl ()
evalJavaScript args = dontCrash do
  let code = foldMap toText (intersperse " " args)
  putTextLn "═════ JS code ═════"
  putTextLn code
  putTextLn "═════ JS output ═════"
  putTextLn . toText =<< JS.evalRawText code

renderDefinitionAnsi :: AST.Definition -> Text
renderDefinitionAnsi =
  Ansi.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . Doc.reAnnotate colorScheme
    . PP.printDefinition

colorScheme :: PP.Ann -> AnsiStyle
colorScheme = \case
  PP.AnnFun -> Ansi.color Cyan
  PP.AnnLiteral -> Ansi.color Red
  PP.AnnIdentifier -> Ansi.color Magenta
  PP.AnnKeyword -> Ansi.color Green

pseudoTerm :: AST.Term
pseudoTerm = AST.TermVar mempty pseudoVar

pseudoVar :: AST.Var
pseudoVar = AST.Var "$$"
