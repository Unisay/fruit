{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Command where

import Control.Monad.Catch (throwM)
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (..))
import Data.These (These (..))
import Error
import qualified JS
import qualified Language.Fruit.Core as Core
import qualified Language.Fruit.Data.InputStream as IS
import Language.Fruit.Data.Span (spanOf)
import qualified Language.Fruit.Parser as Parser
import qualified Language.Fruit.Syntax.AST as AST
import qualified Language.Fruit.Syntax.Printer as PP
import System.Console.Repline
import Text.Show.Pretty (ppShow)

type Env = Core.Term -> Core.Term

type Repl = HaskelineT (StateT Env IO)

runReplM :: ReplOpts (StateT Env IO) -> IO ()
runReplM opts = evalStateT (evalReplOpts opts) identity

definition :: [String] -> Repl ()
definition args = do
  defn@(AST.Definition _ var _) <- parseDefinition args
  unless (var == AST.Var "$$") $ modify $ fmap (astToCore defn)
  formatDefinition defn
  printDefinition defn

clear :: [String] -> Repl ()
clear _ = put identity

parse :: [String] -> Repl ()
parse args = dontCrash $ parseDefinition args >>= printDefinition

printDefinition :: AST.Definition -> Repl ()
printDefinition defn = do
  title "Unoptimized definition:"
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
    Left termFail ->
      case Parser.parse @AST.Definition source of
        Right defn ->
          pure defn
        Left definitionFail ->
          throwM (ErrParser (These termFail definitionFail))

astToCore :: AST.Definition -> Core.Term -> Core.Term
astToCore defn = Core.optimize <$> AST.translateDefinitionToCore defn

coreToJsCode :: Core.Term -> JS.Code
coreToJsCode = renderJs . JS.optimize . JS.translateFromCore

renderJs :: JS.Term -> JS.Code
renderJs = JS.Code . JS.renderTerm

data PipelineResults
  = PipelineResults
      { astDefinition :: AST.Definition,
        unoptimizedCoreTerm :: Core.Term,
        optimizedCoreTerm :: Core.Term,
        unoptimizedJsTerm :: JS.Term,
        optimizedJsTerm :: JS.Term,
        jsCode :: JS.Code
      }

compileToJavaScript :: [String] -> Repl PipelineResults
compileToJavaScript args = do
  astDefinition <- parseDefinition args
  let unoptimizedCoreTerm =
        AST.translateDefinitionToCore
          astDefinition
          (Core.Var pseudoVar)
      optimizedCoreTerm = Core.optimize unoptimizedCoreTerm
      unoptimizedJsTerm = JS.translateFromCore optimizedCoreTerm
      optimizedJsTerm = JS.optimize unoptimizedJsTerm
      jsCode = renderJs optimizedJsTerm
  return PipelineResults {..}

formatAsJavaScript :: [String] -> Repl ()
formatAsJavaScript args = do
  PipelineResults {..} <- compileToJavaScript args
  title "1. Fruit syntax"
  putStrLn $ ppShow astDefinition
  title "2. Unoptimized core"
  putStrLn $ ppShow unoptimizedCoreTerm
  title "3. Optimized core"
  putStrLn $ ppShow optimizedCoreTerm
  title "4. Unoptimized JS syntax"
  putStrLn $ ppShow unoptimizedJsTerm
  title "5. Optimized JS syntax"
  putStrLn $ ppShow optimizedJsTerm
  title "6. JS code"
  putTextLn $ toText jsCode

evalAsJavaScript :: [String] -> Repl ()
evalAsJavaScript args = do
  PipelineResults {..} <- compileToJavaScript args
  env <- get
  let js =
        unoptimizedCoreTerm
          & ( env
                >>> Core.optimize
                >>> JS.translateFromCore
                >>> JS.optimize
                >>> renderJs
                >>> JS.enrichWithStdLib
                >>> toText
            )
  title "JS input"
  putTextLn js
  evaluated <- JS.evalRawText js
  title "JS output"
  putTextLn $ toText evaluated

evalJavaScript :: [String] -> Repl ()
evalJavaScript args = do
  let code = foldMap toText (intersperse " " args)
  title "JS code"
  putTextLn code
  title "JS output"
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

pseudoVar :: IsString s => s
pseudoVar = "$$"

title :: MonadIO m => Text -> m ()
title t =
  putTextLn $ Text.takeEnd 80 $ Text.unwords [halfLine, t, halfLine]
  where
    halfLine = Text.replicate lineLength "━"
    lineLength = 40 - (Text.length t + 2) `div` 2
