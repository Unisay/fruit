{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Catch (throwM)
import qualified Data.String as String
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (..))
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import qualified Js.Eval as JS
import qualified Language.Frut.Data.InputStream as IS
import qualified Language.Frut.Js.Printer as JS
import qualified Language.Frut.Optimizer as Opt
import qualified Language.Frut.Parser as Parser
import qualified Language.Frut.Pretty.Printer as PP
import qualified Language.Frut.Syntax.AST as AST
import Main.Utf8 (withUtf8)
import System.Console.Repline
import Text.Show.Pretty (ppShow)

newtype Error = ErrParser Parser.ParseFail
  deriving stock (Show)

instance Exception Error

type Repl a = HaskelineT IO a

main :: IO ()
main = withUtf8 do
  evalReplOpts
    ReplOpts
      { banner = pure "Frut » ",
        command = cmd,
        options = commands,
        prefix = Just ':',
        tabComplete = Word0 completer,
        initialiser = ini
      }

handleError :: Error -> IO ()
handleError = \case
  ErrParser parserFailure ->
    putStrLn (ppShow parserFailure)

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["help", "parse", "format"]
  return $ filter (isPrefixOf n) names

ini :: Repl ()
ini =
  liftIO $
    putStrLn
      "\n\
      \              __\n\
      \  |   _  _   |__   .|_ \n\
      \  |  (_||||  || |_|||_ \n\
      \ \n\n\
      \(type :help for a list of available commands) \n"

cmd :: String -> Repl ()
cmd = parse . pure

commands :: [(String, [String] -> Repl ())]
commands =
  [ ("help", help),
    ("parse", parse),
    ("js", toJavaScript),
    ("format", format)
  ]

help :: [String] -> Repl ()
help _ =
  liftIO $
    putText
      "\
      \Try these commands:\n\
      \:parse  (or just :p) followed by <expression>\
      \ - parses <expression>\n\
      \:format (or just :f) followed by <expression>\
      \ - formats <expression>\n\
      \:js                  followed by <expression>\
      \ - compiles <expression> to JavaScript\n\
      \:help   (or just :h) - prints this help\n"

parse :: [String] -> Repl ()
parse args = do
  expParsed <- parseExpr args
  putTextLn "═════ Before optimizations: ═════"
  putStrLn $ ppShow expParsed
  putTextLn "═════ After optimizations: ═════"
  putStrLn . ppShow $ Opt.optimize expParsed

format :: [String] -> Repl ()
format args = do
  expParsed <- parseExpr args
  let optimized = Opt.optimize expParsed
  putTextLn . renderAnsi . Opt.optimize $ optimized

parseExpr :: [String] -> Repl AST.ExpParsed
parseExpr args = do
  let input = IS.fromString . String.unwords $ args
  either (throwM . ErrParser) pure $ Parser.parse @AST.ExpParsed input

toJavaScript :: [String] -> Repl ()
toJavaScript args = do
  expParsed <- parseExpr args
  let optimized = Opt.optimize expParsed
      javaScript = JS.renderExpr optimized
  putTextLn $ "JS » " <> javaScript
  evaluated <- JS.evalExpr javaScript
  putTextLn evaluated

renderAnsi :: AST.ExpParsed -> Text
renderAnsi =
  Ansi.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . Doc.reAnnotate colorScheme
    . PP.printExpr

colorScheme :: PP.Ann -> AnsiStyle
colorScheme = \case
  PP.AnnOperator -> Ansi.color Cyan
  PP.AnnLiteral -> Ansi.color Red
