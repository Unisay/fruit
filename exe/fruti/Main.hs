{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.String as String
import qualified Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (..))
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import qualified Language.Frut.Data.InputStream as IS
import qualified Language.Frut.Parser as Parser
import qualified Language.Frut.Pretty.Printer as PP
import qualified Language.Frut.Syntax.AST as AST
import Main.Utf8 (withUtf8)
import System.Console.Repline
import Text.Show.Pretty (ppShow)

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
      \  ██╗     █████╗ ███╗   ███╗    ███████╗██████╗ ██╗   ██╗████████╗██╗\n\
      \  ██║    ██╔══██╗████╗ ████║    ██╔════╝██╔══██╗██║   ██║╚══██╔══╝██║\n\
      \  ██║    ███████║██╔████╔██║    █████╗  ██████╔╝██║   ██║   ██║   ██║\n\
      \  ██║    ██╔══██║██║╚██╔╝██║    ██╔══╝  ██╔══██╗██║   ██║   ██║   ╚═╝\n\
      \  ██║    ██║  ██║██║ ╚═╝ ██║    ██║     ██║  ██║╚██████╔╝   ██║   ██╗\n\
      \  ╚═╝    ╚═╝  ╚═╝╚═╝     ╚═╝    ╚═╝     ╚═╝  ╚═╝ ╚═════╝    ╚═╝   ╚═╝\n\
      \\n\
      \(type :help for a list of available commands) \n"

cmd :: String -> Repl ()
cmd = parse . pure

commands :: [(String, [String] -> Repl ())]
commands =
  [ ("help", help),
    ("parse", parse),
    ("format", format)
  ]

help :: [String] -> Repl ()
help _ =
  liftIO $
    putText
      "\
      \Try these commands:\n\
      \:parse  (or just :p) followed by <expression> - parses <expression>\n\
      \:format (or just :f) followed by <expression> - formats <expression>\n\
      \:help   (or just :h) - prints this help\n"

parse :: [String] -> Repl ()
parse = parseExpr >=> putStrLn . either ppShow ppShow

parseExpr :: [String] -> Repl (Either Parser.ParseFail AST.Expr)
parseExpr =
  pure . Parser.parse @AST.Expr . IS.fromString . String.unwords

format :: [String] -> Repl ()
format =
  parseExpr >=> putTextLn . either renderParseFail renderAnsi

renderParseFail :: Parser.ParseFail -> Text
renderParseFail = fromString . ppShow

renderAnsi :: AST.Expr -> Text
renderAnsi =
  Ansi.renderStrict
    . Doc.layoutPretty Doc.defaultLayoutOptions
    . Doc.reAnnotate colorScheme
    . PP.printExpr

colorScheme :: PP.Ann -> AnsiStyle
colorScheme = \case
  PP.InfixOp -> Ansi.color Cyan
  PP.Literal -> Ansi.color Red
