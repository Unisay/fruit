{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.String as String
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
      \██╗     █████╗ ███╗   ███╗    ███████╗██████╗ ██╗   ██╗████████╗██╗\n\
      \██║    ██╔══██╗████╗ ████║    ██╔════╝██╔══██╗██║   ██║╚══██╔══╝██║\n\
      \██║    ███████║██╔████╔██║    █████╗  ██████╔╝██║   ██║   ██║   ██║\n\
      \██║    ██╔══██║██║╚██╔╝██║    ██╔══╝  ██╔══██╗██║   ██║   ██║   ╚═╝\n\
      \██║    ██║  ██║██║ ╚═╝ ██║    ██║     ██║  ██║╚██████╔╝   ██║   ██╗\n\
      \╚═╝    ╚═╝  ╚═╝╚═╝     ╚═╝    ╚═╝     ╚═╝  ╚═╝ ╚═════╝    ╚═╝   ╚═╝\n\
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
parse = parseExpr >=> putStrLn . ppShow

parseExpr :: [String] -> Repl AST.Expr
parseExpr =
  liftIO
    . either (die . show) pure
    . Parser.parse @AST.Expr
    . IS.fromString
    . String.unwords

format :: [String] -> Repl ()
format = parseExpr >=> print . PP.renderExpr
