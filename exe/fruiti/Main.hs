{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Command
import qualified Data.String as Str
import Error
import Main.Utf8 (withUtf8)
import ReplM
import System.Console.Repline
import Text.Show.Pretty (ppShow)

main :: IO ()
main = withUtf8 do
  runReplM
    ReplOpts
      { banner = pure "Fruit » ",
        command = Command.empty . Str.words,
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

commands :: [(String, [String] -> Repl ())]
commands =
  [ ("help", help),
    ("parse", Command.parse),
    ("def", Command.define),
    ("clear", Command.clear),
    ("jsf", Command.javaScriptFormat),
    ("jse", Command.javaScriptEval),
    ("format", Command.format)
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
      \:def    (or just :d) followed by <expression>\
      \ - parses <expression> and saves it in the session\n\
      \:clear  [<expression>]\
      \ - clears (named expression | all expressions) saved in the session\n\
      \:jsf                 followed by <expression>\
      \ - compiles <expression> to JavaScript and prints it\n\
      \:jse                 followed by <expression>\
      \ - compiles <expression> to JavaScript and evaluates it\n\
      \:help   (or just :h) - prints this help\n"
