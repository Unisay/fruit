{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Command
import qualified Data.String as Str
import Error
import Main.Utf8 (withUtf8)
import System.Console.Repline
import Text.Show.Pretty (ppShow)

main :: IO ()
main = withUtf8 do
  Command.runReplM
    ReplOpts
      { banner = pure "Fruit » ",
        command = dontCrash . Command.definition . Str.words,
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

ini :: Command.Repl ()
ini =
  liftIO $
    putStrLn
      "\n\
      \              __\n\
      \  |   _  _   |__   .|_ \n\
      \  |  (_||||  || |_|||_ \n\
      \ \n\n\
      \(type :help for a list of available commands) \n"

commands :: [(String, [String] -> Command.Repl ())]
commands =
  [ ("help", help),
    ("parse", fmap dontCrash Command.parse),
    ("clear", Command.clear),
    ("jsf", dontCrash . void . Command.formatAsJavaScript),
    ("jse", fmap dontCrash Command.evalAsJavaScript),
    ("jsr", fmap dontCrash Command.evalJavaScript),
    ("format", fmap dontCrash Command.format)
  ]

help :: [String] -> Command.Repl ()
help _ =
  liftIO $
    putText
      "\
      \Try these commands:\n\
      \:parse  (or just :p) followed by <expression>\
      \ - parses <expression>\n\
      \:format (or just :f) followed by <expression>\
      \ - formats <expression>\n\
      \:clear \
      \ - clears all expressions saved in the session\n\
      \:jsf                 followed by <expression>\
      \ - compiles <expression> to JavaScript and prints it\n\
      \:jse                 followed by <expression>\
      \ - compiles <expression> to JavaScript and evaluates it\n\
      \:jsr                 followed by <JavaScript code>\
      \ - evaluates raw JavaScript\n\
      \:help   (or just :h) - prints this help\n"
