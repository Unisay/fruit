{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Language.Fruit.Data.InputStream as InputStream
import qualified Language.Fruit.Parser as Parser
import qualified Language.Fruit.Syntax.AST as AST
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as O
import qualified System.Path as Path
import Text.Show.Pretty (pPrint)

main :: IO ()
main = withUtf8 do
  Args {argsInput} <- O.execParser argsInfo
  sourceCode <- case argsInput of
    StdInput -> InputStream.readHandle stdin
    FileInput file -> InputStream.readFile (Path.toString file)
  pPrint . Parser.parse @AST.SourceFile $ sourceCode

argsInfo :: O.ParserInfo Args
argsInfo =
  O.info
    (argsParser <**> O.helper)
    ( O.fullDesc
        <> O.progDesc "FRUIT compiler"
        <> O.header "FRUIT - Functional Reactive programming language for the Web"
    )

data Input
  = FileInput Path.AbsRelFile
  | StdInput

newtype Args = Args {argsInput :: Input}

argsParser :: O.Parser Args
argsParser = Args <$> input

input :: O.Parser Input
input = fileInput <|> stdInput
  where
    fileInput :: O.Parser Input
    fileInput =
      FileInput
        <$> O.option
          (O.eitherReader Path.parse)
          ( O.long "file"
              <> O.short 'f'
              <> O.metavar "FILENAME"
              <> O.help "Input file"
          )
    stdInput :: O.Parser Input
    stdInput =
      O.flag' StdInput (O.long "stdin" <> O.help "Read from stdin")
