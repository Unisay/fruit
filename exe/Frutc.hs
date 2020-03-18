{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Frutc where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Language.Frut.Parser as Parser
import qualified Language.Frut.Syntax.AST as AST
import Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as O
import System.IO (stdin)
import qualified System.Path as Path
import Text.Show.Pretty (pPrint)
import Language.Frut.Data.InputStream (readInputStream, hReadInputStream)

main :: IO ()
main = do
  setLocaleEncoding utf8
  Args {argsInput} <- O.execParser argsInfo
  sourceCode <- case argsInput of
    StdInput -> hReadInputStream stdin
    FileInput file -> readInputStream (Path.toString file)
  pPrint . Parser.parse @AST.SourceFile $ sourceCode

argsInfo :: O.ParserInfo Args
argsInfo =
  O.info
    (argsParser <**> O.helper)
    ( O.fullDesc
        <> O.progDesc "FRUT compiler"
        <> O.header "FRUT - Functional Reactive programming language for the Web"
    )

data Input
  = FileInput Path.AbsRelFile
  | StdInput

data Args
  = Args
      { argsInput :: Input
      }

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
