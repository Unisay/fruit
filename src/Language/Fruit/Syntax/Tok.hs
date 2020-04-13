{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Fruit.Syntax.Tok where

import Data.Data (Data)
import Language.Fruit.Data.Ident (Ident)

data Tok
  = Indent
  | Dedent
  | Newline
  | Module
  | Imports
  | Exports
  | LParen
  | RParen
  | UpperId Ident
  | LowerId Ident
  | Integer Integer
  | Floating Double
  | Let
  | In
  | Dash
  | Dot
  | Comma
  | Plus
  | Times
  | Div
  | Pow
  | Equal
  | Lambda
  | LeftArrow
  | RightArrow
  | Space Space
  | EOF
  deriving (Eq, Show)

data Space
  = -- | usual white space: @[\\ \\t\\n\\f\\v\\r]+@
    Whitespace
  | -- | comment (either inline or not)
    Comment
  deriving
    ( Eq,
      Ord,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic,
      NFData
    )
