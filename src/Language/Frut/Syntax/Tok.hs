{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Frut.Syntax.Tok where

import Data.Data (Data)
import Language.Frut.Data.Ident (Ident)

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
  | Decimal Integer
  | Let
  | In
  | Dash
  | Dot
  | Comma
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
