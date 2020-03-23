{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Frut.Syntax.AST where

import Data.Data (Data)
import Language.Frut.Data.Ident (Ident)

-- | This is the fundamental unit of parsing - it represents the contents of one source file.
newtype SourceFile
  = SourceFile Module
  deriving (Eq, Ord, Show, Typeable, Data, Generic, NFData)

type QualifiedName = NonEmpty Ident

data Module
  = Module QualifiedName Exports [Import]
  deriving (Eq, Ord, Data, Typeable, NFData, Generic, Show)

newtype Exports
  = Exports (NonEmpty Ident)
  deriving (Eq, Ord, Data, Typeable, NFData, Generic, Show)

data Import
  = Import QualifiedName [Ident]
  deriving (Eq, Ord, Data, Typeable, NFData, Generic, Show)
