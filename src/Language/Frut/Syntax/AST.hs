{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Frut.Syntax.AST where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Frut.Data.Ident (Ident)

-- | This is the fundamental unit of parsing - it represents the contents of one source file.
data SourceFile
  = SourceFile Module
  deriving (Eq, Ord, Show, Typeable, Data, Generic, NFData)

type QualifiedName = NonEmpty Ident

data Module
  = Module QualifiedName Exports [Import]
  deriving (Eq, Ord, Data, Typeable, NFData, Generic, Show)

data Exports
  = Exports (NonEmpty Ident)
  deriving (Eq, Ord, Data, Typeable, NFData, Generic, Show)

data Import
  = Import QualifiedName [Ident]
  deriving (Eq, Ord, Data, Typeable, NFData, Generic, Show)
