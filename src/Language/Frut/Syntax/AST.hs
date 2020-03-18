{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Frut.Syntax.AST where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Frut.Data.Ident (Ident)
import Data.List.NonEmpty (NonEmpty)

-- | This is the fundamental unit of parsing - it represents the contents of one source file.
data SourceFile
  = SourceFile Module
  deriving (Eq, Ord, Show, Typeable, Data, Generic, NFData)

data Module
  = Module (NonEmpty Ident)
  deriving (Eq, Ord, Data, Typeable, NFData, Generic, Show)
