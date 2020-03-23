{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Frut.Data.Ident
  ( Ident (..),
    mkIdent,
  )
where

import Data.Data (Data)
import Data.Semigroup as Sem
import GHC.Show (show)
import Prelude hiding (show)

-- | An identifier
data Ident
  = Ident
      { -- | payload of the identifier
        name :: String,
        -- | hash for quick comparision
        hash :: {-# UNPACK #-} !Int
      }
  deriving (Data, Typeable, Generic, NFData)

-- | Shows the identifier as a string (for use with @-XOverloadedStrings@)
instance Show Ident where
  show = show . name

instance IsString Ident where
  fromString = mkIdent

-- | Uses 'hash' to short-circuit
instance Eq Ident where
  i1 == i2 = hash i1 == hash i2 && name i1 == name i2
  i1 /= i2 = hash i1 /= hash i2 || name i1 /= name i2

-- | Uses 'hash' to short-circuit
instance Ord Ident where
  compare i1 i2 =
    case compare i1 i2 of
      EQ -> compare (name i1) (name i2)
      rt -> rt

-- | "Forgets" about whether either argument was raw
instance Monoid Ident where
  mappend = (<>)
  mempty = mkIdent ""

-- | "Forgets" about whether either argument was raw
instance Semigroup Ident where
  Ident n1 _ <> Ident n2 _ = mkIdent (n1 <> n2)

-- | Smart constructor for making an 'Ident'.
mkIdent :: String -> Ident
mkIdent s = Ident s (hashString s)

-- | Hash a string into an 'Int'
hashString :: String -> Int
hashString = foldl' f golden
  where
    f m c = ord c * magic + m
    magic = 0xdeadbeef
    golden = 1013904242
