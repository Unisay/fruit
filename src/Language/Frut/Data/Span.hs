{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Frut.Data.Span
  ( Span (..),
    Spanned (..),
    mapLo,
    mapHi,
    unspan,
    prettySpan,
    subsetOf,
  )
where

import Data.Data (Data)
import GHC.Show (show, showParen, showString, showsPrec)
import Language.Frut.Data.Position (Position)
import Prelude hiding (show)

-- | Spans represent a contiguous region of code,
-- delimited by two 'Position's. The endpoints are
-- inclusive. Analogous to the information encoded in a selection.
data Span = Span {lo, hi :: !Position}
  deriving (Eq, Ord, Data, Typeable, Generic, NFData)

-- | Field names are not shown
instance Show Span where
  showsPrec p (Span l h) =
    showParen
      (p >= 11)
      ( showString "Span"
          . showString " "
          . showsPrec 11 l
          . showString " "
          . showsPrec 11 h
      )

-- | Check if a span is a subset of another span
subsetOf :: Span -> Span -> Bool
subsetOf (Span l1 h1) (Span l2 h2) =
  min l1 l2 == l1 && max h1 h2 == h2

-- | smallest covering 'Span'
instance Semigroup Span where
  {-# INLINE (<>) #-}
  Span l1 h1 <> Span l2 h2 = Span (l1 `min` l2) (h1 `max` h2)

-- | Pretty print a 'Span'
prettySpan :: Span -> String
prettySpan (Span lo' hi') = show lo' ++ " - " ++ show hi'

-- | A "tagging" of something with a 'Span' that describes its extent.
data Spanned a = Spanned a !(Maybe Span) -- TODO: avoid Maybe?
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NFData)

-- | Extract the wrapped value from 'Spanned'
{-# INLINE unspan #-}
unspan :: Spanned a -> a
unspan (Spanned x _) = x

instance Functor Spanned where
  {-# INLINE fmap #-}
  fmap f (Spanned x s) = Spanned (f x) s

instance Applicative Spanned where
  {-# INLINE pure #-}
  pure x = Spanned x Nothing

  {-# INLINE (<*>) #-}
  Spanned f s1 <*> Spanned x s2 = Spanned (f x) (s1 <> s2)

instance Monad Spanned where
  return = pure
  Spanned x s1 >>= f =
    let Spanned y s2 = f x
     in Spanned y (s1 <> s2)

mapLo :: (Position -> Position) -> Span -> Span
mapLo f (Span l h) = Span (f l) h

mapHi :: (Position -> Position) -> Span -> Span
mapHi f (Span l h) = Span l (f h)
