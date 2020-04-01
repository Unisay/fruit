{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Frut.Data.Span
  ( Span (..),
    Spanned (..),
    Located (..),
    (#),
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
import qualified Language.Frut.Data.Position as Pos
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

-- | Pretty print a 'Span'
prettySpan :: Span -> String
prettySpan (Span lo' hi') = show lo' ++ " - " ++ show hi'

-- | Convenience function lifting 'Mon.<>' to work on all 'Located' things
{-# INLINE (#) #-}
(#) :: (Located a, Located b) => a -> b -> Span
left # right = spanOf left <> spanOf right

-- | smallest covering 'Span'
instance Semigroup Span where
  {-# INLINE (<>) #-}
  Span l1 h1 <> Span l2 h2 = Span (l1 `min` l2) (h1 `max` h2)

instance Monoid Span where
  {-# INLINE mempty #-}
  mempty = Span Pos.initial Pos.initial

  {-# INLINE mappend #-}
  mappend = (<>)

-- | A "tagging" of something with a 'Span' that describes its extent.
data Spanned a = Spanned a !Span
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
  pure x = Spanned x (Span Pos.initial Pos.initial)

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

-- | Describes nodes that can be located - their span can be extracted from them. In general, we
-- expect that for a value constructed as @Con x y z@ where @Con@ is an arbitrary constructor
--
-- prop> (spanOf x <> spanOf y <> spanOf z) `subsetOf` spanOf (Con x y z) == True
class Located a where
  spanOf :: a -> Span

instance Located Span where
  {-# INLINE spanOf #-}
  spanOf = id

instance Located (Spanned a) where
  {-# INLINE spanOf #-}
  spanOf (Spanned _ s) = s

instance Located a => Located (Maybe a) where
  {-# INLINE spanOf #-}
  spanOf = foldMap spanOf

-- | /O(n)/ time complexity
instance Located a => Located [a] where
  {-# INLINE spanOf #-}
  spanOf = foldMap spanOf

-- | /O(n)/ time complexity
instance Located a => Located (NonEmpty a) where
  {-# INLINE spanOf #-}
  spanOf = foldMap spanOf
