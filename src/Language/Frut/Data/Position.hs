{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- Describes a position or a contiguous region in a file.

module Language.Frut.Data.Position
  ( -- * Positions in files
    Position (..),
    prettyPrint,
    initial,

    -- * Moves
    resetCol,
    incOffset,
    moveUp,
    moveBack,
    moveForw,
    newline,
  )
where

import Data.Data (Data)
import GHC.Show (show, showParen, showString, showsPrec)
import Prelude hiding (show)

-- | A position in a source file.
-- The row and column information is kept only
-- for its convenience and human-readability.
-- Analogous to the information encoded in a cursor.
data Position
  = Position
      { -- | absolute offset the source file.
        absoluteOffset :: !Natural,
        -- | row (line) in the source file.
        row :: !Natural,
        -- | column in the source file.
        col :: !Natural
      }
  deriving (Eq, Data, Typeable, Generic, NFData)

instance Ord Position where
  compare = compare `on` absoluteOffset

-- | Field names are not shown
instance Show Position where
  showsPrec p (Position a r c) =
    showParen
      (p >= 11)
      ( showString "Position"
          . showString " "
          . showsPrec 11 a
          . showString " "
          . showsPrec 11 r
          . showString " "
          . showsPrec 11 c
      )

-- | Pretty print a 'Position'
prettyPrint :: Position -> String
prettyPrint (Position _ r c) = show r <> ":" <> show c

-- | Starting position in a file.
{-# INLINE initial #-}
initial :: Position
initial = Position 0 1 1

{-# INLINE resetCol #-}
resetCol :: Position -> Position
resetCol pos = pos {col = 1}

{-# INLINE moveUp #-}
moveUp :: Natural -> Position -> Position
moveUp n pos@Position {absoluteOffset = o, row = r} =
  pos
    { absoluteOffset = if o > n then o - n else 0,
      row = if r > n then r - n else 1
    }

{-# INLINE newline #-}
newline :: Position -> Position
newline pos@Position {absoluteOffset = o, row = r} =
  pos {absoluteOffset = succ o, row = succ r, col = 1}

{-# INLINE moveBack #-}
moveBack :: Natural -> Position -> Position
moveBack n pos@Position {absoluteOffset = o, col = c} =
  pos
    { absoluteOffset = if o > n then o - n else 0,
      col = if c > n then c - n else 1
    }

{-# INLINE moveForw #-}
moveForw :: Natural -> Position -> Position
moveForw n pos =
  pos
    { absoluteOffset = absoluteOffset pos + n,
      col = col pos + n
    }

-- | Advance only the absolute offset, not the row and column information.
-- Only use this if you know what you are doing!
{-# INLINE incOffset #-}
incOffset :: Natural -> Position -> Position
incOffset n pos = pos {absoluteOffset = absoluteOffset pos + n}
