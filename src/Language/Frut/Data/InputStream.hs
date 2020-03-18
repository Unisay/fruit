module Language.Frut.Data.InputStream
  ( -- * InputStream type
    InputStream,
    countLines,
    inputStreamEmpty,

    -- * Introduction forms
    readInputStream,
    hReadInputStream,
    inputStreamFromString,

    -- * Elimination forms
    inputStreamToString,
    takeByte,
    takeChar,
    peekChars,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BE
import Data.Coerce (coerce)
import Data.String (IsString (..))
import Data.Word (Word8)
import System.IO
import GHC.Show (Show, show)
import Prelude hiding (show)

-- | Read an encoded file into an 'InputStream'
readInputStream :: FilePath -> IO InputStream
{-# INLINE readInputStream #-}

-- | Read an 'InputStream' from a 'Handle'
hReadInputStream :: Handle -> IO InputStream
{-# INLINE hReadInputStream #-}

-- | Convert 'InputStream' to 'String'.
inputStreamToString :: InputStream -> String
{-# INLINE inputStreamToString #-}

-- | Convert a 'String' to an 'InputStream'.
inputStreamFromString :: String -> InputStream
{-# INLINE inputStreamFromString #-}

-- | Uses 'inputStreamFromString'
instance IsString InputStream where fromString = inputStreamFromString

-- | Read the first byte from an 'InputStream' and return that byte with what remains of the
-- 'InputStream'. Behaviour is undefined when 'inputStreamEmpty' returns 'True'.
--
-- >>> takeByte "foo bar"
-- (102, "oo bar")
takeByte :: InputStream -> (Word8, InputStream)
{-# INLINE takeByte #-}

-- | Read the first character from an 'InputStream' and return that 'Char' with what remains of the
-- 'InputStream'. Behaviour is undefined when 'inputStreamEmpty' returns 'True'.
--
-- >>> takeChar "foo bar"
-- ('f', "oo bar")
takeChar :: InputStream -> (Char, InputStream)
{-# INLINE takeChar #-}

-- | Return @True@ if the given input stream is empty.
--
-- >>> inputStreamEmpty ""
-- True
--
-- >>> inputStreamEmpty "foo"
-- False
inputStreamEmpty :: InputStream -> Bool
{-# INLINE inputStreamEmpty #-}

-- | Returns the first @n@ characters of the given input stream, without removing them.
--
-- >>> peekChars 5 "foo bar"
-- "foo ba"
--
-- >>> peekChars 5 "foo"
-- "foo"
peekChars :: Int -> InputStream -> String
{-# INLINE peekChars #-}

-- | Returns the number of text lines in the given 'InputStream'
--
-- >>> countLines ""
-- 0
--
-- >>> countLines "foo"
-- 1
--
-- >>> countLines "foo\n\nbar"
-- 3
--
-- >>> countLines "foo\n\nbar\n"
-- 3
countLines :: InputStream -> Int
{-# INLINE countLines #-}

-- | Opaque input type.
newtype InputStream
  = IS BS.ByteString
  deriving (Eq, Ord)

takeByte bs = (BS.head (coerce bs), coerce (BS.tail (coerce bs)))

takeChar bs = maybe (error "takeChar: no char left") coerce (BE.uncons (coerce bs))

inputStreamEmpty = BS.null . coerce

peekChars n = BE.toString . BE.take n . coerce

readInputStream f = coerce <$> BS.readFile f

hReadInputStream h = coerce <$> BS.hGetContents h

inputStreamToString = BE.toString . coerce

inputStreamFromString = IS . BE.fromString

countLines = length . BE.lines . coerce

instance Show InputStream where
  show (IS bs) = show bs
