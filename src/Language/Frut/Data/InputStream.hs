module Language.Frut.Data.InputStream
  ( -- * InputStream type
    InputStream,
    countLines,
    inputStreamEmpty,

    -- * Introduction forms
    readFile,
    readHandle,
    fromString,

    -- * Elimination forms
    toString,
    takeByte,
    takeChar,
    peekChars,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BE
import qualified Data.String as Str
import GHC.Show (show)
import System.IO hiding (readFile)
import Prelude hiding (fromString, readFile, show, toString)

-- | Read an encoded file into an 'InputStream'
readFile :: FilePath -> IO InputStream
readFile f = coerce <$> BS.readFile f
{-# INLINE readFile #-}

-- | Read an 'InputStream' from a 'Handle'
readHandle :: Handle -> IO InputStream
readHandle h = coerce <$> BS.hGetContents h
{-# INLINE readHandle #-}

-- | Convert 'InputStream' to 'String'.
toString :: InputStream -> String
toString = BE.toString . coerce
{-# INLINE toString #-}

-- | Convert a 'String' to an 'InputStream'.
fromString :: String -> InputStream
fromString = IS . BE.fromString
{-# INLINE fromString #-}

-- | Uses 'fromString'
instance Str.IsString InputStream where
  fromString = fromString

-- | Read the first byte from an 'InputStream' and return that byte with what remains of the
-- 'InputStream'. Behaviour is undefined when 'inputStreamEmpty' returns 'True'.
--
-- >>> takeByte "foo bar"
-- (102, "oo bar")
takeByte :: InputStream -> (Word8, InputStream)
takeByte bs = (BS.head (coerce bs), coerce (BS.tail (coerce bs)))
{-# INLINE takeByte #-}

-- | Read the first character from an 'InputStream' and return that 'Char' with what remains of the
-- 'InputStream'. Behaviour is undefined when 'inputStreamEmpty' returns 'True'.
--
-- >>> takeChar "foo bar"
-- ('f', "oo bar")
takeChar :: InputStream -> (Char, InputStream)
takeChar bs =
  maybe (error "takeChar: no char left") coerce (BE.uncons (coerce bs))
{-# INLINE takeChar #-}

-- | Return @True@ if the given input stream is empty.
--
-- >>> inputStreamEmpty ""
-- True
--
-- >>> inputStreamEmpty "foo"
-- False
inputStreamEmpty :: InputStream -> Bool
inputStreamEmpty = BS.null . coerce
{-# INLINE inputStreamEmpty #-}

-- | Returns the first @n@ characters of the given input stream, without removing them.
--
-- >>> peekChars 5 "foo bar"
-- "foo ba"
--
-- >>> peekChars 5 "foo"
-- "foo"
peekChars :: Int -> InputStream -> String
peekChars n = BE.toString . BE.take n . coerce
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
countLines = length . BE.lines . coerce
{-# INLINE countLines #-}

-- | Opaque input type.
newtype InputStream
  = IS BS.ByteString
  deriving (Eq, Ord)

instance Show InputStream where
  show (IS bs) = show bs
