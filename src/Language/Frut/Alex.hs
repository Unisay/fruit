module Language.Frut.Alex where

import Data.Char (chr)
import Data.Word (Word8)
import qualified Language.Frut.Data.InputStream as IS
import Language.Frut.Data.Position

-- Functions required by Alex

-- | type passed around by Alex functions (required by Alex)
type AlexInput = (Position, IS.InputStream)

-- | get the next byte and new input from the current input
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, inp)
  | IS.inputStreamEmpty inp = Nothing
  | otherwise =
    let (b, inp') = IS.takeByte inp
        -- this is safe for latin-1, but ugly
        pos' = alexMove pos (chr (fromIntegral b))
     in pos' `seq` Just (b, (pos', inp'))

-- | find the new position given the next character
alexMove :: Position -> Char -> Position
alexMove pos ' ' = incPos pos 1
alexMove pos '\n' = retPos pos
alexMove pos '\r' = incOffset pos 1
alexMove pos _ = incPos pos 1
