module Language.Frut.Alex where

import qualified Language.Frut.Data.InputStream as IS
import qualified Language.Frut.Data.Position as Pos

-- Functions required by Alex

-- | type passed around by Alex functions (required by Alex)
type AlexInput = (Pos.Position, IS.InputStream)

-- | get the next byte and new input from the current input
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, inp)
  | IS.inputStreamEmpty inp = Nothing
  | otherwise =
    let (b, inp') = IS.takeByte inp
        -- this is safe for latin-1, but ugly
        pos' = alexMove (chr (fromIntegral b)) pos
     in pos' `seq` Just (b, (pos', inp'))

-- | find the new position given the next character
alexMove :: Char -> Pos.Position -> Pos.Position
alexMove '\n' pos =
  pos
    { Pos.absoluteOffset = Pos.absoluteOffset pos + 1,
      Pos.row = Pos.row pos + 1,
      Pos.col = 1
    }
alexMove '\r' pos = Pos.incOffset 1 pos
alexMove _ pos =
  pos
    { Pos.absoluteOffset = Pos.absoluteOffset pos + 1,
      Pos.row = Pos.row pos,
      Pos.col = succ (Pos.col pos)
    }
