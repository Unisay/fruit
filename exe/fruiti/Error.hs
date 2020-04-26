{-# LANGUAGE DerivingStrategies #-}

module Error where

import Data.These (These (..))
import qualified Language.Fruit.Parser as Parser
import Text.Show (Show (show))
import Text.Show.Pretty (ppShow)

newtype Error = ErrParser (These Parser.ParseFail Parser.ParseFail)

instance Exception Error

instance Show Error where
  show (ErrParser (This definitionFail)) = ppShow definitionFail
  show (ErrParser (That termFail)) = ppShow termFail
  show (ErrParser (These termFail definitionFail)) =
    "Failed to parse Term: " <> ppShow termFail
      <> "\n\
         \Failed to parse Definition: "
      <> ppShow definitionFail
