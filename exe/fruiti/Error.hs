{-# LANGUAGE DerivingStrategies #-}

module Error where

import qualified Language.Fruit.Parser as Parser

newtype Error = ErrParser Parser.ParseFail
  deriving stock (Show)

instance Exception Error
