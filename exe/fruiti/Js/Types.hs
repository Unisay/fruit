{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Js.Types where

newtype Code = Code Text
  deriving newtype (Show, ToText)

newtype Result = Result Text
  deriving newtype (Show, ToText)
