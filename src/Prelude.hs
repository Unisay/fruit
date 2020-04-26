{-# LANGUAGE TypeOperators #-}

module Prelude
  ( module Relude,
    type (∨),
  )
where

import Relude

type (∨) a b = Either a b

infixr 5 ∨
