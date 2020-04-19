-- {-# LANGUAGE LambdaCase #-}

module Language.Fruit.Core.Precedence where

-- import Language.Fruit.Core.Types

-- type Precedence = Natural

-- data Associativity
--   = NonAssoc
--   | LeftAssoc
--   | RightAssoc
--   | FullAssoc
--   deriving (Eq, Ord, Show, Enum, Bounded)

-- -- | https://en.wikipedia.org/wiki/Operator_associativity
-- assoc :: Fun -> Associativity
-- assoc = \case
--   Plus -> FullAssoc
--   Minus -> LeftAssoc
--   Times -> FullAssoc
--   Div -> LeftAssoc
--   Pow -> RightAssoc

-- isFullyAssociative :: Fun -> Bool
-- isFullyAssociative = (== FullAssoc) . assoc

-- isLeftAssociative :: Fun -> Bool
-- isLeftAssociative = (== LeftAssoc) . assoc

-- isRightAssociative :: Fun -> Bool
-- isRightAssociative = (== RightAssoc) . assoc

-- associatesLeft :: Fun -> Bool
-- associatesLeft = liftA2 (||) isLeftAssociative isFullyAssociative

-- associatesRight :: Fun -> Bool
-- associatesRight = liftA2 (||) isRightAssociative isFullyAssociative

-- prec :: Fun -> Precedence
-- prec = \case
--   Plus -> 1
--   Minus -> 1
--   Times -> 2
--   Div -> 2
--   Pow -> 3
