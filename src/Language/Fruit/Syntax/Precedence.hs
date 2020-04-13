{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Syntax.Precedence
  ( Precedence,
    prec,
    Associativity (..),
    assoc,
    isFullyAssociative,
    isLeftAssociative,
    isRightAssociative,
    associatesLeft,
    associatesRight,
  )
where

import qualified Language.Fruit.Syntax.AST as AST

type Precedence = Natural

data Associativity
  = NonAssoc
  | LeftAssoc
  | RightAssoc
  | FullAssoc
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | https://en.wikipedia.org/wiki/Operator_associativity
assoc :: AST.Fun -> Associativity
assoc = \case
  AST.Plus -> FullAssoc
  AST.Minus -> LeftAssoc
  AST.Times -> FullAssoc
  AST.Div -> LeftAssoc
  AST.Pow -> RightAssoc

isFullyAssociative :: AST.Fun -> Bool
isFullyAssociative = (== FullAssoc) . assoc

isLeftAssociative :: AST.Fun -> Bool
isLeftAssociative = (== LeftAssoc) . assoc

isRightAssociative :: AST.Fun -> Bool
isRightAssociative = (== RightAssoc) . assoc

associatesLeft :: AST.Fun -> Bool
associatesLeft = liftA2 (||) isLeftAssociative isFullyAssociative

associatesRight :: AST.Fun -> Bool
associatesRight = liftA2 (||) isRightAssociative isFullyAssociative

prec :: AST.Fun -> Precedence
prec = \case
  AST.Plus -> 1
  AST.Minus -> 1
  AST.Times -> 2
  AST.Div -> 2
  AST.Pow -> 3
