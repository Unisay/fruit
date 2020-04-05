{-# LANGUAGE LambdaCase #-}

module Language.Frut.Syntax.Precedence
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

import qualified Language.Frut.Syntax.AST as AST

type Precedence = Natural

data Associativity
  = NonAssoc
  | LeftAssoc
  | RightAssoc
  | FullAssoc
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | https://en.wikipedia.org/wiki/Operator_associativity
assoc :: AST.Operator -> Associativity
assoc = \case
  AST.OperatorPlus -> FullAssoc
  AST.OperatorMinus -> LeftAssoc
  AST.OperatorTimes -> FullAssoc
  AST.OperatorDiv -> LeftAssoc
  AST.OperatorPow -> RightAssoc

isFullyAssociative :: AST.Operator -> Bool
isFullyAssociative = (== FullAssoc) . assoc

isLeftAssociative :: AST.Operator -> Bool
isLeftAssociative = (== LeftAssoc) . assoc

isRightAssociative :: AST.Operator -> Bool
isRightAssociative = (== RightAssoc) . assoc

associatesLeft :: AST.Operator -> Bool
associatesLeft = liftA2 (||) isLeftAssociative isFullyAssociative

associatesRight :: AST.Operator -> Bool
associatesRight = liftA2 (||) isRightAssociative isFullyAssociative

prec :: AST.Operator -> Precedence
prec = \case
  AST.OperatorPlus -> 1
  AST.OperatorMinus -> 1
  AST.OperatorTimes -> 2
  AST.OperatorDiv -> 2
  AST.OperatorPow -> 3
