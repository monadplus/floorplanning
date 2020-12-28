{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Floorplan.SlicingTree
  ( Tree (..),
    SlicingTree,
    toSlicingTree,
    toPolishExpression,
  )
where

-----------------------------------------------------------

import Floorplan.PolishExpression
import Floorplan.Types

-----------------------------------------------------------

data Tree a b where
  Leaf :: a -> Tree a b
  Branch :: Tree a b -> b -> Tree a b -> Tree a b
  deriving stock (Show, Eq)

-- deriving stock instance (Show a, Show b) => Show (Tree a b)
-- deriving stock instance (Eq a, Eq b) => Eq (Tree a b)

type SlicingTree = Tree ModuleIndex Operator

-- | There is a 1-1 correspondence between normalized polish expressions and skewed slicing tress.
--
-- Undo postorder traversal
toSlicingTree :: PolishExpression -> SlicingTree
toSlicingTree =
  check . go . reverse . _pe
  where
    check :: (SlicingTree, [a]) -> SlicingTree
    check (slicingTree, []) = slicingTree
    check (_, _) = error "Check toSlicingTree."

    go :: [Alphabet] -> (SlicingTree, [Alphabet])
    go [] = error "Check toSlicingTree:go."
    go ((Operand i) : xs) = (Leaf i, xs)
    go ((Operator op) : xs) =
      let (r, xs') = go xs
          (l, xs'') = go xs'
       in (Branch l op r, xs'')
{-# INLINE toSlicingTree #-}

-- Postorder traversal
toPolishExpression :: SlicingTree -> PolishExpression
toPolishExpression tree = PolishExpression (go tree)
  where
    go (Leaf moduleIndex) = [Operand moduleIndex]
    go (Branch l op r) = go l ++ go r ++ [Operator op]
{-# INLINE toPolishExpression #-}
