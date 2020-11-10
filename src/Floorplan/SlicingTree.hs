{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Floorplan.SlicingTree
  ( SlicingTree (..),
    toSlicingTree,
    toPolishExpression,
  )
where

import Floorplan.PolishExpression

data SlicingTree where
  Leaf :: ModuleIndex -> SlicingTree
  Branch :: SlicingTree -> Operator -> SlicingTree -> SlicingTree
  deriving stock (Show, Eq)

-- | There is a 1-1 correspondence between normalized polish expressions and skewed slicing tress.
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

toPolishExpression :: SlicingTree -> PolishExpression
toPolishExpression tree = PolishExpression (go tree)
  where
    go (Leaf moduleIndex) = [Operand moduleIndex]
    go (Branch l op r) = go l ++ go r ++ [Operator op]
