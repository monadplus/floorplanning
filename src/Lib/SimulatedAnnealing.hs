{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib.SimulatedAnnealing
  ( module Lib.SimulatedAnnealing
  , module Control.Monad.State.Lazy
  ) where

-----------------------------------------------------------------------------------

import Data.Function (on)
import Data.Maybe(isJust, fromJust)
import qualified Data.IntMap.Strict as IMap
import Control.Monad.State.Lazy
import Control.Arrow((&&&))
import qualified Data.List as List
import qualified System.Random.MWC as Random
import Debug.Trace

import Lib.PolishExpression

-- -----------------------------------------------------------------------------------

-- {-

-- FIXME dynamic programming: store the area of each Branch and only update the ones affected by the move.
-- FIXME wirelength into account

-- Nice to have:
-- TODO semigroup with a phantom type ?
-- TODO data PEState = Normalized | Unnormalized
--      newtype PolishExpression (n :: PEState) = PolishExpression { _pe :: [Alphabet] }
-- -}

newtype Temperature = Temperature Double deriving newtype (Show, Eq, Ord, Num, Fractional)

newtype Height = Height { _height :: Int } deriving newtype (Show, Eq, Ord, Num)
newtype Width = Width { _width :: Int } deriving newtype (Show, Eq, Ord, Num)
newtype Shape = Shape (Width, Height) deriving stock (Show, Eq, Ord)
pattern Shape' :: Int -> Int -> Shape
pattern Shape' w h = Shape ((Width w),(Height h))

newtype Area = Area { _area :: Double }  deriving newtype (Show, Eq, Ord, Num, Fractional)
newtype AspectRatio = AspectRatio { _aspectRatio :: Double } deriving newtype (Show, Eq, Ord, Num, Fractional)

newtype Score = Score { _score :: Double } deriving newtype (Show, Eq, Ord, Num)

data Coordinate = Coordinate { _x :: Int, _y :: Int }
  deriving stock (Show, Eq, Ord)

newtype Problem = Problem
  { _problem :: IMap.IntMap ([Shape], [ModuleIndex])
  }

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
      go ((Operand i):xs) = (Leaf i, xs)
      go ((Operator op):xs) =
        let (r, xs')  = go xs
            (l, xs'') = go xs'
          in (Branch l op r, xs'')

{-
- Current = Best = 12*3*4*..*n*
- Cooling ratio = r = 0.85
- ∆_avg = perform random moves and compute the average value fo the magnitude of change in cost per move.
- T_0: we should have exp(-∆_avg/T_0) = P ~ 1, so T_0 = -∆_avg/ln(P)
- T_final = T_0 * 0.2 (~ 10 iterations, with r=0.85)
- T = T_0
- N = n*\gamma where \gamma is a user defined constant

repeat
     repeat
           New = current + randomly select move // M3 requires trying several times.
           #moves <- #moves + 1
           ∆cost <- Cost(new) - Cost(Z)
           if (∆cost \leq 0) or (random(0,1) < e^{ -∆cost / T }) // Boltzmann acceptance criterion, where r is a random number [0, 1)
                if(∆cost < 0) then downhill <- downhill + 1
                Current = New
                if cost(Current) < cost(Best) then Best <- Current
           else
               reject <- reject + 1
     until (downhill == N) or (#moves == 2N)
     T <- r*T
     #moves = 0
until (reject / #moves > 0.95) or (T <= T_final) or OutOfTime
return Best
-}

data Variables = Variables
  { _best :: PolishExpression
  , _current :: PolishExpression
  , _currentTmp :: Temperature
  , _finalTmp :: Temperature
  , _gen :: Random.GenIO
  , _moves :: Int
  , _downhill :: Int
  , _reject :: Int
  , _N :: Int
  }

-- Call with Problem, r=0.85, gamma=2
-- The Problem should have been previously validated i.e. numbers from 1,2,...,n
-- simulatedAnnealing :: Problem -> Double -> Double -> IO SlicingTree
-- simulatedAnnealing problem r gamma = do
--   gen <- Random.createSystemRandom
--   let n = problemSize problem
--       initial = fromJust $ initialPE n -- FIXME
--       moveIncrAvg = computeMoveIncrAvg gen initial
--   return undefined   -- TODO

-- | Given an initial polish expression, apply a sequence of n random moves and
-- compute the average of the magnitude of increment of cost at each move.
-- TODO
computeMoveIncrAvg :: Random.GenIO -> Problem -> PolishExpression -> Double
computeMoveIncrAvg = undefined

problemSize :: Problem -> Int
problemSize = length . IMap.keys . _problem

area :: Shape -> Area
area (Shape (Width w, Height h)) = Area (fromIntegral (w*h))

aspectRatio :: Shape -> AspectRatio
aspectRatio (Shape (Width w, Height h)) = AspectRatio (fromIntegral h / fromIntegral w)

-- | Computes the best solution w.r.t. the cost function using the algorithm described in [Wong and Liu]
--
-- It takes into account both Area and Wirelength
-- computeBestSolution :: Problem -> SlicingTree -> (Shape, Score, [(ModuleIndex, Shape)])
-- computeBestSolution problem slicingTree =
--   let permutations = evalState (shapeCurves slicingTree) problem
--       allShapes = fmap fst permutations
--       (best, score) = List.foldl' bestShape (undefined, 0 :: Score) allShapes
--       (_, info) = fromJust $ List.find ((==) best . fst) permutations
--   in (best, score, info)

--   where
--     bestShape :: (Shape, Score) -> Shape -> (Shape, Score)
--     bestShape old@(_, oldScore) shape =
--             let newScore = objectiveFunction (area shape)
--                 ratio = let r = aspectRatio shape
--                          in if r < 1 then 1/r else r
--             -- TODO aspect ratio as Problem
--              in if (ratio > 2) then old
--                 else if oldScore >= newScore then old
--                      else (shape, newScore)

{- TODO Wirelength is approximated

The paper explores all wirelengths of all shape curves using a clever technique.
I will simplify this by only computing W for the root's shapes computed for the areas.

1. First you need to compute the center of each module.

        Starting from the root shape (l_x, l_y) = (0,0) and (x,y) = (x_total, y_total)
        When a Branch H/+ is found, we will compute (l_x, l_y) and (x,y) for the right and left Branch and pass it down.
        When a Branch V/* is found, same.
        When a leaf is found, we will receive a (l_x, l_y) and (x,y) and we need to compute the (c_x, c_y).

2. Sum (forall modules):
      Compute the eucliean distance to the connected modules
        each distance is multiplied by the #wires between modules (1 in our case)
-}


-- TODO the score should be a linear combination of the area and the wire length
objectiveFunction :: Area -> Score
objectiveFunction (Area area') = Score area'

-----------------------------------------------------------------------------------
-- Shape Curves

type ShapeCurve = (Coordinate, [(ModuleIndex, Shape)])
type ShapeCurves = [ShapeCurve]

shapeCurves
  :: (MonadState Problem m)
  => SlicingTree
  -> m ShapeCurves
shapeCurves (Leaf moduleIndex) = do
  let sort' = List.sortBy (compare `on` (_x . fst))
      toShapeCurves shape@(Shape (Width w, Height h)) =
        (Coordinate {_x = w, _y = h}, [(moduleIndex, shape)])
  sort' . (fmap toShapeCurves) <$> getShapes moduleIndex
shapeCurves (Branch left op right) = do
  l <- shapeCurves left
  r <- shapeCurves right
  return (combineShapeCurves op l r)

getShapes :: (MonadState Problem m) => ModuleIndex -> m [Shape]
getShapes i = gets (\(Problem s) -> fst $ s IMap.! i)

combineShapeCurves
  :: Operator
  -> ShapeCurves
  -> ShapeCurves
  -> ShapeCurves
combineShapeCurves op l r =
  let limit = maximum $ fmap (minimum . fmap (getXY . fst)) [l,r]
      aboveLimit = filter ((>= limit) . getXY . fst)
      lCurves = intersectAndCombine r <$> aboveLimit l
      rCurves = intersectAndCombine l <$> aboveLimit r
      removeDuplicates = List.nubBy ((==) `on` fst)
  in removeDuplicates $ combineAndSort lCurves rCurves
    where
      getXY = if op == V then _y else _x

      intersectAndCombine :: ShapeCurves -> ShapeCurve -> ShapeCurve
      intersectAndCombine curves (c@(Coordinate x1 y1), i) =
        let f = if op == V then reverse else id
            (Coordinate x2 y2, i') = intersection getXY (f curves) c
            coord = if op == V
              then Coordinate { _x = x1+x2, _y = max y1 y2}
              else Coordinate { _x = max x1 x2, _y = y1+y2}
            info = i ++ i'
        in  (coord, info)

      -- This only works if curves are sorted in 'x' increasing order for H
      -- and 'y' increasing order for V
      intersection :: (Coordinate -> Int) -> ShapeCurves -> Coordinate -> ShapeCurve
      intersection f (x:xs) c1 = go x xs where
        go previous [] = previous
        go previous (next@(c2, _):rest)
          | f c1 < f c2   = previous
          | otherwise = go next rest

      -- Sort 'x' in increasing order and 'y' in decreasing order.
      -- Note, we are taking advantge of 'f' being a decreasing function
      -- and points were already sorted at the leaves.
      combineAndSort :: ShapeCurves -> ShapeCurves -> ShapeCurves
      combineAndSort [] ys = ys
      combineAndSort xs [] = xs
      combineAndSort (x:xs) (y:ys)
        | x `less` y = x : combineAndSort xs (y:ys)
        | otherwise  = y : combineAndSort (x:xs) ys
        where
          less :: ShapeCurve -> ShapeCurve -> Bool
          less (Coordinate x1 _, _) (Coordinate x2 _, _) = x1 < x2

---------------------------------------------------------------------
-- Util

-- | Double generated u.a.r from the range [0,1]
rand :: Random.GenIO -> IO Double
rand = Random.uniformRM (0.0, 1.0)
