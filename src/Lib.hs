{- Normalized polish expression [Wong and Liu, 1986]

# Problem

The original problem received a list of triplets:
  (A_i, r_i, s_i)  where A_i is the area
                         r_i and s_i are the limits of the aspect ratio

  Some modules are allowed to rotate 90 degrees.
  O_1 = fixed orientation modules
  O_2 = free orientation modules

  (1) w_i * h_i = A_i
  (2) r_i ≤ h_i/w_i ≤ s_i                                     if i ∋ O_1
  (3) r_i ≤ h_i/w_i ≤ s_i   or     1/r_i ≤ h_i/w_i ≤ s_i      if i ∋ O_2

Objective: minimize global bounding box subject to aspect ratio constraints
           minimize total wirelength between blocks
           a*area(F) + (1 - a)*L(F)     0 <= a <= 1

# Slicing floorplang, Slicing tree representation and Polish Expression

Polish expression: sequence of elements from {1,2,...,n,*,+} with the balloting property.
                   The traverse of a slicing tree in postorder gives a (proper) polish expression.

(i) Each block appears exactly once in the string
(ii) Balloting property: forall positions in the string #operators < #operand
(iii) no consecutive operator of the same type: normality property.

Normalized: no consecutive operators of the same type (H or V)
            1-1 correspondence between the set of normalized polished expressions of length 2n-1 and the set of
            skewed slicing trees with n leaves.

LIFO structure to convert polish expression to binary tree.

Moves:
 * M1: swap two adjacent operands
 * M2: complementing (swap H and V) some chain of operators
 * M3: swap a pair of adjacent operands and operators
! In  case of M3, we need to validate properties (ii) and (iii)

# Simulated Annealing


- Current = Best = 12*3*4*..*n*
- Cooling ratio = r = 0.85
- ∆_avg = perform random moves and compute the average value fo the magnitude of change in cost per move.
- T_0: we should have exp(-∆_avg/T_0) = P ~ 1, so T_0 = -∆_avg/ln(P)
- T_final = T_0 * 0.2 (~ 10 iterations, with r=0.85)
- T = T_0
- N = n*\gamma where \gamma is a user defined constant

repeat
     repeat
           New = Best + randomly select move // M3 requires trying several times.
           #moves <- #moves + 1
           ∆cost <- Cost(new) - Cost(Z)
           if (∆cost \leq 0) or (random(0,1) < e^{ -∆cost / T }) // Boltzmann acceptance criterion, where r is a random number [0, 1)
                if(∆cost < 0) then downhill <- downhill + 1
                Current = New
                if cost(Current) < cost(Best) then Best <- Current
           else
               reject <- reject + 1
     until (downhill >= N) or (#moves > 2N)
     T <- r*T
     #moves = 0
until (reject / #moves > 0.95) or (T <= T_final) or OutOfTime
return Best

# Our problem

To simplify our problem, we made some assumptions:
 * The blocks are rectangular and the shapes are given, at least one per module.
 * At most one connection between modules.

Problem:
  * adjacency list = [[3,4,5], [1,3,4,5,6], ...]
  * shape list = [[(3,5),(4,4)], [(2,6),(3,3)], ...]

-}

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
module Lib where

import PolishExpression
import Data.Function (on)
import Data.Maybe(isJust, fromJust)
import qualified Data.IntMap.Strict as IMap
import Control.Monad.State.Lazy
import Control.Arrow((&&&))
import qualified Data.List as List
import qualified System.Random.MWC as Random

-----------------------------------------------------------------------------------

{-

FIXME dynamic programming: store the area of each node and only update the ones affected by the move.
FIXME wirelength into account

Nice to have:
TODO semigroup with a phantom type ?
TODO data PEState = Normalized | Unnormalized
     newtype PolishExpression (n :: PEState) = PolishExpression { _pe :: [Alphabet] }
-}

newtype Temperature = Temperature Double deriving newtype (Show, Eq, Ord, Num, Fractional)

newtype Height = Height { _height :: Int } deriving newtype (Show, Eq, Ord, Num)
newtype Width = Width { _width :: Int } deriving newtype (Show, Eq, Ord, Num)
newtype Area = Area { _area :: Double }  deriving newtype (Show, Eq, Ord, Num, Fractional)
newtype AspectRatio = AspectRatio { _aspectRatio :: Double } deriving newtype (Show, Eq, Ord, Num, Fractional)
newtype Score = Score { _score :: Double } deriving newtype (Show, Eq, Ord, Num)

newtype Shape = Shape (Width, Height) deriving stock (Show, Eq, Ord)
newtype Point = Point (Width, Height) deriving stock (Show, Eq, Ord)

newtype Problem = Problem
  { _problem :: IMap.IntMap ([Shape], [ModuleIndex])
  }

data SlicingTree where
  Leaf :: ModuleIndex -> SlicingTree
  Node :: SlicingTree -> Operator -> SlicingTree -> SlicingTree


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
simulatedAnnealing :: Problem -> Double -> Double -> IO SlicingTree
simulatedAnnealing problem r gamma = do
  seed <- Random.createSystemRandom
  let n = problemSize problem
      initial = fromJust $ initialPE n -- FIXME
      moveIncrAvg = computeMoveIncrAvg seed initial
  return undefined   -- TODO

-- | Given an initial polish expression, apply a sequence of n random moves and
-- compute the average of the magnitude of increment of cost at each move.
-- TODO
computeMoveIncrAvg :: Random.GenIO -> Problem -> PolishExpression -> Double
computeMoveIncrAvg = undefined

-- | Double generated u.a.r from the range [0,1]
rand :: Random.GenIO -> IO Double
rand = Random.uniformRM (0.0, 1.0)

problemSize :: Problem -> Int
problemSize = length . Map.keys . _problem

area :: Shape -> Area
area (Shape (Width w, Height h)) = Area (fromIntegral (w*h))

aspectRatio :: Shape -> AspectRatio
aspectRatio (Shape (Width w, Height h)) = AspectRatio (fromIntegral h / fromIntegral w)

-- | Computes the best solution w.r.t. the cost function using the algorithm described in [Wong and Liu]
--
-- It takes into account both Area and Wirelength
computeBestSolution :: Problem -> SlicingTree -> (Shape, Score, [(ModuleIndex, Shape)])
computeBestSolution Problem slicingTree =
  let permutations = evalState (shapeCurves slicingTree) Problem
      allShapes = fmap fst permutations
      (best, score) = List.foldl' bestShape (undefined, 0 :: Score) allShapes
      (_, info) = fromJust $ List.find ((==) best . fst) permutations
  in (best, score, info)

  where
    bestShape :: (Shape, Score) -> Shape -> (Shape, Score)
    bestShape old@(_, oldScore) shape =
            let newScore = objectiveFunction (area shape)
                ratio = let r = aspectRatio shape
                         in if r < 1 then 1/r else r
            -- TODO aspect ratio as Problem
             in if (ratio > 2) then old
                else if oldScore >= newScore then old
                     else (shape, newScore)

{- TODO Wirelength is approximated

The paper explores all wirelengths of all shape curves using a clever technique.
I will simplify this by only computing W for the root's shapes computed for the areas.

1. First you need to compute the center of each module.

        Starting from the root shape (l_x, l_y) = (0,0) and (x,y) = (x_total, y_total)
        When a node H/+ is found, we will compute (l_x, l_y) and (x,y) for the right and left node and pass it down.
        When a node V/* is found, same.
        When a leaf is found, we will receive a (l_x, l_y) and (x,y) and we need to compute the (c_x, c_y).

2. Sum (forall modules):
      Compute the eucliean distance to the connected modules
        each distance is multiplied by the #wires between modules (1 in our case)
-}


-- TODO the score should be a linear combination of the area and the wire length
objectiveFunction :: Area -> Score
objectiveFunction (Area area') = Score area'

-- | These are the shapes curves as in
shapeCurves :: SlicingTree -> State Problem [(Shape, [(ModuleIndex, Shape)])]
shapeCurves (Leaf i) = fmap (\s -> (s, [(i, s)])) <$> getShapes i
shapeCurves (Node left op right) = do
  ll <- shapeCurves left
  rr <- shapeCurves right
  return $ do (l, lInfo) <- ll
              (r, rInfo) <- rr
              let combinedShape = combineBlocks op l r
              return (combinedShape, (lInfo ++ rInfo))

getShapes :: (Monad m, MonadState Problem m) => ModuleIndex -> m [Shape]
getShapes i = gets (\(Problem s) -> fst $ s IMap.! i)

combineBlocks :: Operator -> Shape -> Shape -> Shape
combineBlocks H (Shape (lw, lh)) (Shape (rw, rh)) = Shape (max lw rw, lh + rh)
combineBlocks V (Shape (lw, lh)) (Shape (rw, rh)) = Shape (lw + rw, max lh rh)

-- | Validates and transforms a polish expression to its corresponding slicing floorplan
-- toSlicingFP :: Problem -> PolishExpression -> Either String SlicingTree
-- toSlicingFP (validate -> Right polishExpression) = undefined
-- toSlicingFP (validate -> err) = err

