{- Normalized polish expression [Wong and Liu, 1986]

#### Problem

Input: adjacency list = [[3,4,5], [1,3,4,5,6], ...]
       shape list = [[(3,5),(4,4)], [(2,6),(3,3)], ...]

Rectangular modules/blocks

The original problem received a list of triplets:
  (A_i, r_i, s_i)  where A_i is the area
                         r_i and s_i are the limits of the aspect ratio

  Some modules are allowed to rotate 90 degrees.
  O_1 = fixed orientation modules
  O_2 = free orientation modules

  (1) w_i * h_i = A_i
  (2) r_i ≤ h_i/w_i ≤ s_i                                     if i ∋ O_1
  (3) r_i ≤ h_i/w_i ≤ s_i   or     1/r_i ≤ h_i/w_i ≤ s_i      if i ∋ O_2

No overlapps
Are rotations allowed ? guess no

Objective: minimize global bounding box subject to aspect ratio constraints
           minimize total wirelength between blocks (?)
           a*area(F) + (1 - a)*L(F)     0 <= a <= 1

Representation: Slicing floorplan by polish notation

Cost evaluation:
 * packing: based on horizontal/vertical constriants
 * block sizing
 * wirelength estimation

#### Polish Expression

Polish expression: sequence of elements from {1,2,...,n,*,+} with the balloting property.
                   The traverse of a slicing tree in postorder gives a (proper) polish expression.

(i) Each block appears exactly once in the string
(ii) Balloting property: forall positions in the string #operators < #operand
(iii) no consecutive operator of the same type: normality property.

Normalized: no consecutive operators of the same type (H or V)
            1-1 correspondence between the set of normalized polished expressions of length 2n-1 and the set of
            skewed slicing trees with n leaves.

LIFO structure to convert polish expression to binary tree.

#### Overview of the algorithm

Define Simulated Annealing parameters:
   - Initial and final temperature
   - Cooling rate
   - M_t = number of moves at each temperature

Random initial polish expression = PE_0

Compute C_0 = cost(PE_0) = A_0 + lambda * W_0

Set Z = PE_0  (solution, the one returned at the end of the annealing)

At each temperature, we make M_t moves where
  M_t = k n    (k = 5-10, n = #modules)
We randomly select one of the three types and randomly choose a pair or chain:

 * M1: swap two adjacent operands
 * M2: complementing (swap H and V) some chain of operators
 * M3: swap a pair of adjacent operands and operators

! In  case of M3, we need to validate properties (ii) and (iii)

How to validate (ii)

Next, we perform the chosen move and obtain a new polish expression.
If the cost of this new solution is lower, accept the move.
Else, we accept based on a probability function that is temperature dependent.
High probability of accepting bad moves on high temperatures.
Low probability of accepting bad moves on low temperatures.

Update Z.
After making all the moves at current temperature, reduce temperature using the cooling rate r < 1
and repeat.

When the temperature reaches the final temperature, or the number of moves accepted is sufficiently low,
  stop the process and return Z.

#### Algorithm for area

Given a slicing floorplan representend as a BT.
In order to get the width and heigh, traverse the tree in the following order:
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

import Data.Function (on)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Maybe(isJust, fromJust)
import qualified Data.IntMap.Strict as IMap
import Control.Monad.State.Lazy
import Control.Arrow((&&&))
import qualified Data.List as List

-----------------------------------------------------------------------------------

-- TODO don't repeat permutations of the polish expression!
-- TODO wirelength into account
-- TODO explore shapes in a more efficient way

-- TODO dynamic programming: store the area of each node and only update the ones affected by the move.
-- TODO semigroup with a phantom type ?
-- TODO data PEState = Normalized | Unnormalized
--      newtype PE (n :: PEState) = PE { unPE :: Vector PElem }

{-
Implemented:
* A way to compute the best shape of a slicing floorplan
* A way to validate if a PE is valid

Missing:
* From a P.E. to a slicing floorplan
* Randomly generate input like in the paper
* Parse input from file
* Moves, randomly do one and validate the resulting P.E.
-}

newtype Height = Height { _height :: Int } deriving newtype (Show, Eq, Ord, Num)
newtype Width = Width { _width :: Int } deriving newtype (Show, Eq, Ord, Num)
newtype Area = Area { _area :: Double }  deriving newtype (Show, Eq, Ord, Num, Fractional)
newtype AspectRatio = AspectRatio { _aspectRatio :: Double } deriving newtype (Show, Eq, Ord, Num, Fractional)
newtype Score = Score { _score :: Double } deriving newtype (Show, Eq, Ord, Num)

type ModuleIndex = Int
newtype Shape = Shape (Width, Height) deriving stock (Show, Eq, Ord)
newtype Input = Input { _input :: IMap.IntMap ([Shape], [ModuleIndex]) }

-- | H means a slice in the horizontal plane i.e. elements must be placed vertical.
--
-- V: * (original notation)
-- H: + (original notation)
data Operator = V | H deriving stock (Show, Eq)

data PElem
  = POperand !ModuleIndex
  | POperator Operator

data SlicingFP where
  Leaf :: ModuleIndex -> SlicingFP
  Node :: SlicingFP -> Operator -> SlicingFP -> SlicingFP

-- | Represents a polish expression
newtype PE = PE { unPE :: Vector PElem }

-----------------------------------------------------------------------------------

area :: Shape -> Area
area (Shape (Width w, Height h)) = Area (fromIntegral (w*h))

aspectRatio :: Shape -> AspectRatio
aspectRatio (Shape (Width w, Height h)) = AspectRatio (fromIntegral h / fromIntegral w)

-- | Normalize over the interval [0,1]
-- normalize :: (Ord a, Fractional a, Foldable t) => a -> t a -> a
-- normalize a as =
--   let minA = minimum as
--       maxA = maximum as
--   in (a - minA) / (maxA - minA)

isOperand :: PElem -> Bool
isOperand (POperand _) = True
isOperand (POperator _) = False

-- | Score of a Shape: promotes squared floorplans.
computeBestShape :: Input -> SlicingFP -> (Shape, Score, [(ModuleIndex, Shape)])
computeBestShape input slicingTree =
  let permutations = evalState (shapePermutations slicingTree) input
      allShapes = fmap fst permutations
      (areas, aspectRatios) = unzip $ fmap (area &&& aspectRatio) allShapes
      (best, score) = computeBestShape' (minimum areas, maximum areas) allShapes
      (_, info) = fromJust $ List.find ((==) best . fst) permutations
  in (best, score, info)
  where
    computeBestShape' :: (Area, Area) -> [Shape] -> (Shape, Score)
    computeBestShape' (minArea, maxArea) = List.foldl' bestShape (undefined, 0 :: Score)
      where
        bestShape :: (Shape, Score) -> Shape -> (Shape, Score)
        bestShape old@(oldShape, oldScore) shape =
          let normalize x min' max' = (x - min') / (max' - min')
              normalizedArea = normalize (area shape) minArea maxArea
              newScore = objectiveFunction normalizedArea
              ratio = let r = aspectRatio shape
                      in if r < 1 then 1/r else r
          in if (ratio > 2) -- NOTE At most, an aspect ratio of 2:1
               then old
               else if oldScore >= newScore
                       then old
                       else (shape, newScore)

        -- aspectScore :: AspectRatio -> Double
        -- aspectScore r = _aspectRatio $
        --   1.0 / (abs (1 - r))

{-
1. First you need to compute the center of each module.
   See how the paper does.
2. Sum (forall modules):
      Compute the eucliean distance to the connected modules
        each distance is multiplied by the #wires between modules (1 in our case)

computeWirelength :: (MonadState Input) => SlicingTree -> m Double
-}


-- Area must be normalized!
-- TODO the score should be a linear combination of the area and the wire length
objectiveFunction :: Area -> Score
objectiveFunction (Area area') = Score area'

-- | These are possible permutations of shapes
--
-- NOTE this may grow too much
--
-- (easy) The paper explores a better techniques based on shape functions
shapePermutations :: SlicingFP -> State Input [(Shape, [(ModuleIndex, Shape)])]
shapePermutations (Leaf i) = fmap (\s -> (s, [(i, s)])) <$> getShapes i
shapePermutations (Node left op right) = do
  ll <- shapePermutations left
  rr <- shapePermutations right
  return $ do (l, lInfo) <- ll
              (r, rInfo) <- rr
              let combinedShape = combineBlocks op l r
              return (combinedShape, (lInfo ++ rInfo))

getShapes :: (Monad m, MonadState Input m) => ModuleIndex -> m [Shape]
getShapes i = gets (\(Input s) -> fst $ s IMap.! i)

combineBlocks :: Operator -> Shape -> Shape -> Shape
combineBlocks H (Shape (lw, lh)) (Shape (rw, rh)) = Shape (max lw rw, lh + rh)
combineBlocks V (Shape (lw, lh)) (Shape (rw, rh)) = Shape (lw + rw, max lh rh)

-- | Produces the initial (proper) polish expression of the form 12*3*...*n*
--
-- 'Int' parameter is the number of modules/blocks.
initialPE :: Int -> Maybe PE
initialPE n
  | n < 3 = Nothing
  | otherwise =
      let op = (POperator V)
          operators = fmap POperand [2..]
       in Just . PE . Vector.fromList $
            (POperand 1 : take (n-2) (List.intersperse op operators)) ++ [op]

-- | Validates and transforms a polish expression to its corresponding slicing floorplan
-- toSlicingFP :: Input -> PE -> Either String SlicingFP
-- toSlicingFP (validate -> Right pe) = undefined
-- toSlicingFP (validate -> err) = err

-- | Validate a polish expression.
--
-- (i) Each block appears exactly once in the string
-- (ii) Balloting property: forall positions in the string #operands > #operators
-- (iii) no consecutive operator of the same type: normality property.
--
-- (i) is validated by construction.
validate :: PE -> Either String PE
validate pe =
  if | propII pe -> if | propIII pe -> Right pe
                       | otherwise -> Left "Property III violated."
     | otherwise -> Left "Property II violated."
  where
    -- TODO e_{i-1} /= e_{i+1} && 2N_i+1 < i
    propII :: PE -> Bool
    propII (PE v)= isJust $ foldM (\ acc e -> if isOperand e
                                        then Just (acc + 1)
                                        else if acc - 1 > 0
                                               then Just (acc - 1)
                                               else Nothing) (0 :: Int) v
    propIII :: PE -> Bool
    propIII =
      let operators = Vector.toList . Vector.mapMaybe onlyOperators
          maxConsecutives = List.maximum . fmap length . List.group
       in (< 2) . maxConsecutives . operators . unPE
      where
        onlyOperators :: PElem -> Maybe Operator
        onlyOperators (POperator op) = Just op
        onlyOperators (POperand _) = Nothing
