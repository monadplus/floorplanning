{-

* TODO dynamic programming: store the area of each Branch and only update the ones affected by the mov

Nice to have:

* TODO semigroup with a phantom type ?
* TODO data PEState = Normalized | Unnormalized
       newtype PolishExpression (n :: PEState) = PolishExpression { _pe :: [Alphabet] }
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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.SimulatedAnnealing
  ( module Lib.SimulatedAnnealing
  , module Control.Monad.State.Lazy
  ) where

-----------------------------------------------------------------------------------

import Data.Function (on)
import qualified Data.IntMap.Strict as Map
import Data.IntMap.Strict (IntMap, (!))
import Control.Monad.State.Lazy
import qualified Data.List as List
import qualified System.Random.MWC as Random
import Data.Coerce
import Lib.PolishExpression
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

-----------------------------------------------------------------------------------

newtype Temperature = Temperature Double deriving newtype (Show, Eq, Ord, Num, Fractional)

newtype Height = Height { _height :: Int } deriving newtype (Show, Eq, Ord, Num)
newtype Width = Width { _width :: Int } deriving newtype (Show, Eq, Ord, Num)
newtype Shape = Shape (Width, Height) deriving stock (Show, Eq, Ord)
pattern Shape' :: Int -> Int -> Shape
pattern Shape' w h = Shape ((Width w),(Height h))

newtype WireLength = WireLength { _wireLength :: Double }  deriving newtype (Show, Eq, Ord, Num, Fractional)
newtype AspectRatio = AspectRatio { _aspectRatio :: Double } deriving newtype (Show, Eq, Ord, Num, Fractional)

newtype Cost = Cost { _cost :: Double } deriving newtype (Show, Eq, Ord, Num, Floating, Fractional)

-- | Lambda of the cost function: A(alpha) + lambda*W(alpha)
-- 0 <= Lambda <= 1
newtype Lambda = Lambda { _lambda :: Double } deriving newtype (Show, Eq, Ord, Num, Fractional)
-- | Gamma as in section 2.5 Annealing Schedule
newtype Gamma = Gamma { _gamma :: Int } deriving newtype (Show, Eq, Ord, Num)
-- | Cooling rate as in section 2.5 Annealing Schedule
newtype CoolingRate = CoolingRate { _coolingRate :: Double } deriving newtype (Show, Eq, Ord, Num, Fractional)

data Coordinate = Coordinate { _x :: Double, _y :: Double }
  deriving stock (Show, Eq, Ord)

-- Hide constructor
newtype Problem = Problem
  { _problem :: IntMap ([Shape], [ModuleIndex])
  }

{-
* Modules numbered from 1 to n
* One shape per module
* Modules connected to existent modules.
-}
validateProblem :: IntMap ([Shape], [ModuleIndex]) -> Either String Problem
validateProblem problem = do
  let modules = Map.keys problem
      n = List.length modules
      (listOfShapes, listOfConnections) = unzip (Map.elems problem)
  unless (maximum modules == n) $ Left "Modules numbered from 1 to n."
  when (any null listOfShapes) $ Left "At least one shape per module."
  forM_ listOfConnections $ \connections -> do
   when (any (> n) connections) $ Left "Module does not exist."
   unless (length (List.nub connections) == List.length connections) $ Left "Connection repeated."
  return $ Problem problem

defaultCoolingRate :: CoolingRate
defaultCoolingRate = 0.85

-- TODO is 2 ok ?
defaultGamma :: Gamma
defaultGamma = 2

data Variables = Variables
  { _best :: PolishExpression
  , _bestCost :: Cost
  , _current :: PolishExpression
  , _currentCost :: Cost
  , _currentTmp :: Temperature
  , _finalTmp :: Temperature
  , _gen :: Random.GenIO
  , _moves :: Int
  , _downhill :: Int
  , _rejects :: Int
  , _bigN :: Int
  }
makeLenses ''Variables

-- TODO  Add time limit.
-- | Applies an annealing schedule to approximate the best solution
simulatedAnnealing :: (MonadIO m) => Problem -> Lambda -> CoolingRate -> Gamma -> m SlicingTree
simulatedAnnealing problem lambda r gamma = do
  let n = problemSize problem
  initial <- case initialPE n of
               Nothing -> liftIO $ fail "Too small problem"
               Just pe -> return pe
  initialCost <- runComputeCost initial
  _gen <- liftIO $ Random.createSystemRandom
  moveIncrAvg <- computeMoveIncrAvg _gen problem initial
  let p = (0.98 :: Double) -- Probability of acceptance at high-temperature.
      _currentTmp = coerce $ (- moveIncrAvg) / log p
      _finalTmp = coerce $ (r^(10 :: Int))*(coerce _currentTmp)
      _bigN = n*(coerce gamma)
      _current = initial
      _currentCost = initialCost
      _best = initial
      _bestCost = initialCost
      _moves = 0
      _downhill = 0
      _rejects = 0
  runAnnealing Variables{..}
    where
      runComputeCost :: (MonadIO m) => PolishExpression -> m Cost
      runComputeCost pe = evalStateT (computeCost pe lambda) problem

      runAnnealing :: (MonadIO m) => Variables -> m SlicingTree
      runAnnealing variables = do
        Variables{..} <- execStateT outerLoop variables
        return $ toSlicingTree _best

      outerLoop :: (MonadState Variables m, MonadIO m) => m ()
      outerLoop = do
        innerLoop
        currentTmp *= (coerce r)
        stop <- outerStopCondition
        unless stop $ do
          moves .= 0
          rejects .= 0
          downhill .= 0
          outerLoop

      -- The reject rate is too high because
      --   * the temperature is too low and/or
      --   * the solution is too good
      outerStopCondition :: (MonadState Variables m) => m Bool
      outerStopCondition = do
        Variables{..} <- get
        let rejectRate = (fromIntegral _rejects / fromIntegral _moves) :: Double
        return (rejectRate > 0.95 || _currentTmp < _finalTmp)

      -- Be very careful not to use this values after being updated!
      innerLoop :: (MonadState Variables m, MonadIO m) => m ()
      innerLoop = do
        (Variables {..}) <- get
        newPE <- perturbate _gen _current
        newCost <- runComputeCost newPE
        moves += 1
        let incrCost = newCost - _currentCost
        randomValue <- rand _gen
        if incrCost < 0 || coerce randomValue < exp (- incrCost / (coerce _currentTmp))
          then do
            when (incrCost < 0) $ downhill += 1
            current .= newPE
            currentCost .= newCost
            when (newCost < _bestCost) $ do
              best .= newPE
              bestCost .= newCost
          else rejects += 1
        stop <- innerStopCondition
        unless stop $ do
          innerLoop

      innerStopCondition :: (MonadState Variables m) => m Bool
      innerStopCondition = do
        Variables{..} <- get
        return (_downhill >= _bigN || _moves >= 2*_bigN)


-- TODO
-- | Given an initial polish expression, apply a sequence of n random moves and
-- compute the average of the magnitude of increment of cost at each move.
computeMoveIncrAvg :: (MonadIO m) => Random.GenIO -> Problem -> PolishExpression -> m Double
computeMoveIncrAvg = undefined

------------------------------------------------------------------------------
-- Polish Expression Cost

-- | Computes the cost of the best solution of a polish expression
computeCost
  :: (MonadState Problem m)
  => PolishExpression
  -> Lambda
  -> m Cost
computeCost pe lambda = do
  let slicingTree = toSlicingTree pe
  shapeCurves <- getShapeCurves slicingTree
  scores <- traverse (computeCost' slicingTree) shapeCurves
  return $ minimum scores
  where
    computeCost' :: (MonadState Problem m) => SlicingTree -> ShapeCurve -> m Cost
    computeCost' slicingTree (Coordinate a b, info) = do
      let moduleShapes = Map.fromList info
          area = a*b
      wirelength <- totalWireLength moduleShapes slicingTree
      return . coerce $ area + (coerce lambda)*(coerce wirelength)

------------------------------------------------------------------------------------
-- Wirelength

{- Wirelength is an approximation

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

Slicing tree to slicing floorplan:

                +---+
+---+   +---+   | B |
| A | + | B | = +---+
+---+   +---+   | A |
                +---+
+---+   +---+   +---+---+
| A | * | B | = | A | B |
+---+   +---+   +---+---+

Manhatten distance.

-}

data BoundingBox = BoundingBox { _bottomLeft :: Coordinate, _topRigth :: Coordinate }
  deriving stock (Show, Eq)
pattern BoundingBox' :: Double -> Double -> Double -> Double -> BoundingBox
pattern BoundingBox' x_bl y_bl x_tr y_tr = BoundingBox (Coordinate x_bl y_bl) (Coordinate x_tr y_tr)

computeCenter :: BoundingBox -> Coordinate
computeCenter (BoundingBox' x_bl y_bl x_tr y_tr) = Coordinate ((x_bl + x_tr)/2) ((y_bl + y_tr)/2)

totalWireLength
  :: (MonadState Problem m)
  => IntMap Shape -- For each module, the chosen shape (this comes from the curveShapes associated info)
  -> SlicingTree
  -> m WireLength
totalWireLength moduleShapes slicingTree = do
  let boundingBoxes = getBoundingBoxes moduleShapes slicingTree
      centers = computeCenter <$> boundingBoxes
  Problem problem <- get
  return $ Map.foldlWithKey' (go centers) (0 :: WireLength) problem
    where
      go :: IntMap Coordinate -> WireLength -> ModuleIndex -> ([Shape], [ModuleIndex]) -> WireLength
      go centers !acc moduleIndex (_, connections) =
        let getCenter i = centers ! i
            c1 = getCenter moduleIndex
            -- Avoid double counting distances
            f acc moduleIndex2
              | moduleIndex2 > moduleIndex = (+ acc) . coerce . manhattanDistance c1 . getCenter $ moduleIndex2
              | otherwise = acc
         in acc + List.foldl' f (0 :: WireLength) connections

-- | Returns the bounding box coordinates of each module.
getBoundingBoxes :: IntMap Shape -> SlicingTree -> IntMap BoundingBox
getBoundingBoxes moduleShapes = Map.fromList . snd  . go (Coordinate 0 0)
  where
    getShape moduleIndex = moduleShapes ! moduleIndex
    getTopRight (Coordinate x y) (Shape' w h) = Coordinate (x + fromIntegral w) (y + fromIntegral h)
   
    go :: Coordinate -> SlicingTree -> (BoundingBox, [(ModuleIndex, BoundingBox)])
    go bottomLeft (Leaf moduleIndex) =
      let topRight = getTopRight bottomLeft (getShape moduleIndex)
          boundingBox = BoundingBox bottomLeft topRight
       in (boundingBox, [(moduleIndex, boundingBox)])
    go bottomLeft (Branch l V r) =
      let (BoundingBox bl@(Coordinate _ y1) (Coordinate x2 _), info1) = go bottomLeft l
          (BoundingBox _ tr, info2) = go (Coordinate x2 y1) r
       in (BoundingBox bl tr, info1 ++ info2)
    go bottomLeft (Branch l H r) =
      let (BoundingBox bl@(Coordinate x1 _) (Coordinate _ y2), info1) = go bottomLeft l
          (BoundingBox _ tr, info2) = go (Coordinate x1 y2) r
       in (BoundingBox bl tr, info1 ++ info2)


-----------------------------------------------------------------------------------
-- Shape Curves

--             shape of the curve
type ShapeCurve = (Coordinate, [(ModuleIndex, Shape)])
type ShapeCurves = [ShapeCurve]

getShapeCurves
  :: (MonadState Problem m)
  => SlicingTree
  -> m ShapeCurves
getShapeCurves (Leaf moduleIndex) = do
  let sort' = List.sortBy (compare `on` (_x . fst))
      toShapeCurves shape@(Shape (Width w, Height h)) =
        (Coordinate {_x = fromIntegral w, _y = fromIntegral h}, [(moduleIndex, shape)])
  sort' . (fmap toShapeCurves) <$> getShapes moduleIndex
getShapeCurves (Branch left op right) = do
  l <- getShapeCurves left
  r <- getShapeCurves right
  return (combineShapeCurves op l r)

getShapes :: (MonadState Problem m) => ModuleIndex -> m [Shape]
getShapes i = gets (\(Problem s) -> fst $ s ! i)

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
      intersection :: (Coordinate -> Double) -> ShapeCurves -> Coordinate -> ShapeCurve
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
-- Slicing Tree

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

toPolishExpression :: SlicingTree -> PolishExpression
toPolishExpression tree = PolishExpression (go tree) where
  go (Leaf moduleIndex) = [Operand moduleIndex]
  go (Branch l op r) = go l ++ go r ++ [Operator op]

---------------------------------------------------------------------
-- Util

-- | Double generated u.a.r from the range [0,1]
rand :: MonadIO m => Random.GenIO -> m Double
rand = liftIO . Random.uniformRM (0.0, 1.0)

problemSize :: Problem -> Int
problemSize = length . Map.keys . _problem

aspectRatio :: Shape -> AspectRatio
aspectRatio (Shape (Width w, Height h)) = AspectRatio (fromIntegral h / fromIntegral w)

manhattanDistance :: Coordinate -> Coordinate -> Double
manhattanDistance (Coordinate x1 y1) (Coordinate x2 y2) =
  abs (x1 - x2) + abs (y1 - y2)
