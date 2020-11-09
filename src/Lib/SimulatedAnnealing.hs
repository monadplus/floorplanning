{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Lib.SimulatedAnnealing
  ( module Lib.SimulatedAnnealing,
    module Control.Monad.State.Lazy,
  )
where

-----------------------------------------------------------------------------------

import Control.Monad.State.Lazy
import Data.Coerce
import Data.Function (on)
import Data.Generics.Product.Typed
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import Data.Kind
import qualified Data.List as List
import qualified Data.Time.Clock as Clock
import GHC.Generics
import Lens.Micro.Mtl
import Lens.Micro.TH
import Lib.PolishExpression
import Text.Printf(printf)
import qualified System.Random.MWC as Random

-----------------------------------------------------------------------------------

{-

* TODO dynamic programming: store the area of each Branch and only update the ones affected by the mov

Nice to have:

* TODO semigroup with a phantom type ?
* TODO data PEState = Normalized | Unnormalized
       newtype PolishExpression (n :: PEState) = PolishExpression { _pe :: [Alphabet] }
-}

-----------------------------------------------------------------------------------

newtype Temperature = Temperature Double deriving newtype (Show, Eq, Ord, Num, Fractional)

newtype Height = Height {_height :: Int} deriving newtype (Show, Eq, Ord, Num)

newtype Width = Width {_width :: Int} deriving newtype (Show, Eq, Ord, Num)

newtype Shape = Shape (Width, Height) deriving stock (Show, Eq, Ord)

pattern Shape' :: Int -> Int -> Shape
pattern Shape' w h = Shape ((Width w), (Height h))

newtype WireLength = WireLength {_wireLength :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional)

newtype AspectRatio = AspectRatio {_aspectRatio :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional)

-- Hide constructor
newtype Interval a = Interval { _interval :: (a, a) }
  deriving newtype (Show, Eq)

mkInterval :: (Ord a) => (a, a) -> Maybe (Interval a)
mkInterval interval@(a, b)
  | a <= b = Just (coerce interval)
  | otherwise = Nothing

inside :: (Ord a) => Interval a -> a -> Bool
inside (Interval (a, b)) x
  | a <= x && x <= b = True
  | otherwise = False

-- | Cost > 0
newtype Cost = Cost {_cost :: Double}
  deriving newtype (Show, Eq, Ord, Num, Floating, Fractional)

-- | Lambda of the cost function: A(alpha) + lambda*W(alpha)
newtype Lambda = Lambda {_lambda :: Double} deriving newtype (Show, Eq, Ord, Num, Fractional)

-- | \[ 0 \leq \lambda \leq 1 \]
mkLambda :: Double -> Maybe Lambda
mkLambda d
  | d >= 0 && d <= 1 = Just (coerce d)
  | otherwise = Nothing

-- | Gamma as in section 2.5 Annealing Schedule
newtype Gamma = Gamma {_gamma :: Int} deriving newtype (Show, Eq, Ord, Num)

-- | Slides suggest 5-10
defaultGamma :: Gamma
defaultGamma = 5

-- | Cooling rate as in section 2.5 Annealing Schedule
newtype CoolingRate = CoolingRate {_coolingRate :: Double} -- hide constructor
  deriving newtype (Show, Eq, Ord, Num, Fractional)

-- | 0 < cooling rate < 1
mkCoolingRate :: Double -> Maybe CoolingRate
mkCoolingRate d
  | d > 0 && d < 1 = Just (coerce d)
  | otherwise = Nothing

defaultCoolingRate :: CoolingRate
defaultCoolingRate = 0.85

data Coordinate = Coordinate {_x :: Double, _y :: Double}
  deriving stock (Show, Eq, Ord)

newtype Problem = Problem -- Hide constructor
  { _problem :: IntMap ([Shape], [ModuleIndex])
  }

-- |
-- Properties of a valid problem:
--   * Modules numbered from 1 to n
--   * One shape per module
--   * Modules connected to existent modules.
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

data Variables = Variables
  { _best :: PolishExpression,
    _bestCost :: Cost,
    _current :: PolishExpression,
    _currentCost :: Cost,
    _currentTmp :: Temperature,
    _finalTmp :: Temperature,
    _gen :: Random.GenIO,
    _moves :: Int,
    _downhill :: Int,
    _rejects :: Int,
    _bigN :: Int,
    _startTime :: Clock.UTCTime
  }
  deriving stock (Generic)

makeLenses ''Variables

-- | Applies an annealing schedule to approximate the best solution
simulatedAnnealing ::
  (MonadIO m) =>
  Problem ->
  Interval AspectRatio ->
  Lambda ->
  CoolingRate ->
  Gamma ->
  m SlicingTree
simulatedAnnealing problem aspectRatio lambda r gamma = do
  let n = problemSize problem
  initial <- case initialPE n of
    Nothing -> liftIO $ fail "Too small problem"
    Just pe -> return pe
  initialCost <- evalStateT (computeCost initial DAlways lambda) problem
  _gen <- liftIO $ Random.createSystemRandom
  moveIncrAvg <- avgIncrementByMove _gen lambda problem initial
  liftIO $ putStrLn "Avg Increment finished"
  _startTime <- liftIO $ Clock.getCurrentTime
  let p = (0.98 :: Double) -- Probability of acceptance at high-temperature.
      _currentTmp = coerce $ (- moveIncrAvg) / log p -- log = ln
      _finalTmp = coerce $ (r ^ (10 :: Int)) * (coerce _currentTmp) -- 10 outer loop iterations (arbitrary)
      _bigN = n * (coerce gamma)
      _current = initial
      _currentCost = initialCost
      _best = initial
      _bestCost = initialCost
      _moves = 0
      _downhill = 0
      _rejects = 0
  runAnnealing Variables {..}
  where
    runComputeCost :: (MonadIO m) => PolishExpression -> m (Maybe Cost)
    runComputeCost pe = evalStateT (computeCost pe (DSometimes aspectRatio) lambda) problem

    runAnnealing :: (MonadIO m) => Variables -> m SlicingTree
    runAnnealing variables = do
      Variables {..} <- execStateT (outerLoop 1) variables
      return $ toSlicingTree _best

    started :: MonadIO m => Int -> m ()
    started = liftIO . printf "Iteration %d: started"

    finished :: MonadIO m => Int -> m ()
    finished = liftIO . printf "Iteration %d: finished"

    outerLoop :: (MonadState Variables m, MonadIO m) => Int -> m ()
    outerLoop !it = do
      started it
      innerLoop 1
      currentTmp *= (coerce r) -- reduce temperature
      stop <- outerStopCondition
      unless stop $ do
        moves .= 0
        rejects .= 0
        downhill .= 0
        finished it
        outerLoop (it + 1)

    -- The reject rate is too high because
    --   * the temperature is too low and/or
    --   * the solution is too good
    outerStopCondition :: (MonadState Variables m, MonadIO m) => m Bool
    outerStopCondition = do
      Variables {..} <- get
      currentTime <- liftIO Clock.getCurrentTime
      let rejectRate = (fromIntegral _rejects / fromIntegral _moves) :: Double
          timeDiff = Clock.diffUTCTime currentTime _startTime
          timeOutAfter = (300 :: Clock.NominalDiffTime) -- 5 minutes
      return (rejectRate > 0.95 || _currentTmp < _finalTmp || timeDiff > timeOutAfter)

    innerLoop :: (MonadState Variables m, MonadIO m) => Int -> m ()
    innerLoop !it = do
      started it
      Variables {..} <- get
      newPE <- perturbate _gen _current
      newCost' <- runComputeCost newPE
      moves += 1
      case newCost' of
        Nothing ->
          rejects += 1
        Just newCost -> do
          let incrCost = newCost - _currentCost
          randomValue <- rand
          let alpha = coerce $ exp (- incrCost / (coerce _currentTmp)) :: Double
          if incrCost <= 0 || randomValue < alpha
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
        finished it
        innerLoop (it + 1)

    innerStopCondition :: (MonadState Variables m) => m Bool
    innerStopCondition = do
      Variables {..} <- get
      return (_downhill >= _bigN || _moves >= 2 * _bigN)

-- | Given an initial polish expression, apply a sequence of n random moves and
-- compute the average of the magnitude of increment of cost at each move.
avgIncrementByMove
  :: MonadIO m
  => Random.GenIO
  -> Lambda
  -> Problem
  -> PolishExpression
  -> m Double
avgIncrementByMove gen lambda problem initialPE = flip evalStateT problem $ do
  let n = 50 -- #perturbations to approximate the average
      getCost pe = computeCost pe DAlways lambda
  initialCost <- getCost initialPE
  perturbationsCosts <- replicateM n (getCost =<< perturbate gen initialPE)
  return . average . fmap (coerce . abs . subtract initialCost) $ perturbationsCosts

------------------------------------------------------------------------------
-- Polish Expression Cost

-- | Computes the cost of the best solution of a polish expression
computeCost
  :: (MonadState Problem m)
  => PolishExpression
  -> DReturn r (Interval AspectRatio)
  -> Lambda
  -> m (Maybe' r Cost)
computeCost pe dReturn lambda = do
  let slicingTree = toSlicingTree pe
  case dReturn of
    DAlways -> do
      scores <- traverse (computeCost' slicingTree) =<< getShapeCurves slicingTree
      return $ minimum scores
    DSometimes aspectRatioInterval -> do
      let checkAspectRatio (Coordinate x y, _) = inside aspectRatioInterval $ AspectRatio (y / x)
      shapeCurves <- filter checkAspectRatio <$> getShapeCurves slicingTree
      case shapeCurves of
        [] -> return Nothing
        _  -> do scores <- traverse (computeCost' slicingTree) shapeCurves
                 return $ Just (minimum scores)
  where
    computeCost' :: (MonadState Problem m) => SlicingTree -> ShapeCurve -> m Cost
    computeCost' slicingTree (Coordinate a b, info) = do
      let moduleShapes = Map.fromList info
          area = a * b
      wirelength <- totalWireLength moduleShapes slicingTree
      return . coerce $ area + (coerce lambda) * (coerce wirelength)

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

data BoundingBox = BoundingBox {_bottomLeft :: Coordinate, _topRigth :: Coordinate}
  deriving stock (Show, Eq)

pattern BoundingBox' :: Double -> Double -> Double -> Double -> BoundingBox
pattern BoundingBox' x_bl y_bl x_tr y_tr = BoundingBox (Coordinate x_bl y_bl) (Coordinate x_tr y_tr)

computeCenter :: BoundingBox -> Coordinate
computeCenter (BoundingBox' x_bl y_bl x_tr y_tr) = Coordinate ((x_bl + x_tr) / 2) ((y_bl + y_tr) / 2)
computeCenter _ = undefined

totalWireLength ::
  (MonadState Problem m) =>
  IntMap Shape -> -- For each module, the chosen shape (this comes from the curveShapes associated info)
  SlicingTree ->
  m WireLength
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
getBoundingBoxes moduleShapes = Map.fromList . snd . go (Coordinate 0 0)
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

getShapeCurves ::
  (MonadState Problem m) =>
  SlicingTree ->
  m ShapeCurves
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

combineShapeCurves ::
  Operator ->
  ShapeCurves ->
  ShapeCurves ->
  ShapeCurves
combineShapeCurves op l r =
  let limit = maximum $ fmap (minimum . fmap (getXY . fst)) [l, r]
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
          coord =
            if op == V
              then Coordinate {_x = x1 + x2, _y = max y1 y2}
              else Coordinate {_x = max x1 x2, _y = y1 + y2}
          info = i ++ i'
       in (coord, info)

    -- This only works if curves are sorted in 'x' increasing order for H
    -- and 'y' increasing order for V
    intersection :: (Coordinate -> Double) -> ShapeCurves -> Coordinate -> ShapeCurve
    intersection f (x : xs) c1 = go x xs
      where
        go previous [] = previous
        go previous (next@(c2, _) : rest)
          | f c1 < f c2 = previous
          | otherwise = go next rest
    intersection _ _ _ = error "Empty shapeCurve!"

    -- Sort 'x' in increasing order and 'y' in decreasing order.
    -- Note, we are taking advantge of 'f' being a decreasing function
    -- and points were already sorted at the leaves.
    combineAndSort :: ShapeCurves -> ShapeCurves -> ShapeCurves
    combineAndSort [] ys = ys
    combineAndSort xs [] = xs
    combineAndSort (x : xs) (y : ys)
      | x `less` y = x : combineAndSort xs (y : ys)
      | otherwise = y : combineAndSort (x : xs) ys
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

---------------------------------------------------------------------

class HasGen (s :: Type) (m :: Type -> Type) where
  getGen :: m Random.GenIO

instance (MonadState s m, HasType Random.GenIO s) => HasGen s m where
  getGen = getTyped <$> get

---------------------------------------------------------------------

data Return = Always | Sometimes

type family Maybe' (r :: Return) (a :: Type) :: Type where
  Maybe' 'Always a = a
  Maybe' 'Sometimes a = Maybe a

data DReturn (r :: Return) (a :: Type) where
  DAlways :: DReturn 'Always a
  DSometimes :: a -> DReturn 'Sometimes a

deriving stock instance Functor (DReturn r)

---------------------------------------------------------------------

-- | Double generated u.a.r from the range [0,1]
rand :: forall s m. (HasGen s m, MonadIO m) => m Double
rand = liftIO . Random.uniformRM (0.0, 1.0) =<< getGen @s

problemSize :: Problem -> Int
problemSize = length . Map.keys . _problem

manhattanDistance :: Coordinate -> Coordinate -> Double
manhattanDistance (Coordinate x1 y1) (Coordinate x2 y2) =
  abs (x1 - x2) + abs (y1 - y2)

average :: (Fractional a) => [a] -> a
average [] = error "Average of empty list."
average xs =
  let (n, x) = List.foldl' go (1 :: Int, head xs) (tail xs)
   in x / fromIntegral n
  where
    go !(!n, !acc) x = (n + 1, acc + x)
