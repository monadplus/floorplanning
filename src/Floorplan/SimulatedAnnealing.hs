{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Floorplan.SimulatedAnnealing
  ( module Floorplan.SimulatedAnnealing,
    module Floorplan.Types,
    module Data.IntMap.Strict,
  )
where

-----------------------------------------------------------------------------------

import Control.Concurrent
import Control.Monad.State.Lazy
import Data.Coerce
import Data.Function (on)
import Data.Generics.Product.Typed
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import Data.Kind
import qualified Data.List as List
import qualified Data.Time.Clock as Clock
import Floorplan.PolishExpression
import qualified Floorplan.Pretty as Pretty
import Floorplan.SlicingTree
import Floorplan.Types
import GHC.Generics
import Lens.Micro.Mtl
import Lens.Micro.TH
import qualified System.Console.ANSI as Console
import qualified System.Random.MWC as Random
import Prelude hiding (print)
import qualified Prelude
import Floorplan.Problem

-----------------------------------------------------------------------------------

-- | Lambda of the cost function: \[A(\alpha) + \lambda * W(\alpha)\]
newtype Lambda = Lambda {_lambda :: Double} deriving newtype (Show, Eq, Ord, Num, Fractional)

-- | \[ 0 \leq \lambda \leq 1 \]
mkLambda :: Double -> Maybe Lambda
mkLambda d
  | d >= 0 && d <= 1 = Just (coerce d)
  | otherwise = Nothing
{-# INLINE mkLambda #-}

instance Read Lambda where
  readsPrec = readInstance (toEither "Error: expected lambda [0,1]" . mkLambda)

-- | Gamma as in section 2.5 Annealing Schedule
newtype Gamma = Gamma {_gamma :: Int}
  deriving newtype (Show, Eq, Ord, Num)

-- | 0 < gamma < 100
mkGamma :: Int -> Maybe Gamma
mkGamma x
  | 0 < x && x < 100 = Just (coerce x)
  | otherwise = Nothing
{-# INLINE mkGamma #-}

instance Read Gamma where
  readsPrec = readInstance (toEither "Error: expected gamma (0,100)" . mkGamma)

-- | Slides suggest 5-10
defaultGamma :: Gamma
defaultGamma = 5

-- | Cooling rate as in section 2.5 Annealing Schedule
newtype CoolingRate = CoolingRate {_coolingRate :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional)

-- | 0 < cooling rate < 1
mkCoolingRate :: Double -> Maybe CoolingRate
mkCoolingRate d
  | d > 0 && d < 1 = Just (coerce d)
  | otherwise = Nothing
{-# INLINE mkCoolingRate #-}

instance Read CoolingRate where
  readsPrec = readInstance (toEither "Error: expected cooling rate (0,1)" . mkCoolingRate)

defaultCoolingRate :: CoolingRate
defaultCoolingRate = 0.85

newtype Temperature = Temperature Double
  deriving newtype (Show, Eq, Ord, Num, Fractional)

data Mode = Production FilePath | Demo Int
  deriving stock (Read, Show, Eq, Ord)

data Parameters = Parameters
  { mode :: Mode
  , aspectRatio :: Interval AspectRatio
  , lambda :: Lambda
  , coolingRate :: CoolingRate
  , gamma :: Gamma
  }
  deriving stock Show

data Input = Input
  { _iProblem :: Problem,
    _iParams :: Parameters
  }
  deriving stock (Generic)

makeLenses ''Input

---------------------------------------------------------------------
-- Annealing Schedule

data Variables = Variables
  { _best :: (PolishExpression, Cost, IntMap BoundingBox),
    _current :: (PolishExpression, Cost, IntMap BoundingBox),
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

{-| The Simulated Annealing Algorithm:

* Current = Best = 12*3*4*..*n*
* Cooling ratio = r = 0.85
* ∆_avg = perform random moves and compute the average value fo the magnitude of change in cost per move.
* T_0: we should have exp(-∆_avg/T_0) = P ~ 1, so T_0 = -∆_avg/ln(P)
* T_final = T_0 * 0.2 (~ 10 iterations, with r=0.85)
* T = T_0
* N = n*\gamma where \gamma is a user defined constant

> repeat
>      repeat
>            New = Best + randomly select move // M3 requires trying several times.
>            #moves <- #moves + 1
>            ∆cost <- Cost(new) - Cost(Z)
>            if (∆cost \leq 0) or (random(0,1) < e^{ -∆cost / T }) // Boltzmann acceptance criterion, where r is a random number [0, 1)
>                 if(∆cost < 0) then downhill <- downhill + 1
>                 Current = New
>                 if cost(Current) < cost(Best) then Best <- Current
>            else
>                reject <- reject + 1
>      until (downhill >= N) or (#moves > 2N)
>      T <- r*T
>      #moves = 0
> until (reject / #moves > 0.95) or (T <= T_final) or OutOfTime
> return Best
-}
simulatedAnnealing :: MonadIO m => Input -> m Floorplan
simulatedAnnealing (Input problem Parameters{..}) = do
  simulatedAnnealing' problem aspectRatio lambda coolingRate gamma mode
{-# INLINE simulatedAnnealing #-}

simulatedAnnealing' ::
  MonadIO m =>
  Problem ->
  Interval AspectRatio ->
  Lambda ->
  CoolingRate ->
  Gamma ->
  Mode ->
  m Floorplan
simulatedAnnealing' problem aspectRatio lambda r gamma mode = do
  let n = problemSize problem
  initial <- case initialPE n of
    Nothing -> liftIO $ fail "Too small problem"
    Just pe -> return pe
  (_, initialCost, initialBB) <- evalStateT (computeCost initial lambda aspectRatio) problem
  _gen <- liftIO $ Random.createSystemRandom
  moveIncrAvg <- avgIncrementByMove _gen lambda aspectRatio problem initial
  _startTime <- liftIO $ Clock.getCurrentTime
  let p = (0.98 :: Double) -- Probability of acceptance at high-temperature.
      _currentTmp = coerce $ (- moveIncrAvg) / log p -- log = ln
      iterations = 50 :: Int
      _finalTmp = coerce $ (r ^ iterations) * (coerce _currentTmp)
      _bigN = n * (coerce gamma)
      _current = (initial, initialCost, initialBB)
      _best = (initial, initialCost, initialBB)
      _moves = 0
      _downhill = 0
      _rejects = 0
  runAnnealing Variables {..}
  where
    runComputeCost :: (MonadIO m) => PolishExpression -> m (Validity, Cost, IntMap BoundingBox)
    runComputeCost pe = evalStateT (computeCost pe lambda aspectRatio) problem
    {-# INLINE runComputeCost #-}

    runAnnealing :: (MonadIO m) => Variables -> m Floorplan
    runAnnealing variables = do
      Variables {..} <- execStateT (outerLoop 1) variables
      let (_, _, boundingBoxes) = _best
      return (Floorplan boundingBoxes)
    {-# INLINE runAnnealing #-}

    printPartialSolution :: (MonadState Variables m, MonadIO m) => m ()
    printPartialSolution =
      case mode of
        Production _ -> return ()
        Demo _ -> do
          --(_, _, boundingBoxes) <- use best
          (_, _, _) <- use best
          liftIO Console.clearScreen
          terminalSize <- liftIO Console.getTerminalSize
          case terminalSize of
            Nothing ->
              return ()
            Just (_, _) -> do
              liftIO $ Console.setCursorPosition 0 0 -- rows columns
              --Pretty.prettyPrint (Floorplan boundingBoxes)
              --liftIO $ threadDelay (10 ^ (5 :: Int))
    {-# INLINE printPartialSolution #-}

    outerLoop :: (MonadState Variables m, MonadIO m) => Int -> m ()
    outerLoop !it = do
      innerLoop 1
      currentTmp *= (coerce r) -- reduce temperature
      stop <- outerStopCondition
      unless stop $ do
        moves .= 0
        rejects .= 0
        downhill .= 0
        outerLoop (it + 1)
    {-# INLINE outerLoop #-}

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
      when (timeDiff > timeOutAfter) $ print @String "Time out!"
      return (rejectRate > 0.95 || _currentTmp < _finalTmp || timeDiff > timeOutAfter)
    {-# INLINE outerStopCondition #-}

    innerLoop :: (MonadState Variables m, MonadIO m) => Int -> m ()
    innerLoop !it = do
      Variables {..} <- get
      newPE <- perturbate _gen (fst3 _current)
      (validity, newCost, newBB) <- runComputeCost newPE
      moves += 1
      let incrCost = newCost - (snd3 _current)
      randomValue <- rand
      let alpha = coerce $ exp (- incrCost / (coerce _currentTmp)) :: Double
      if incrCost <= 0 || randomValue < alpha
        then do
          when (incrCost <= 0) $ downhill += 1
          current .= (newPE, newCost, newBB)
          -- We need the isValid check because we start from an invalid solution (aspect ratio)
          -- and we should *not* explore invalid solutions.
          let isValid = validity == Valid
          when (isValid && newCost < (snd3 _best)) $ do
            best .= (newPE, newCost, newBB)
            printPartialSolution
        else rejects += 1
      stop <- innerStopCondition
      unless stop $ innerLoop (it + 1)
    {-# INLINE innerLoop #-}

    innerStopCondition :: (MonadState Variables m) => m Bool
    innerStopCondition = do
      Variables {..} <- get
      return (_downhill >= _bigN || _moves >= 2 * _bigN)
    {-# INLINE innerStopCondition #-}

-- | Given an initial polish expression, apply a sequence of n random moves and
-- compute the average of the magnitude of increment of cost at each move.
--
-- This is an approximation.
avgIncrementByMove ::
  MonadIO m =>
  Random.GenIO ->
  Lambda ->
  Interval AspectRatio ->
  Problem ->
  PolishExpression ->
  m Double
avgIncrementByMove gen lambda aspectRatio problem initialPE = flip evalStateT problem $ do
  let n = 100 -- #perturbations to approximate the average
      getCost pe = snd3 <$> computeCost pe lambda aspectRatio
  initialCost <- getCost initialPE
  perturbationsCosts <- replicateM n (getCost =<< perturbate gen initialPE)
  return . average . fmap (coerce . abs . subtract initialCost) $ perturbationsCosts
{-# INLINE avgIncrementByMove #-}

------------------------------------------------------------------------------
-- Polish Expression Cost

data Validity = Valid | InvalidAspectRatio
  deriving stock (Show, Eq)

-- | Computes the cost of the best solution of a polish expression
--
-- The solution may not be valid:
-- * Aspect ratio constraint violated
-- * Missing module
--
-- Invalid solutions should be explored but never accepted as solutions.
computeCost ::
  (MonadState Problem m) =>
  PolishExpression ->
  Lambda ->
  Interval AspectRatio ->
  m (Validity, Cost, IntMap BoundingBox)
{-# INLINE computeCost #-}
computeCost pe lambda aspectRatioInterval = do
  let slicingTree = toSlicingTree pe
  solutions <- traverse (computeCost' slicingTree) =<< getShapeCurves slicingTree
  returnBestSolution solutions
  where
    -- Given the final shape of the bounding box, recursively deconstruct the bounding box of each module,
    -- computes the minimum wiring length for the solution and return the cost (area + wiring).
    computeCost' :: (MonadState Problem m) => SlicingTree -> ShapeCurve -> m (Validity, Cost, IntMap BoundingBox)
    computeCost' slicingTree (Coordinate a b, info) = do
      let moduleShapes = Map.fromList info
          boundingBoxes = getBoundingBoxes moduleShapes slicingTree
          area = a * b
          validAspectRatio = if inside aspectRatioInterval $ AspectRatio (b / a) then Valid else InvalidAspectRatio
      wirelength <- totalWireLength boundingBoxes
      let cost = Cost (area + ((coerce lambda) * (coerce wirelength)))
      return (validAspectRatio, cost, boundingBoxes)
    {-# INLINE computeCost' #-}

    returnBestSolution :: (MonadState Problem m) => [(Validity, Cost, IntMap BoundingBox)] -> m (Validity, Cost, IntMap BoundingBox)
    returnBestSolution solutions = do
      p <- use pproblem
      let problemModules = Map.keys p
          -- TODO should not be needed.
          includesAllModules (_, _, bbs) = Map.keys bbs == problemModules
          onlyValidSolutions = filter includesAllModules solutions
      return $ List.minimumBy (compare `on` snd3) onlyValidSolutions
    {-# INLINE returnBestSolution #-}

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

totalWireLength ::
  (MonadState Problem m) =>
  IntMap BoundingBox ->
  m WireLength
{-# INLINE totalWireLength #-}
totalWireLength boundingBoxes = do
  let centers = computeCenter <$> boundingBoxes
  Problem problem <- get
  return $ Map.foldlWithKey' (go centers) (0 :: WireLength) problem
  where
    go :: IntMap Coordinate -> WireLength -> ModuleIndex -> (x, [ModuleIndex]) -> WireLength
    go centers !acc moduleIndex (_, connections) =
      let getCenter i = centers ! i
          c1 = getCenter moduleIndex
          f acc moduleIndex2
            -- Avoid double counting distances
            | moduleIndex2 > moduleIndex = (+ acc) . coerce . manhattanDistance c1 $ getCenter moduleIndex2
            | otherwise = acc
       in acc + List.foldl' f (0 :: WireLength) connections
    {-# INLINE go #-}

-- | Returns the bounding box coordinates of each module.
getBoundingBoxes :: IntMap Shape -> SlicingTree -> IntMap BoundingBox
{-# INLINE getBoundingBoxes #-}
getBoundingBoxes moduleShapes = Map.fromList . snd . go (Coordinate 0 0)
  where
    getShape :: ModuleIndex -> Shape
    getShape moduleIndex = moduleShapes ! moduleIndex
    {-# INLINE getShape #-}

    getTopRight :: Coordinate -> Shape -> Coordinate
    getTopRight (Coordinate x y) (Shape' w h) = Coordinate (x + fromIntegral w) (y + fromIntegral h)
    getTopRight (Coordinate _ _) _ = error "incomplete-uni-pattern???"
    {-# INLINE getTopRight #-}

    go :: Coordinate -> SlicingTree -> (BoundingBox, [(ModuleIndex, BoundingBox)])
    {-# INLINE go #-}
    go bottomLeft (Leaf moduleIndex) =
      let moduleShape = getShape moduleIndex
          topRight = getTopRight bottomLeft moduleShape
          boundingBox = BoundingBox bottomLeft topRight
       in (boundingBox, [(moduleIndex, boundingBox)])
    go bottomLeft (Branch l V r) =
      let (BoundingBox bl@(Coordinate _ y1) (Coordinate x2 y2), info1) = go bottomLeft l
          (BoundingBox                   _  (Coordinate x2' y2'), info2) = go (Coordinate x2 y1) r
          tr = Coordinate x2' (max y2 y2')
       in (BoundingBox bl tr, info1 ++ info2)
    go bottomLeft (Branch l H r) =
      let (BoundingBox bl@(Coordinate x1 _) (Coordinate x2 y2), info1) = go bottomLeft l
          (BoundingBox                    _ (Coordinate x2' y2'), info2) = go (Coordinate x1 y2) r
          tr = Coordinate (max x2 x2') y2'
       in (BoundingBox bl tr, info1 ++ info2)

-----------------------------------------------------------------------------------
-- Shape Curves

type ShapeCurve = (Coordinate, [(ModuleIndex, Shape)])

type ShapeCurves = [ShapeCurve]

getShapeCurves ::
  (MonadState Problem m) =>
  SlicingTree ->
  m ShapeCurves
{-# INLINE getShapeCurves #-}
getShapeCurves (Branch left op right) = do
  l <- getShapeCurves left
  r <- getShapeCurves right
  return (combineShapeCurves op l r)
getShapeCurves (Leaf moduleIndex) =
  sort' . (fmap toShapeCurve) <$> getModuleShapes moduleIndex
    where
      sort' = List.sortBy (compare `on` (_x . fst))
      {-# INLINE sort' #-}

      toShapeCurve shape@(Shape (Width w, Height h)) =
        (Coordinate {_x = fromIntegral w, _y = fromIntegral h}, [(moduleIndex, shape)])
      {-# INLINE toShapeCurve #-}

      -- | Returns the shapes associated to a module
      getModuleShapes :: (MonadState Problem m) => ModuleIndex -> m [Shape]
      getModuleShapes i = gets (\(Problem s) -> fst $ s ! i)
      {-# INLINE getModuleShapes #-}

-- | Combine two shape curves using the formulas from the paper.
combineShapeCurves ::
  Operator ->
  ShapeCurves ->
  ShapeCurves ->
  ShapeCurves
{-# INLINE combineShapeCurves #-}
combineShapeCurves op l r =
  let limit = maximum $ fmap (minimum . fmap (getXY . fst)) [l, r]
      aboveLimit = filter ((>= limit) . getXY . fst)
      lCurves = intersectAndCombine r <$> aboveLimit l
      rCurves = intersectAndCombine l <$> aboveLimit r
      removeDuplicates = List.nubBy ((==) `on` fst)
   in removeDuplicates $ combineAndSort lCurves rCurves
  where
    getXY = if op == V then _y else _x
    {-# INLINE getXY #-}

    intersectAndCombine :: ShapeCurves -> ShapeCurve -> ShapeCurve
    intersectAndCombine curves (c@(Coordinate x1 y1), i) =
      let f = if op == V then reverse else id
          (Coordinate x2 y2, i') = intersection getXY (f curves) c
          info = i ++ i'
          coord = case op of
            V -> Coordinate {_x = x1 + x2, _y = max y1 y2} -- A | B
            H -> Coordinate {_x = max x1 x2, _y = y1 + y2}
       in (coord, info)
    {-# INLINE intersectAndCombine #-}

    -- Hacky: works only if curves are sorted in 'x' increasing order for H and 'y' increasing order for V
    -- This function avoid repeating permutations of shapes but it increasing the complexity by a lot.
    -- FIXME you only need to compute permutations of shapes in the current setting.
    --       This function can be simplified.
    intersection :: (Coordinate -> Double) -> ShapeCurves -> Coordinate -> ShapeCurve
    intersection getCoord (x : xs) c1 = go x xs
      where
        go previous [] = previous
        go previous (next@(c2, _) : rest)
          | getCoord c1 < getCoord c2 = previous
          | otherwise = go next rest
    intersection _ _ _ = error "Empty shapeCurve!"
    {-# INLINE intersection #-}

    -- Sort 'x' in increasing order (implictly sorts 'y' in decreasing order because @f@ is a decreasing function).
    --
    -- Note, since points in previous ShapeCurves were sorted, we can sort faster.
    combineAndSort :: ShapeCurves -> ShapeCurves -> ShapeCurves
    combineAndSort [] ys = ys
    combineAndSort xs [] = xs
    combineAndSort (x : xs) (y : ys)
      | x `less` y = x : combineAndSort xs (y : ys)
      | otherwise = y : combineAndSort (x : xs) ys
      where
        less :: ShapeCurve -> ShapeCurve -> Bool
        less (Coordinate x1 _, _) (Coordinate x2 _, _) = x1 < x2
    {-# INLINE combineAndSort #-}

---------------------------------------------------------------------
-- Util

class HasGen (s :: Type) (m :: Type -> Type) where
  getGen :: m Random.GenIO

instance (MonadState s m, HasType Random.GenIO s) => HasGen s m where
  {-# INLINE getGen #-}
  getGen = getTyped <$> get

-------------------------------

data Return = Always | Sometimes

type family Maybe' (r :: Return) (a :: Type) :: Type where
  Maybe' 'Always a = a
  Maybe' 'Sometimes a = Maybe a

data DReturn (r :: Return) (a :: Type) where
  DAlways :: DReturn 'Always a
  DSometimes :: a -> DReturn 'Sometimes a

deriving stock instance Functor (DReturn r)

-------------------------------

-- | Double generated u.a.r from the range [0,1]
rand :: forall s m. (HasGen s m, MonadIO m) => m Double
rand = liftIO . Random.uniformRM (0.0, 1.0) =<< getGen @s
{-# INLINE rand #-}

average :: (Fractional a) => [a] -> a
average [] = error "Average of empty list."
average xs = x / (fromIntegral n) where
  go (!n, !acc) x = (n + 1, acc + x)
  (n, x) = List.foldl' go (1 :: Int, head xs) (tail xs)
{-# INLINE average #-}

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
{-# INLINE fst3 #-}

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
{-# INLINE snd3 #-}

thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c
{-# INLINE thrd3 #-}

print :: forall a m. (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print
{-# INLINE print #-}
