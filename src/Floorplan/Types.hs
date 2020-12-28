{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
module Floorplan.Types
  ( ModuleIndex
  , Height(..)
  , Width(..)
  , Shape(.., Shape')
  , rotate90degrees
  , Area(..)
  , WireLength(..)
  , AspectRatio(..)
  , Interval
  , mkInterval
  , inside
  , Cost(..)
  , Coordinate(..)
  , manhattanDistance
  , BoundingBox(.., BoundingBox')
  , computeCenter
  , boundingBoxArea
  , Floorplan(..)
  , getModules
  , totalArea
  , minTotalArea
  , toEither
  , readInstance
  )where

----------------------------------------------------------------------------------

import Data.Coerce
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.Semigroup
import qualified Data.List as List
import Data.Function

----------------------------------------------------------------------------------

type ModuleIndex = Int

newtype Height = Height {_height :: Int} deriving newtype (Show, Eq, Ord, Num)
newtype Width = Width {_width :: Int} deriving newtype (Show, Eq, Ord, Num)

--------------------------------

newtype Shape = Shape (Width, Height) deriving stock (Show, Eq, Ord)

pattern Shape' :: Int -> Int -> Shape
pattern Shape' w h = Shape ((Width w), (Height h))

rotate90degrees :: Shape -> Shape
rotate90degrees (Shape' w h) = Shape' h w
rotate90degrees _ = error "non-exhaustive pattern match ?"
{-# INLINE rotate90degrees #-}

--------------------------------

newtype Area = Area {_area :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional)
  deriving (Semigroup, Monoid) via (Sum Double)

--------------------------------

newtype WireLength = WireLength {_wireLength :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional)

--------------------------------

newtype AspectRatio = AspectRatio {_aspectRatio :: Double}
  deriving newtype (Read, Show, Eq, Ord, Num, Fractional)

--------------------------------

newtype Interval a = Interval {_interval :: (a, a)}
  deriving newtype (Show, Eq)

instance (Read a, Ord a) => Read (Interval a) where
  readsPrec = readInstance (toEither "Error: invalid interval" . mkInterval)

mkInterval :: (Ord a) => (a, a) -> Maybe (Interval a)
mkInterval interval@(a, b)
  | a <= b = Just (coerce interval)
  | otherwise = Nothing
{-# INLINE mkInterval #-}

inside :: (Ord a) => Interval a -> a -> Bool
inside (Interval (a, b)) x
  | a <= x && x <= b = True
  | otherwise = False
{-# INLINE inside #-}

--------------------------------

-- | TODO Cost > 0
newtype Cost = Cost {_cost :: Double}
  deriving newtype (Show, Eq, Ord, Num, Floating, Fractional)

--------------------------------

data Coordinate = Coordinate {_x :: Double, _y :: Double}
  deriving stock (Show, Eq, Ord)

manhattanDistance :: Coordinate -> Coordinate -> Double
manhattanDistance (Coordinate x1 y1) (Coordinate x2 y2) =
  abs (x1 - x2) + abs (y1 - y2)
{-# INLINE manhattanDistance #-}

--------------------------------

data BoundingBox = BoundingBox {_bottomLeft :: Coordinate, _topRigth :: Coordinate}
  deriving stock (Show, Eq)

pattern BoundingBox' :: Double -> Double -> Double -> Double -> BoundingBox
pattern BoundingBox' x_bl y_bl x_tr y_tr <- BoundingBox (Coordinate !x_bl !y_bl) (Coordinate !x_tr !y_tr) where
  BoundingBox' !x_bl !y_bl !x_tr !y_tr = BoundingBox (Coordinate x_bl y_bl) (Coordinate x_tr y_tr) where

computeCenter :: BoundingBox -> Coordinate
computeCenter (BoundingBox' x_bl y_bl x_tr y_tr) = Coordinate ((x_bl + x_tr) / 2) ((y_bl + y_tr) / 2)
computeCenter _ = error "incomplete-uni-patterns???"
{-# INLINE computeCenter #-}

boundingBoxArea :: BoundingBox -> Area
boundingBoxArea (BoundingBox' x0 y0 x y) = Area ((x - x0)*(y - y0))
boundingBoxArea _ = error "incomplete-uni-patterns???"
{-# INLINE boundingBoxArea #-}

--------------------------------

-- | Keys of the map correspondgo to modules
newtype Floorplan = Floorplan { _floorplan :: IntMap BoundingBox }

getModules :: Floorplan -> [ModuleIndex]
getModules (Floorplan dict) = Map.keys dict

-- | Area of the floorplan in \(R^2\)
totalArea :: Floorplan -> Area
totalArea (Floorplan dict) = Area (w*h)
  where
    xs = fmap _topRigth $ Map.elems dict
    Coordinate w _ = List.maximumBy (compare `on` _x) xs
    Coordinate _ h = List.maximumBy (compare `on` _y) xs
{-# INLINE totalArea #-}

-- | Sum of area of each block of the floorplan
--
-- prop> minTotalArea <= totalArea
--
-- The floorplan has the best area when minTotalArea = totalArea.
minTotalArea :: Floorplan -> Area
minTotalArea (Floorplan dict) =
  foldMap boundingBoxArea (Map.elems dict)
{-# INLINE minTotalArea #-}

---------------------------------

toEither :: String -> Maybe a -> Either String a
toEither err = maybe (Left err) Right
{-# INLINE toEither #-}

readInstance :: Read b => (b -> Either String a) -> Int -> ReadS a
readInstance constr d s =
    [ (a, s')
    | (b, s') <- readsPrec d s,
      let a =
            case constr b of
              Left err -> error err
              Right a -> a
    ]
{-# INLINE readInstance #-}
