{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Floorplan.Types
  ( ModuleIndex
  , Height(..)
  , Width(..)
  , Shape(.., Shape')
  , WireLength(..)
  , AspectRatio(..)
  , Interval
  , mkInterval
  , inside
  , Cost(..)
  , Coordinate(..)
  , manhattanDistance
  , BoundingBox(.., BoundingBox')
  , BoundingBoxes
  , computeCenter
  , Floorplan(..)
  )where

----------------------------------------------------------------------------------

import Data.Coerce
import Data.IntMap.Strict (IntMap)

----------------------------------------------------------------------------------

type ModuleIndex = Int

newtype Height = Height {_height :: Int} deriving newtype (Show, Eq, Ord, Num)
newtype Width = Width {_width :: Int} deriving newtype (Show, Eq, Ord, Num)

--------------------------------

newtype Shape = Shape (Width, Height) deriving stock (Show, Eq, Ord)

pattern Shape' :: Int -> Int -> Shape
pattern Shape' w h = Shape ((Width w), (Height h))

--------------------------------

newtype WireLength = WireLength {_wireLength :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional)

--------------------------------

newtype AspectRatio = AspectRatio {_aspectRatio :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional)

--------------------------------

newtype Interval a = Interval {_interval :: (a, a)}
  deriving newtype (Show, Eq)

mkInterval :: (Ord a) => (a, a) -> Maybe (Interval a)
mkInterval interval@(a, b)
  | a <= b = Just (coerce interval)
  | otherwise = Nothing

inside :: (Ord a) => Interval a -> a -> Bool
inside (Interval (a, b)) x
  | a <= x && x <= b = True
  | otherwise = False

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

--------------------------------

data BoundingBox = BoundingBox {_bottomLeft :: Coordinate, _topRigth :: Coordinate}
  deriving stock (Show, Eq)

type BoundingBoxes = IntMap BoundingBox

pattern BoundingBox' :: Double -> Double -> Double -> Double -> BoundingBox
pattern BoundingBox' x_bl y_bl x_tr y_tr = BoundingBox (Coordinate x_bl y_bl) (Coordinate x_tr y_tr)

computeCenter :: BoundingBox -> Coordinate
computeCenter (BoundingBox' x_bl y_bl x_tr y_tr) = Coordinate ((x_bl + x_tr) / 2) ((y_bl + y_tr) / 2)

--------------------------------

newtype Floorplan = Floorplan { _floorplan :: BoundingBoxes }
