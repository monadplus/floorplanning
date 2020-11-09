{-# LANGUAGE DerivingStrategies #-}

module Lib.Pretty
  ( pretty,
    prettyPrint,
  )
where

import qualified Data.IntMap.Strict as Map
import qualified Data.List as List
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Lib.SimulatedAnnealing

----------------------------------------------------------

prettyPrint :: MonadIO m => Floorplan -> m ()
prettyPrint = liftIO . putStrLn . pretty

-- | You need to set output to utf8
pretty :: Floorplan -> String
pretty (Floorplan boundingBoxes) =
  show $ List.foldl' (fillMatrix rows) matrix discreteBoundingBoxes
  where
    getXY (_, (BoundingBox _ (Coordinate x y))) = (floor x, floor y)
    (columns, rows) = maximum . fmap getXY . Map.toList $ boundingBoxes
    matrix = Matrix.zero rows columns :: Matrix ModuleIndex
    discreteBoundingBoxes = (fmap . fmap) toDBB $ Map.toList boundingBoxes

    fillMatrix :: Int -> Matrix ModuleIndex -> (ModuleIndex, DBoundingBox) -> Matrix ModuleIndex
    fillMatrix rows matrix (moduleIndex, DBoundingBox x_bl y_bl x_tr y_tr) =
      List.foldl' go matrix [(n, m) | m <- [x_bl + 1 .. x_tr], n <- [y_bl + 1 .. y_tr]]
      where
        -- Row/Column from 1 to n
        go :: Matrix ModuleIndex -> (Int, Int) -> Matrix ModuleIndex
        go matrix (n, m) = Matrix.setElem moduleIndex (rows - n + 1, m) matrix

----------------------------------------------------------

-- | Discrete Bounding Box
data DBoundingBox = DBoundingBox {x_bl :: Int, y_bl :: Int, x_tr :: Int, y_t :: Int}
  deriving stock (Show)

toDBB :: BoundingBox -> DBoundingBox
toDBB (BoundingBox' x_bl y_bl x_tr y_tr) = DBoundingBox (floor x_bl) (floor y_bl) (floor x_tr) (floor y_tr)
