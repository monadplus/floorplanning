{-# LANGUAGE DerivingStrategies #-}

module Floorplan.Pretty
  ( pretty,
    prettyPrint,
  )
where

----------------------------------------------------------

import Control.Monad.IO.Class
import qualified Data.IntMap.Strict as Map
import qualified Data.List as List
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Floorplan.Types

----------------------------------------------------------

prettyPrint :: MonadIO m => Floorplan -> m ()
prettyPrint = liftIO . putStrLn . pretty

-- | You need to set output to utf8
pretty :: Floorplan -> String
pretty = Matrix.prettyMatrix . toMatrix

----------------------------------------------------------

-- | Discrete Bounding Box
data DBoundingBox = DBoundingBox {x_bl :: Int, y_bl :: Int, x_tr :: Int, y_t :: Int}
  deriving stock (Show)

toDBB :: BoundingBox -> DBoundingBox
toDBB (BoundingBox' x_bl y_bl x_tr y_tr) = DBoundingBox (floor x_bl) (floor y_bl) (floor x_tr) (floor y_tr)
toDBB _ = error "non-exhaustive pattern match ???"

toMatrix :: Floorplan -> Matrix ModuleIndex
toMatrix (Floorplan boundingBoxes) =
  List.foldl' (fillMatrix nRows) matrix discreteBoundingBoxes
  where
    nColumns =
      let getX (_, (BoundingBox _ (Coordinate x _))) = floor x
       in maximum . fmap getX . Map.toList $ boundingBoxes

    nRows =
      let getY (_, (BoundingBox _ (Coordinate _ y))) = floor y
       in maximum . fmap getY . Map.toList $ boundingBoxes

    -- Indexed at 1
    matrix = Matrix.zero nRows nColumns :: Matrix ModuleIndex

    discreteBoundingBoxes = (fmap . fmap) toDBB $ Map.toList boundingBoxes

    fillMatrix :: Int -> Matrix ModuleIndex -> (ModuleIndex, DBoundingBox) -> Matrix ModuleIndex
    fillMatrix rows matrix (moduleIndex, DBoundingBox x_bl y_bl x_tr y_tr) =
      -- From 0 to n-1, but matrix are index at 1 so from 1 to n.
      List.foldl' go matrix [(n, m) | n <- [y_bl + 1 .. y_tr], m <- [x_bl + 1 .. x_tr]]
      where
        -- Row/Column from 1 to n
        go :: Matrix ModuleIndex -> (Int, Int) -> Matrix ModuleIndex
        go matrix (n, m) = Matrix.setElem moduleIndex (rows - n + 1, m) matrix
