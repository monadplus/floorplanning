module Demo where

import Control.Concurrent
import qualified Data.IntMap.Strict as Map
import Floorplan
import System.Console.ANSI
import System.IO

-- TODO the demo should use random generators
runDemo :: IO ()
runDemo = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  let Just aspectRatioInterval = mkInterval ((AspectRatio 0), (AspectRatio 100000))
      Just lambda = mkLambda 0.5
      Right problem =
        validateProblem $
          Map.fromList
            [ (1, ([Shape' 1 4, Shape' 4 1], [6, 7])),
              (2, ([Shape' 3 3], [3, 4, 5])),
              (3, ([Shape' 2 1, Shape' 1 2], [2, 8, 9])),
              (4, ([Shape' 3 2, Shape' 2 3], [2, 5, 6])),
              (5, ([Shape' 2 4, Shape' 4 2], [2, 4, 6])),
              (6, ([Shape' 2 4, Shape' 4 2], [1, 4, 5])),
              (7, ([Shape' 1 5, Shape' 5 1], [1, 9])),
              (8, ([Shape' 2 6, Shape' 6 2], [3])),
              (9, ([Shape' 2 2], [3, 7]))
            ]

  floorplan <-
    simulatedAnnealing
      problem
      aspectRatioInterval
      lambda
      defaultCoolingRate
      defaultGamma

  return () -- prettyPrint floorplan
