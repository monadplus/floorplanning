module Main where

import Lib
import qualified Data.IntMap.Strict as Map
import System.IO

main :: IO ()
main = do

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  let Just aspectRatioInterval = mkInterval ((AspectRatio 0.5), (AspectRatio 1.5))
      Just lambda = mkLambda 1.0
      problem =
        Problem $
          Map.fromList
            [ (1, ([Shape' 8 2], [1,3])),
              (2, ([Shape' 8 2], [2,3])),
              (3, ([Shape' 2 4], [1,2])),
              (4, ([Shape' 2 4], [])),
              (5, ([Shape' 2 4], [])),
              (6, ([Shape' 2 4], [])),
              (7, ([Shape' 5 1], [])),
              (8, ([Shape' 3 4], [])),
              (9, ([Shape' 3 4], []))
            ]

  floorplan <- Lib.simulatedAnnealing
                 problem
                 aspectRatioInterval
                 lambda
                 defaultCoolingRate
                 defaultGamma

  floorplan `seq` putStrLn "Floorplan finished!"

  Lib.prettyPrint floorplan
