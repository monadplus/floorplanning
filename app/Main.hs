module Main where

import Floorplan
import qualified Data.IntMap.Strict as Map
import System.IO

main :: IO ()
main = do

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  let Just aspectRatioInterval = mkInterval ((AspectRatio 0.5), (AspectRatio 2))
      Just lambda = mkLambda 1.0
      problem =
        Problem $
          Map.fromList
            [ (1, ([Shape' 1 3], [])),
              (2, ([Shape' 2 4], [])),
              (3, ([Shape' 2 2], [])),
              (4, ([Shape' 3 3], []))
            ]

  floorplan <- simulatedAnnealing
                 problem
                 aspectRatioInterval
                 lambda
                 defaultCoolingRate
                 defaultGamma

  floorplan `seq` putStrLn "Floorplan finished!"

  prettyPrint floorplan
