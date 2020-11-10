module Main where

import Floorplan
import qualified Data.IntMap.Strict as Map
import System.IO
import System.Console.ANSI
import Control.Concurrent

main :: IO ()
main = do

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  let Just aspectRatioInterval = mkInterval ((AspectRatio 0.5), (AspectRatio 2))
      Just lambda = mkLambda 1.0
      Right problem =
        validateProblem $
          Map.fromList
            [ (1, ([Shape' 2 2], [])),
              (2, ([Shape' 2 2], [])),
              (3, ([Shape' 2 2], [])),
              (4, ([Shape' 2 2], []))
            ]

  floorplan <- simulatedAnnealing
                 problem
                 aspectRatioInterval
                 lambda
                 defaultCoolingRate
                 defaultGamma

  prettyPrint floorplan

-- oneToTen :: IO ()
-- oneToTen = do
--   Just (rows, _) <- getTerminalSize
--   setCursorPosition (rows `div` 2) 0
--   go 10 rows
--   where
--   go 0 rows = print 0
--   go n rows = do
--     clearScreen
--     print n
--     setCursorPosition (rows `div` 2) 0
--     threadDelay (10^6)
--     go (n - 1) rows
