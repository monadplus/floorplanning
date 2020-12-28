{-# LANGUAGE RecordWildCards #-}
module Floorplan.Report
  ( Report(..)
  , mkReport
  , printReport
  )where

import Floorplan.Types
import Floorplan.SimulatedAnnealing
import qualified Floorplan.Pretty as Pretty
import Control.Monad.State.Lazy
import Data.Coerce
import Text.Tabular
import qualified Text.Tabular.AsciiArt as Ascii

data Report = Report
  { rModules :: Int,
    rLambda :: Lambda,
    rTotalArea :: Area, -- ^ Minimum total area
    rArea :: Area, -- ^ total area
    rWirelength :: WireLength,
    rFloorplan :: Floorplan
  }

mkReport :: Input -> Floorplan -> Report
mkReport (Input problem Parameters{..}) plan = Report{..}
  where
    rModules = length (getModules plan)
    rLambda = lambda
    rTotalArea = minTotalArea plan
    rArea = totalArea plan
    rWirelength = evalState (totalWireLength $ coerce plan) problem
    rFloorplan = plan

printReport :: Report -> IO ()
printReport Report{..} = do
  putStrLn "============ Report ============"
  Pretty.prettyPrint rFloorplan
  putStrLn ""
  putStrLn $ Ascii.render id id id table
  where
    table :: Table String String String
    table =  Table
      (Group SingleLine
        [ Group NoLine [Header "P1"]
        ])
      (Group DoubleLine
        [ Group SingleLine [Header "n", Header "λ", Header "Total Area", Header "A", Header "W"]
        ])
      [[show rModules, show rLambda, show rTotalArea, show rArea, show rWirelength]
      ]
