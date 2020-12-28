module Floorplan.Report
  ( Report(..)
  , mkReport
  , printReport
  )where

import Floorplan.Types
import Floorplan.Pretty

data Report = Report
  { floorplan :: Floorplan
  }

mkReport :: Floorplan -> Report
mkReport = undefined

printReport :: Report -> IO ()
printReport = undefined
