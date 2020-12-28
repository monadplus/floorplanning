{-| The Floorplanning Problem

The original problem received a list of triplets:

* \[(A_i, r_i, s_i)\]  where A_i is the area, r_i and s_i are the limits of the aspect ratio

Some modules are allowed to rotate 90 degrees:

* (1) w_i * h_i = A_i
* (2) r_i ≤ h_i/w_i ≤ s_i                                     if i ∋ O_1
* (3) r_i ≤ h_i/w_i ≤ s_i   or     1/r_i ≤ h_i/w_i ≤ s_i      if i ∋ O_2

* O_1 = fixed orientation modules
* O_2 = free orientation modules

Objective: minimize global bounding box subject to aspect ratio constraints
           minimize total wirelength between blocks
           a*area(F) + (1 - a)*L(F)     0 <= a <= 1
-}
module Floorplan
  ( -- * Annealing Schedule: main function
    simulatedAnnealing,

    -- * Data Types
    Config(..),
    Mode(..),
    Lambda,
    mkLambda,
    Gamma,
    mkGamma,
    defaultGamma,
    CoolingRate,
    mkCoolingRate,
    defaultCoolingRate,
    Problem,
    mkProblem,
    genProblem,

    -- * Pretty printer of the solution
    module Floorplan.Pretty,

    -- * Floorplan Types
    module Floorplan.Types,

    -- * Reexports
    module Data.IntMap.Strict,
  )
where

import Data.IntMap.Strict
import Floorplan.Pretty
import Floorplan.SimulatedAnnealing
import Floorplan.Types
import Floorplan.Problem
