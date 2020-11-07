{-# LANGUAGE TypeApplications #-}
module Test.SimulatedAnnealing where

import Lib.SimulatedAnnealing
import Test.Hspec

simulatedAnnealingSpec :: Spec
simulatedAnnealingSpec =
  describe "Simulated Annealing Tests" $ do
    areaSpec

areaSpec :: Spec
areaSpec = describe "Area" $ do
  it "should compute the best area" $ do
    return @IO ()
