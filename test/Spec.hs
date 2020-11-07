module Main (main) where

import           System.IO  (hSetEncoding, stderr, stdout, utf8)
import           Test.Hspec

import Test.PolishExpression
import Test.SimulatedAnnealing

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hspec allUnitTests

allUnitTests :: Spec
allUnitTests = describe "Floorplanning" $ do
  polishExpressionSpec
  simulatedAnnealingSpec
