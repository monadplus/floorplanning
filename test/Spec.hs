module Main (main) where

import System.IO
import Test.Hspec
import Test.PolishExpression
import Test.Pretty
import Test.Problem
import Test.SimulatedAnnealing

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout NoBuffering
  hspec allUnitTests

allUnitTests :: Spec
allUnitTests = describe "Floorplanning" $ do
  polishExpressionSpec
  simulatedAnnealingSpec
  prettySpec
  problemSpec
