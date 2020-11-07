{-# LANGUAGE TypeApplications #-}

module Test.SimulatedAnnealing where

import Control.Monad
import Data.Function (on)
import qualified Data.IntMap.Strict as IMap
import Data.List
import Lib.PolishExpression
import Lib.SimulatedAnnealing
import Test.Hspec

simulatedAnnealingSpec :: Spec
simulatedAnnealingSpec =
  describe "Simulated Annealing Tests" $ do
    shapeCurvesSpec
    slicingTreeSpec

shapeCurvesSpec :: Spec
shapeCurvesSpec = describe "Shape Curves" $ do
  describe "Base cases" $ do
    let problem =
          Problem $
            IMap.fromList
              [ (1, ([Shape' 3 2, Shape' 1 4], [])), -- No wiring
                (2, ([Shape' 2 5, Shape' 5 3], [])) -- No wiring
              ]

    it "should return the shapeCurves of a '+'" $ do
      let pe = parsePolishExpression' "12+"
          expected =
            [ (Coordinate 2 9, [(1, Shape' 1 4), (2, Shape' 2 5)]),
              (Coordinate 3 7, [(1, Shape' 3 2), (2, Shape' 2 5)]),
              (Coordinate 5 5, [(1, Shape' 3 2), (2, Shape' 5 3)])
            ]
      runCurveTest problem pe expected

    it "should return the shapeCurves of a '*'" $ do
      let pe = parsePolishExpression' "12*"
          expected =
            [ (Coordinate 3 5, [(1, Shape' 1 4), (2, Shape' 2 5)]),
              (Coordinate 6 4, [(1, Shape' 1 4), (2, Shape' 5 3)]),
              (Coordinate 8 3, [(1, Shape' 3 2), (2, Shape' 5 3)])
            ]
      runCurveTest problem pe expected

  describe "More involved cases" $ do
    it "should return the shapeCurves of '123*+'" $ do
      let problem =
            Problem $
              IMap.fromList
                [ (1, ([Shape' 2 2, Shape' 1 3], [])),
                  (2, ([Shape' 3 2, Shape' 1 4], [])),
                  (3, ([Shape' 2 5, Shape' 5 3], []))
                ]
          pe = parsePolishExpression' "123*+"
          expected =
            [ (Coordinate 3 7, [(2, Shape' 1 4), (3, Shape' 2 5), (1, Shape' 2 2)]),
              (Coordinate 6 6, [(2, Shape' 1 4), (3, Shape' 5 3), (1, Shape' 2 2)]),
              (Coordinate 8 5, [(2, Shape' 3 2), (3, Shape' 5 3), (1, Shape' 2 2)])
            ]
      runCurveTest problem pe expected

    it "should return the shapeCurves of '123**'" $ do
      let problem =
            Problem $
              IMap.fromList
                [ (1, ([Shape' 2 2, Shape' 1 6], [])),
                  (2, ([Shape' 3 2, Shape' 1 4], [])),
                  (3, ([Shape' 2 5, Shape' 5 3], []))
                ]
          pe = parsePolishExpression' "123**"
          expected =
            [ (Coordinate 4 6, [(2, Shape' 1 4), (3, Shape' 2 5), (1, Shape' 1 6)]),
              (Coordinate 5 5, [(2, Shape' 1 4), (3, Shape' 2 5), (1, Shape' 2 2)]),
              (Coordinate 8 4, [(2, Shape' 1 4), (3, Shape' 5 3), (1, Shape' 2 2)]),
              (Coordinate 10 3, [(2, Shape' 3 2), (3, Shape' 5 3), (1, Shape' 2 2)])
            ]
      runCurveTest problem pe expected
    it "should return the shapeCurves of '4123**+'" $ do
      let problem =
            Problem $
              IMap.fromList
                [ (1, ([Shape' 2 2, Shape' 1 6], [])),
                  (2, ([Shape' 3 2, Shape' 1 4], [])),
                  (3, ([Shape' 2 5, Shape' 5 3], [])),
                  (4, ([Shape' 1 5], []))
                ]
          pe = parsePolishExpression' "4123**+"
          expected =
            [ (Coordinate 4 11, [(2, Shape' 1 4), (3, Shape' 2 5), (1, Shape' 1 6), (4, Shape' 1 5)]),
              (Coordinate 5 10, [(2, Shape' 1 4), (3, Shape' 2 5), (1, Shape' 2 2), (4, Shape' 1 5)])
            ]
      runCurveTest problem pe expected
  where
    runCurveTest :: Problem -> PolishExpression -> ShapeCurves -> Expectation
    runCurveTest problem pe expected = do
      let slicingTree = toSlicingTree pe
          result = evalState (shapeCurves slicingTree) problem
          sortedExpected = sortBy (compare `on` fst) expected
      forM_ (zip result sortedExpected) $ \((c1, info1), (c2, info2)) -> do
        c1 `shouldBe` c2
        info1 `shouldMatchList` info2

slicingTreeSpec :: Spec
slicingTreeSpec = describe "Slicing Tree" $ do
  it "toSlicingTree" $ do
    let pe = parsePolishExpression' "123*4+*"
        result = toSlicingTree pe
        expected = Branch (Leaf 1) V (Branch (Branch (Leaf 2) V (Leaf 3)) H (Leaf 4))
    result `shouldBe` expected

  it "toSlicingTree (2)" $ do
    let pe = parsePolishExpression' "12+34*56*++"
        result = toSlicingTree pe
        expected = Branch (Branch (Leaf 1) H (Leaf 2)) H (Branch (Branch (Leaf 3) V (Leaf 4)) H (Branch (Leaf 5) V (Leaf 6)))
    result `shouldBe` expected

  it "toSlicingTree (3)" $ do
    let Just pe = initialPE 2
        result = toSlicingTree pe
        expected = Branch (Leaf 1) V (Leaf 2)
    result `shouldBe` expected
