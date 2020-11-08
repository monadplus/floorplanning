{-# LANGUAGE TypeApplications #-}

module Test.SimulatedAnnealing where

import Control.Monad
import Data.Foldable
import Data.Function (on)
import qualified Data.IntMap.Strict as Map
import Data.List
import Lib.PolishExpression
import Lib.SimulatedAnnealing
import Test.Hspec

simulatedAnnealingSpec :: Spec
simulatedAnnealingSpec =
  describe "Simulated Annealing Tests" $ do
    shapeCurvesSpec
    slicingTreeSpec
    wireLengthSpec
    costSpec

shapeCurvesSpec :: Spec
shapeCurvesSpec = describe "Shape Curves" $ do
  it "should return a single curve when there is only one shape per module" $ do
    let slicingTree = toSlicingTree $ parsePolishExpression' "12*3+45+*"
        problem =
          Problem $
            Map.fromList
              [ (1, ([Shape' 1 1], [])),
                (2, ([Shape' 2 2], [])),
                (3, ([Shape' 3 3], [])),
                (4, ([Shape' 4 4], [])),
                (5, ([Shape' 5 5], []))
              ]
        result = evalState (getShapeCurves slicingTree) problem
    result `shouldSatisfy` ((== 1) . length)

  describe "Base cases" $ do
    let problem =
          Problem $
            Map.fromList
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
              Map.fromList
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
              Map.fromList
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
              Map.fromList
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
          result = evalState (getShapeCurves slicingTree) problem
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

  it "toPolishExpression" $ do
    let f = toPolishExpression . toSlicingTree
        runTest pe = f pe `shouldBe` pe
        polishExpressions = ["12+34*56*++", "12*4*5*6*7*", "123**"]
    traverse_ (runTest . parsePolishExpression') polishExpressions

wireLengthSpec :: Spec
wireLengthSpec = describe "Wire Length" $ do
  {-
    +--------+---+
    |   3    | 5 |
    +---+----|   |
    | 1 | 2  +---+
    |   |    | 4 |
    +---+----+---+
  -}
  let slicingTree = toSlicingTree $ parsePolishExpression' "12*3+45+*"
      moduleShapes =
        Map.fromList
          [ (1, Shape' 2 3),
            (2, Shape' 2 3),
            (3, Shape' 4 2),
            (4, Shape' 2 2),
            (5, Shape' 2 3)
          ]

  describe "boundingBoxes" $ do
    it "should return the bounding box coordinates" $ do
      let result = getBoundingBoxes moduleShapes slicingTree
          expected =
            Map.fromList
              [ (1, BoundingBox' 0 0 2 3),
                (2, BoundingBox' 2 0 4 3),
                (3, BoundingBox' 0 3 4 5),
                (4, BoundingBox' 4 0 6 2),
                (5, BoundingBox' 4 2 6 5)
              ]
      result `shouldBe` expected

  describe "totalWireLength" $ do
    it "should return the total wire length between modules" $ do
      let problem =
            Problem $
              Map.fromList
                [ (1, ([], [2, 3, 5])),
                  (2, ([], [1, 3, 4])),
                  (3, ([], [1, 2])),
                  (4, ([], [2, 5])),
                  (5, ([], [1, 4]))
                ]
      let result = evalState (totalWireLength moduleShapes slicingTree) problem
          -- expected computed by hand
          expected = (20.0 :: WireLength)
      result `shouldBe` expected

-- TODO cost on multiple shapes
costSpec :: Spec
costSpec = describe "Cost" $ do
  it "should return the cost of a polish expression" $ do
    {- Reusing the wirelenght of wireLengthSpec
      +--------+---+
      |   3    | 5 |
      +---+----|   |
      | 1 | 2  +---+
      |   |    | 4 |
      +---+----+---+
    -}
    let pe = parsePolishExpression' "12*3+45+*"
        problem =
           Problem $
              Map.fromList
                [ (1, ([Shape' 2 3], [2, 3, 5])),
                  (2, ([Shape' 2 3], [1, 3, 4])),
                  (3, ([Shape' 4 2], [1, 2])),
                  (4, ([Shape' 2 2], [2, 5])),
                  (5, ([Shape' 2 3], [1, 4]))
                ]
        result = evalState (computeCost pe (0.5 :: Lambda)) problem
        expected = Cost (5*6 + 0.5*20.0) -- 40
    result `shouldBe` expected
  it "should return the cost of a polish expression with multiple shapes" $ do
    let pe = parsePolishExpression' "12*3+45+*"
        -- Notice the shapes are intentionally smaller on one of the shapes.
        -- The solver should pick the smallest shape and the solution should be different from the previous test.
        problem =
           Problem $
              Map.fromList
                [ (1, ([Shape' 2 3, Shape' 1 2], [2, 3, 5])),
                  (2, ([Shape' 1 2, Shape' 2 3], [1, 3, 4])),
                  (3, ([Shape' 4 2, Shape' 3 1], [1, 2])),
                  (4, ([Shape' 2 2], [2, 5])),
                  (5, ([Shape' 1 2, Shape' 2 3], [1, 4]))
                ]
        result = evalState (computeCost pe (0.5 :: Lambda)) problem
        expected = Cost 34.5
    result `shouldBe` expected
