{-# LANGUAGE TypeApplications #-}

module Test.PolishExpression where

import Data.Either
import Floorplan.PolishExpression
import Test.Hspec
import System.Random.MWC

polishExpressionSpec :: Spec
polishExpressionSpec =
  describe "Polish Expression Tests" $ do
    perturbateSpec
    moveSpec
    parseSpec
    utilSpec

perturbateSpec :: Spec
perturbateSpec = describe "Perturbate" $ do
  it "should perturbate a polish expression many times without throwing" $ do
    let Just pe = initialPE 100
    pe' <- perturbateN 1000 pe
    validate pe' `shouldSatisfy` isRight

-- TODO moves should be tested with property-based testing
moveSpec :: Spec
moveSpec = describe "Move" $ do
  describe "M1" $ do
    it "should swap to adjacents operands" $ do
      let initial = parsePolishExpression' "12*3*4*5*"
          final = parsePolishExpression' "13*2*4*5*"
      move1' 1 initial `shouldBe` final

    it "should swap to adjacents operands (2)" $ do
      let initial = parsePolishExpression' "12*3*4*5*"
          final = parsePolishExpression' "21*3*4*5*"
      move1' 0 initial `shouldBe` final

    it "should swap to adjacents operands (3)" $ do
      let initial = parsePolishExpression' "12*3*4*5*"
          final = parsePolishExpression' "12*3*5*4*"
      move1' 3 initial `shouldBe` final

  describe "M2" $ do
    it "should swap to adjacents operands" $ do
      let initial = parsePolishExpression' "12*3*4*5*"
          final = parsePolishExpression' "12+3*4*5*"
          r = move2' 0 0 initial
      r `shouldBe` final

    it "should swap to adjacents operands (2)" $ do
      let initial = parsePolishExpression' "12*3*4*5*"
          final = parsePolishExpression' "12+3+4+5+"
          r = move2' 0 3 initial
      r `shouldBe` final

    it "should swap to adjacents operands (3)" $ do
      let initial = parsePolishExpression' "12*3*4*5*"
          final = parsePolishExpression' "12*3+4+5+"
          r = move2' 1 2 initial
      r `shouldBe` final

    it "should swap to adjacents operands (4)" $ do
      let initial = parsePolishExpression' "12*3*4*5*"
          final = parsePolishExpression' "12*3*4*5+"
          r = move2' 3 0 initial
      r `shouldBe` final

  describe "M3" $ do
    it "should swap a pair of adjacent operands and operators" $ do
      let initial = parsePolishExpression' "12+3*"
          final = parsePolishExpression' "123+*" -- only valid permutation
      gen <- createSystemRandom
      r <- move3 gen initial
      case r of
        Nothing -> return ()
        Just pe -> pe `shouldBe` final

-- TODO this should be a property-based test
parseSpec :: Spec
parseSpec = describe "parse" $ do
  it "should parse a normalized polish expression" $ do
    case parsePolishExpression "12*3*" of
      Left err -> expectationFailure err
      Right pe -> pe `shouldBe` PolishExpression [Operand 1, Operand 2, Operator V, Operand 3, Operator V]

  it "should parse another normalized polish expression" $ do
    case parsePolishExpression "123*+4+5*" of
      Left err -> expectationFailure err
      Right pe -> do
        let expected =
              PolishExpression
                [ Operand 1,
                  Operand 2,
                  Operand 3,
                  Operator V,
                  Operator H,
                  Operand 4,
                  Operator H,
                  Operand 5,
                  Operator V
                ]
        pe `shouldBe` expected

  it "should fail parsing an invalid character" $ do
    case parsePolishExpression "123T4" of
      Left err -> err `shouldBe` "Expecting an operand/operator but T found."
      Right _ -> expectationFailure "Expecting the parse to fail on character T."

  it "should fail parsing a non-balloting sequence" $ do
    case parsePolishExpression "12*+4+5" of
      Left err -> err `shouldBe` "Property (ii) violated: balloting property"
      Right _ -> expectationFailure "Expecting the parse to fail."

utilSpec :: Spec
utilSpec = describe "Util" $ do
  it "takeOperators" $ do
    let pe = parsePolishExpression' "123*+4*56*+"
        (l, r) = takeOperators 3 `applyTo` pe -- "123*+4*" "56*+"
    l `shouldBe` [Operand 1, Operand 2, Operand 3, Operator V, Operator H, Operand 4, Operator V]
    r `shouldBe` [Operand 5, Operand 6, Operator V, Operator H]
