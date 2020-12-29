{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Problem (
    problemSpec
  )where

import Floorplan.Problem
import Floorplan.Types
import Test.Hspec
import System.Directory
import Data.Foldable
import Test.Hspec.Megaparsec
import qualified Data.Text.IO as Text

inputDir :: FilePath
inputDir = "test/resources/"

successfulDir :: FilePath
successfulDir = inputDir <> "successful/"

failureDir :: FilePath
failureDir = inputDir <> "failure/"

problemSpec :: Spec
problemSpec =
  describe "Problem Tests" $ do
    parsingSpec

parsingSpec :: Spec
parsingSpec = describe "Parsing" $ do
  it "should parse sucessful/example_0" $ do
    let Right expected =
          mkProblem $ [ (1, ([Shape' 1 4], [2,3,4,5]))
                      , (2, ([Shape' 2 2], [1,3,5]))
                      , (3, ([Shape' 3 1], [1,2]))
                      , (4, ([Shape' 1 2], [1,5]))
                      , (5, ([Shape' 1 1], [1,2,4]))
                      ]
    r <- parseProblemFile (inputDir <> "example_0")
    case r of
      Left err -> expectationFailure err
      Right result -> do
        result `shouldBe` expected

  it "should parse all examples from sucessful/" $ do
    files <- listDirectory successfulDir
    for_ files $ \fp -> do
      content <- Text.readFile (successfulDir <> fp)
      parse problemParser "" `shouldSucceedOn` content

  it "should fail parsing all examples from failure/" $ do
    files <- listDirectory failureDir
    for_ files $ \fp -> do
      content <- Text.readFile (failureDir <> fp)
      parse problemParser "" `shouldFailOn` content
