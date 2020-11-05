module Main (main) where

import           System.IO  (hSetEncoding, stderr, stdout, utf8)
import           Test.Hspec
import           Lib

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hspec allUnitTests

allUnitTests :: Spec
allUnitTests = describe "floorplanning tests" $ do
  it "computeArea" $ do
    let block1 = DBlock (Width 1) (Height 1)
        block2 = DBlock (Width 2) (Height 2)
        block3 = DBlock (Width 3) (Height 3)
        slicingFP = Node (Node (Leaf block1) V (Leaf block2)) H (Leaf block3)
    computeArea slicingFP `shouldBe` Area (3,5)
