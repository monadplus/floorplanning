module Test.Pretty where

import qualified Data.IntMap.Strict as Map
import qualified Data.Matrix as Matrix
import Floorplan.Pretty
import Floorplan.SimulatedAnnealing
import Test.Hspec

prettySpec :: Spec
prettySpec = do
  describe "Pretty Tests" $ do
    it "should print a floorplan in matrix form" $ do
      {-

             0        5   7    10
          8  +--------+---+-----+
             |        | 3 |     |
          5  +---+----|   |     |
             | 1 | 2  +---+-----| 3
             |   |    |    4    |
          0  +---+----+---------+
             0   2    5         10

      -}
      let floorplan =
            Floorplan $
              Map.fromList
                [ (1, BoundingBox' 0 0 2 5),
                  (2, BoundingBox' 2 0 5 5),
                  (3, BoundingBox' 5 3 7 8),
                  (4, BoundingBox' 5 0 10 3)
                ]
          expectedMatrix :: Matrix.Matrix ModuleIndex
          expectedMatrix =
            Matrix.fromLists
              [ [0, 0, 0, 0, 0, 3, 3, 0, 0, 0],
                [0, 0, 0, 0, 0, 3, 3, 0, 0, 0],
                [0, 0, 0, 0, 0, 3, 3, 0, 0, 0],
                [1, 1, 2, 2, 2, 3, 3, 0, 0, 0],
                [1, 1, 2, 2, 2, 3, 3, 0, 0, 0],
                [1, 1, 2, 2, 2, 4, 4, 4, 4, 4],
                [1, 1, 2, 2, 2, 4, 4, 4, 4, 4],
                [1, 1, 2, 2, 2, 4, 4, 4, 4, 4]
              ]

      pretty floorplan `shouldBe` Matrix.prettyMatrix expectedMatrix

    it "should print another floorplan in matrix form" $ do
      {-

                          4
          5  +------+-----+-----+
             |      |  2  |  3  |
             |      +-----+-----+ 3
             |  1   |           |
             |      |           |
          0  +------+-----------+
             0      2           6

      -}
      let floorplan =
            Floorplan $
              Map.fromList
                [ (1, BoundingBox' 0 0 2 5),
                  (2, BoundingBox' 2 3 4 5),
                  (3, BoundingBox' 4 3 6 5)
                ]
          expectedMatrix :: Matrix.Matrix ModuleIndex
          expectedMatrix =
            Matrix.fromLists
              [ [1, 1, 2, 2, 3, 3]
              , [1, 1, 2, 2, 3, 3]
              , [1, 1, 0, 0, 0, 0]
              , [1, 1, 0, 0, 0, 0]
              , [1, 1, 0, 0, 0, 0]
              ]
      pretty floorplan `shouldBe` Matrix.prettyMatrix expectedMatrix
