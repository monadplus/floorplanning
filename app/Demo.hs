module Demo where

import Floorplan
import System.IO

runDemo :: IO ()
runDemo = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  let Just aspectRatioInterval = mkInterval ((AspectRatio 0.5), (AspectRatio 2.0))
      Just lambda = mkLambda 0.5

  Right problem <- genProblem 30

  let config =
        Config
          { _cProblem = problem,
            _cAspectRatio = aspectRatioInterval,
            _cLambda = lambda,
            _cCoolingRate = defaultCoolingRate,
            _cGamma = defaultGamma,
            _cMode = Demo
          }

  _ <- simulatedAnnealing config
  return ()
