{-# LANGUAGE RecordWildCards #-}

module Main where

import Floorplan
import Options.Applicative
import System.IO

------------------------------------

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (paramsParser <**> helper) fullDesc

------------------------------------

data Parameters = Parameters
  { mode :: Mode
  , aspectRatio :: Interval AspectRatio
  , lambda :: Lambda
  , coolingRate :: CoolingRate
  , gamma :: Gamma
  }
  deriving (Show)

paramsParser :: Parser Parameters
paramsParser =
  Parameters <$> modeParser
             <*> aspectRatioParser
             <*> lambdaParser
             <*> coolingRateParser
             <*> gammaParser

modeParser :: Parser Mode
modeParser = productionParser <|> demoParser
  where
    productionParser =
      Production <$> fileParser "Input file"
    demoParser =
      Demo <$>
        option
          auto
          ( long "demo"
              <> help "Run demo with the given number of modules"
              <> metavar "INTEGER"
          )

aspectRatioParser :: Parser (Interval AspectRatio)
aspectRatioParser =
  option
    auto
    ( long "aspect-ratio"
        <> value def
        <> showDefault
        <> help "Aspect Ratio Constraint"
        <> metavar "(DOUBLE, DOUBLE)"
    )
  where
    Just def = mkInterval ((AspectRatio 0.5), (AspectRatio 2.0))

lambdaParser :: Parser Lambda
lambdaParser =
  option
    auto
    ( long "lambda"
        <> value def
        <> showDefault
        <> help "Lambda Parameter: area + lambda*wirelength"
        <> metavar "[0.0,1.0]"
    )
  where
    Just def = mkLambda 0.5

coolingRateParser :: Parser CoolingRate
coolingRateParser =
  option
    auto
    ( long "cooling-rate"
        <> value defaultCoolingRate
        <> showDefault
        <> help "Cooling Rate Parameter: simulated annealing"
        <> metavar "(0.0,1.0)"
    )

gammaParser :: Parser Gamma
gammaParser =
  option
    auto
    ( long "gamma"
        <> value defaultGamma
        <> showDefault
        <> help "Gamma Parameter: simulated annealing"
        <> metavar "(1,100)"
    )

fileParser :: String -> Parser FilePath
fileParser helpMsg =
  strOption
    ( long "file"
        <> short 'f'
        <> help helpMsg
        <> metavar "FILE"
    )

-------------------------------------------------------------

run :: Parameters -> IO ()
run params@Parameters{..} = do
  setOutputEncoding
  problem <- getProblem mode
  floorplan <- simulatedAnnealing (toConfig params problem)
  printReport $ mkReport floorplan
    where
      setOutputEncoding = do
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

      getProblem :: Mode -> IO Problem
      getProblem (Demo n) = do
        r <- genProblem n
        case r of
          Left err -> error err
          Right r -> return r
      getProblem (Production fp) = parseProblemFile' fp

      toConfig :: Parameters -> Problem -> Configuration
      toConfig Parameters{..} problem =
        Configuration
          { _cProblem = problem,
            _cAspectRatio = aspectRatio,
            _cLambda = lambda,
            _cCoolingRate = coolingRate,
            _cGamma = gamma,
            _cMode = mode
          }
