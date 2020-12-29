{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Floorplan.Problem
  ( Problem(..),
    problemSize,

    -- * Smart constructor
    mkProblem,

    -- * Generator
    genProblem,

    -- * Parsing
    problemParser,
    parseProblem,
    parseProblemFile,
    parseProblemFile',

    -- * Lenses
    pproblem,

    -- * Reexports
    parse
  )
where

import Control.Monad.State.Lazy
import Data.Functor
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List (foldl')
import qualified Data.List as List
import Floorplan.Types
import Lens.Micro.TH
import qualified System.Random.MWC as Random
import Prelude hiding (print)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Bifunctor
import Data.Coerce
import Lens.Micro
import Text.Printf

newtype Problem = Problem
  { _pproblem :: IntMap ([Shape], [ModuleIndex])
  }
  deriving newtype (Show, Eq)

makeLenses ''Problem

problemSize :: Problem -> Int
problemSize = length . Map.keys . _pproblem

-- | Properties of a valid problem:
--
-- * Modules numbered from 1 to n
-- * One shape per module
-- * Modules connected to existent modules.
--
-- Additionally, adds the rotated shape to the valid shapes of the module.
mkProblem :: IntMap ([Shape], [ModuleIndex]) -> Either String Problem
mkProblem = validate . addRotationToShapes
  where
    addRotationToShapes =
      fmap (over _1 (List.nub . concatMap (\x -> [x, rotate90degrees x])))
    {-# INLINE addRotationToShapes #-}

    validate problem = do
      let modules = Map.keys problem
          n = List.length modules
          (listOfShapes, listOfConnections) = unzip (Map.elems problem)
      unless (List.sort modules == [1..n]) $ Left "Error: modules must be numbered from 1 to n"
      when (any null listOfShapes) $ Left "Error: at least one shape per module"
      forM_ listOfConnections $ \connections -> do
        when (any (> n) connections) $ Left (printf "Error: one of the connections does not exist %s" (show connections))
        unless (length (List.nub connections) == List.length connections) $ Left (printf "Error: one of the connections is repeated %s" (show connections))
      return (Problem problem)
    {-# INLINE validate #-}
{-# INLINE mkProblem #-}

-- | Generates a problem of the given number of modules.
genProblem :: MonadIO m => Int -> m (Either String Problem)
genProblem n = genProblem' =<< liftIO Random.createSystemRandom
  where
    genProblem' :: MonadIO m => Random.GenIO -> m (Either String Problem)
    genProblem' g = do
      connections <- genConnections
      problem <- traverse (\conn -> (,conn) <$> genModuleShapes) connections
      return $ mkProblem problem
      where
        genModuleShapes :: MonadIO m => m [Shape]
        genModuleShapes = genShape (1, 5) <&> (\shape -> [shape, rotate90degrees shape])
        {-# INLINE genModuleShapes #-}

        genShape :: MonadIO m => (Int, Int) -> m Shape
        genShape limits = do
          x <- liftIO $ Random.uniformRM limits g
          y <- liftIO $ Random.uniformRM limits g
          return (Shape' x y)
        {-# INLINE genShape #-}

        genConnections :: MonadIO m => m (IntMap [ModuleIndex])
        genConnections = do
          wires <- traverse (\i -> (i,) <$> genWires i) [1 .. n]
          return $ foldl' go (Map.fromList wires) wires
          where
            go :: IntMap [ModuleIndex] -> (ModuleIndex, [ModuleIndex]) -> IntMap [ModuleIndex]
            go acc (i, is) = foldl' (\acc' i' -> Map.adjust (i :) i' acc') acc is
        {-# INLINE genConnections #-}

        genWires :: MonadIO m => Int -> m [ModuleIndex]
        genWires moduleIndex = filterM (const p) [moduleIndex + 1 .. n]
          where
            p = return . (< (0.25 :: Double)) =<< (liftIO $ Random.uniform g)
        {-# INLINE genWires #-}
    {-# INLINE genProblem' #-}
{-# INLINE genProblem #-}

---------------------------------------------------------------------

-- | Pure parser with no custom errors.
type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space
  hspace1
  (L.skipLineComment "#")
  empty -- block comment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

int :: Parser Int
int = lexeme L.decimal

-- decimal :: Parser Double
-- decimal = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

comma :: Parser ()
comma = void (symbol ",")

tuple :: Parser a -> Parser b -> Parser (a,b)
tuple p q = parens ((,) <$> p <*> (comma *> q))

list :: Parser a -> Parser [a]
list p = brackets (sepBy p comma)

-- | Parses a problem from a string.
--
-- Example of input:
-- @
-- 1   [(1,4)]   [2,3,4,5]
-- 2   [(2,2)]   [1,3,5]
-- 3   [(3,1)]   [1,2]
-- 4   [(1,2)]   [1,4]
-- 5   [(1,1)]   [1,2,5]
-- @
parseProblem :: Text -> Either String Problem
parseProblem str =
  mkProblem =<< first errorBundlePretty (parse problemParser "" str)
{-# INLINE parseProblem #-}

-- | After parsing, apply 'mkProblem' in order to get a 'Problem'.
problemParser :: Parser (IntMap ([Shape], [ModuleIndex]))
problemParser = Map.fromList <$> sepEndBy1 row newline
  where
    shape :: Parser Shape
    shape = coerce $ tuple int int

    row :: Parser (ModuleIndex, ([Shape], [ModuleIndex]))
    row = do
      i <- int
      shapes <- list shape
      connections <- list int
      return (i, (shapes, connections))
{-# INLINE problemParser #-}

parseProblemFile :: FilePath -> IO (Either String Problem)
parseProblemFile fp = do
  str <- Text.readFile fp
  return (parseProblem str)
{-# INLINE parseProblemFile #-}

parseProblemFile' :: FilePath -> IO Problem
parseProblemFile' fp = do
  r <- parseProblemFile fp
  case r of
    Left err -> error err
    Right x -> return x
{-# INLINE parseProblemFile' #-}
