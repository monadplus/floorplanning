{-# LANGUAGE AllowAmbiguousTypes #-}
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

    -- * Lenses
    pproblem,
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

newtype Problem = Problem
  { _pproblem :: IntMap ([Shape], [ModuleIndex])
  }

makeLenses ''Problem

problemSize :: Problem -> Int
problemSize = length . Map.keys . _pproblem

-- | Properties of a valid problem:
-- * Modules numbered from 1 to n
-- * One shape per module
-- * Modules connected to existent modules.
mkProblem :: IntMap ([Shape], [ModuleIndex]) -> Either String Problem
mkProblem problem = do
  let modules = Map.keys problem
      n = List.length modules
      (listOfShapes, listOfConnections) = unzip (Map.elems problem)
  unless (maximum modules == n) $ Left "Modules numbered from 1 to n."
  when (any null listOfShapes) $ Left "At least one shape per module."
  forM_ listOfConnections $ \connections -> do
    when (any (> n) connections) $ Left "Module does not exist."
    unless (length (List.nub connections) == List.length connections) $ Left "Connection repeated."
  return $ Problem problem

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

        genShape :: MonadIO m => (Int, Int) -> m Shape
        genShape limits = do
          x <- liftIO $ Random.uniformRM limits g
          y <- liftIO $ Random.uniformRM limits g
          return (Shape' x y)

        genConnections :: MonadIO m => m (IntMap [ModuleIndex])
        genConnections = do
          wires <- traverse (\i -> (i,) <$> genWires i) [1 .. n]
          return $ foldl' go (Map.fromList wires) wires
          where
            go :: IntMap [ModuleIndex] -> (ModuleIndex, [ModuleIndex]) -> IntMap [ModuleIndex]
            go acc (i, is) = foldl' (\acc' i' -> Map.adjust (i :) i' acc') acc is

        genWires :: MonadIO m => Int -> m [ModuleIndex]
        genWires moduleIndex = filterM (const p) [moduleIndex + 1 .. n]
          where
            p = return . (< (0.25 :: Double)) =<< (liftIO $ Random.uniform g)
