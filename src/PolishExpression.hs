{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module PolishExpression where

-----------------------------------------------------

import Control.Monad (foldM)
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Coerce
import qualified System.Random.MWC as Random


-----------------------------------------------------

type ModuleIndex = Int

-- | H means a slice in the horizontal plane i.e. elements must be placed vertical.
--
-- V: * (original notation)
-- H: + (original notation)
data Operator = V | H
  deriving stock (Show, Eq)

data Alphabet
  = Operand ModuleIndex
  | Operator Operator

isOperand :: Alphabet -> Bool
isOperand (Operand _) = True
isOperand (Operator _) = False

newtype PolishExpression = PolishExpression { _pe :: [Alphabet]}

-- | Produces the initial polish expression of the form 12*3*...*n*
--
-- 'Int' parameter is the number of modules/blocks.
initialPE :: Int -> Maybe PolishExpression
initialPE n
  | n < 3 = Nothing
  | otherwise =
    let op = (Operator V)
        operators = fmap Operand [2 ..]
     in Just . PolishExpression $
          (Operand 1 : take (n -2) (List.intersperse op operators)) ++ [op]

-- | Perturbate a polish expression randomly applying one of the three valid moves.
perturbate :: Random.GenIO -> PolishExpression -> IO PolishExpression
perturbate gen pe = do
  n <- Random.uniformRM (1 :: Integer, 3 :: Integer) gen
  case n of
    1 -> move1
    2 -> move2
    3 -> move3
    _ -> error "Random value should be in the range [1,3]"
  where
    sizes =
      let nOperands = length . filter isOperand $ coerce pe
          nOperators = (length (coerce @_ @[Alphabet] pe)) - nOperands
      in (nOperands, nOperators)

    -- Swap two adjacent operands
    move1 :: IO PolishExpression
    move1 = do
      let (nOperands, _) = sizes
      index <- Random.uniformRM (0, nOperands - 2) gen
      return . coerce $ go index (coerce pe)
        where
          go _ [] = error "unexpected index"
          go 0 (x@(Operator _):xs) = x : go 0 xs
          go 0 (x:xs) =
            let xs' = takeWhile (not . isOperand) xs
                x' = head xs'
             in x' : xs' ++ [x] ++ (tail xs')
          go n (x:xs) = x : go (if isOperand x then n - 1 else n) xs

    -- Complement some chain of operators
    move2 :: IO PolishExpression
    move2 = do
      let (_, nOperators) = sizes
      start <- Random.uniformRM (0, nOperators - 1) gen
      n <- Random.uniformRM (0, nOperators - start - 1) gen
      return . coerce $ go start n (coerce pe)
        where
          go _ _ [] = error "unexpected index"
          go 0 n (x@(Operand _):xs) = x : go 0 n xs
          go 0 n (x:xs) =
            let (xs', rest) = takeOperators n xs
             in x : ((fmap complement xs') ++ rest)
          go s n (x:xs) = x : go (if isOperand x then s else s - 1) n xs

    -- Swap a pair of adjacent operands and operators
    move3 :: IO PolishExpression
    move3 = move3' 0
      where
        move3' :: Int -> IO PolishExpression
        move3' 1000 = error "move3 is looping!"
        move3' it = do
          let len = length $ coerce @_ @[Alphabet] pe
          i <- Random.uniformRM (0, len - 2) gen
          let (xs, (x : y : ys)) = splitAt @Alphabet (i - 1) (coerce pe)
              pe' = coerce (xs ++ (y : x : ys))
          case validate pe' of
            Left _  -> move3' (it + 1)
            Right validPE -> return validPE

-- | Returns 'm' characters of the alphabet.
-- 'm' = n operators + n' operands < 2n-1
takeOperators :: Int -> [Alphabet] -> ([Alphabet], [Alphabet])
takeOperators _ [] = ([], [])
takeOperators 0 xs = ([], xs)
takeOperators n (x:xs) =
  let (xs', rest) = takeOperators (if isOperand x then n else n - 1) xs
   in (x:xs', rest)

-- | Complements a chain.
complement :: Alphabet -> Alphabet
complement x@(Operand _) = x
complement (Operator H) = Operator V
complement (Operator V) = Operator H

-- Donat dos index relatius, fer swap

-- | Validate a polish expression, usually after a move.
--
-- For some reason, they allow not normalized polish expressions (the P_0 is an example)
validate :: PolishExpression -> Either String PolishExpression
validate polishExpression =
  if
      | propII polishExpression -> Right polishExpression
      | otherwise -> Left "Property II violated."
  where
    propII :: PolishExpression -> Bool
    propII (PolishExpression v) =
      isJust $
        foldM
          ( \acc e ->
              if isOperand e
                then Just (acc + 1)
                else
                  if acc - 1 > 0
                    then Just (acc - 1)
                    else Nothing
          )
          (0 :: Int)
          v
    -- Not neeeded
    -- propIII :: PolishExpression -> Bool
    -- propIII =
    --   let operators = mapMaybe onlyOperators
    --       maxConsecutives = List.maximum . fmap length . List.group
    --    in (< 2) . maxConsecutives . operators . coerce
    --   where
    --     onlyOperators :: Alphabet -> Maybe Operator
    --     onlyOperators (Operator op) = Just op
    --     onlyOperators (Operand _) = Nothing
