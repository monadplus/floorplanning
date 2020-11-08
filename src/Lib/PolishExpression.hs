{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Lib.PolishExpression
  ( module Lib.PolishExpression,
    Random.createSystemRandom,
  )
where

-----------------------------------------------------

import Control.Applicative
import Control.Exception (AssertionFailed (..), throwIO)
import Control.Monad (foldM, when)
import Data.Coerce
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified System.Random.MWC as Random
import Text.Printf
import Text.Read (readMaybe)
import Control.Monad.IO.Class

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
  deriving stock (Show, Eq)

newtype PolishExpression = PolishExpression {_pe :: [Alphabet]}
  deriving newtype (Show, Eq)

-- | Produces the initial polish expression of the form 12*3*...*n*
--
-- 'Int' parameter is the number of modules/blocks.
initialPE :: Int -> Maybe PolishExpression
initialPE n
  | n < 2 = Nothing
  | otherwise =
    let operator = (Operator V)
        operands = take (n -1) $ fmap Operand [2 ..]
        pe = PolishExpression $ (Operand 1 : (List.intersperse operator operands)) ++ [operator]
     in case validate pe of
          Left _ -> Nothing
          Right _ -> Just pe

-- | Perturbate a polish expression randomly applying one of the three valid moves.
perturbate :: (MonadIO m) => Random.GenIO -> PolishExpression -> m PolishExpression
perturbate gen pe = liftIO $ do
  go =<< Random.uniformRM (1, 3) gen
  where
    go :: Int -> IO PolishExpression
    go = \case
      1 -> move1 gen pe
      2 -> move2 gen pe
      3 -> do
        r <- move3 gen pe
        case r of -- let's assume movement 3 is not valid for any pair of operand/operator
          Nothing -> go =<< Random.uniformRM (1, 2) gen
          Just pe' -> return pe'
      _ -> error "Random value should be in the range [1,3]"

perturbateN :: (MonadIO m) => Int -> PolishExpression -> m PolishExpression
perturbateN n pe = do
  when (n < 0) $ (liftIO . throwIO) (AssertionFailed "Expected a positive number.")
  gen <- liftIO $ Random.createSystemRandom
  go n gen pe
  where
    go 0 gen pe = return pe
    go n gen pe = do
      pe' <- perturbate gen pe
      go (n - 1) gen pe'

-- | Swap two adjacent operands
move1 :: (MonadIO m) => Random.GenIO -> PolishExpression -> m PolishExpression
move1 gen pe = do
  let (nOperands, _) = sizes pe
  i <- liftIO $ Random.uniformRM (0, nOperands - 2) gen
  return (move1' i pe)

-- | Swap operands i and i+1
move1' :: Int -> PolishExpression -> PolishExpression
move1' i pe = go i `over` pe
  where
    go _ [] = error "unexpected index"
    go 0 (x@(Operator _) : xs) = x : go 0 xs
    go 0 (e1 : xs) =
      let (xs', (e2 : rest)) = span isOperator xs -- OK by construction
       in e2 : (xs' ++ (e1 : rest))
    go n (x : xs) = x : go (if isOperand x then n - 1 else n) xs

-- | Complement some random chain of operators
move2 :: (MonadIO m) => Random.GenIO -> PolishExpression -> m PolishExpression
move2 gen pe = liftIO $ do
  let (_, nOperators) = sizes pe
  i <- Random.uniformRM (0, nOperators - 1) gen
  chainLength <- Random.uniformRM (0, nOperators - i - 1) gen
  return (move2' i chainLength pe)

-- | Complement the chain of operators of size 'chainLength' starting at 'i'
move2' ::
  -- | Start of the chain
  Int ->
  -- | Lenght of the chain
  Int ->
  PolishExpression ->
  PolishExpression
move2' i chainLength pe = go i chainLength `over` pe
  where
    go _ _ [] = error "unexpected index"
    go 0 n (x@(Operand _) : xs) = x : go 0 n xs
    go 0 n (x : xs) =
      let (xs', rest) = takeOperators n xs
       in fmap complement (x : xs') ++ rest
    go s n (x : xs) = x : go (if isOperand x then s else s - 1) n xs

    complement :: Alphabet -> Alphabet
    complement x@(Operand _) = x
    complement (Operator H) = Operator V
    complement (Operator V) = Operator H

-- | Swap a pair of adjacent operands and operators
--
-- It may happen that after one iteration the polish expression is the same:
-- * If none of the swaps is picked
-- * If only the invalid swaps are picked
-- * If none of the swaps are valid
move3 :: (MonadIO m) => Random.GenIO -> PolishExpression -> m (Maybe PolishExpression)
move3 gen pe = liftIO $ do
  let len = length `applyTo` pe
  move3' len 10
  where
    move3' :: Int -> Int -> IO (Maybe PolishExpression)
    move3' _ 0 = return Nothing -- We failed n times, let's assume no valid swap is possible.
    move3' len n = do
      r <- trySwap (coerce pe)
      case r of
        Nothing -> move3' len (n - 1)
        Just xs -> return . return $ PolishExpression xs
      where
        trySwap :: [Alphabet] -> IO (Maybe [Alphabet])
        trySwap = trySwap' 0

        trySwap' :: Int -> [Alphabet] -> IO (Maybe [Alphabet])
        trySwap' _ [] = return Nothing
        trySwap' _ (_ : []) = return Nothing
        trySwap' count xs@((Operator _) : (Operand _) : _) = doWithProb count xs
        trySwap' count xs@((Operand _) : (Operator _) : _)
          | count > 1 = doWithProb count xs
          | otherwise = next count xs
        trySwap' count xs = next count xs

        doWithProb count xs@(e1 : e2 : rest) = do
          r <- Random.uniformRM (0, len -1) gen
          if r < 2
            then return $ Just (e2 : e1 : rest)
            else next count xs

        next count (e1@(Operator _) : e2@(Operator _) : rest) = fmap ([e1, e2] ++) <$> trySwap' (count - 2) rest
        next count (e1@(Operand _) : e2@(Operand _) : rest) = fmap ([e1, e2] ++) <$> trySwap' (count + 2) rest
        next count (e1 : e2 : rest) = fmap ([e1, e2] ++) <$> trySwap' count rest
        next _ _ = error "Check next in move3."

-- | Returns 'm' characters of the alphabet.
-- 'm' = n operators + n' operands < 2n-1
takeOperators :: Int -> [Alphabet] -> ([Alphabet], [Alphabet])
takeOperators _ [] = ([], [])
takeOperators 0 xs = ([], xs)
takeOperators n (x : xs) =
  let (xs', rest) = takeOperators (if isOperand x then n else n - 1) xs
   in (x : xs', rest)

-- Donat dos index relatius, fer swap

-- | Validate a polish expression, usually after a move.
--
-- For some reason, they allow not normalized polish expressions (the P_0 is an example)
validate :: PolishExpression -> Either String PolishExpression
validate polishExpression =
  if propI polishExpression
    then
      if propII polishExpression
        then Right polishExpression
        else Left "Property (ii) violated: balloting property"
    else Left "Property (i) violated: #operators = n - 1"
  where
    propI :: PolishExpression -> Bool
    propI (PolishExpression v) =
      let nOperands = length . filter isOperand $ v
          nOperators = length . filter isOperator $ v
       in nOperators == nOperands - 1

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

---------------------------------------------------------
-- Parsing and Quasiquoting

-- TODO quasi-quoting for polish expressions

parsePolishExpression :: String -> Either String PolishExpression
parsePolishExpression str = do
  pe <- parse str
  validate pe
  where
    parse :: String -> Either String PolishExpression
    parse str = coerce $ parse' str []
      where
        parse' :: String -> [Alphabet] -> Either String [Alphabet]
        parse' [] acc = Right (reverse acc)
        parse' (c : cs) acc = do
          let err = Left (printf "Expecting an operand/operator but %c found." c)
          c' <- parseOperand c <|> parseOperator c <|> err
          parse' cs (c' : acc)

parsePolishExpression' :: String -> PolishExpression
parsePolishExpression' str =
  case parsePolishExpression str of
    Left err -> error err
    Right pe -> pe

parseOperand :: Char -> Either String Alphabet
parseOperand c =
  case readMaybe @Int [c] of
    Nothing -> Left (printf "Expecting an operand but %c found." c)
    Just int -> Right (Operand int)

parseOperator :: Char -> Either String Alphabet
parseOperator c =
  case c of
    '*' -> Right (Operator V)
    'V' -> Right (Operator V)
    '+' -> Right (Operator H)
    'H' -> Right (Operator H)
    _ -> Left (printf "Expecting an operator but %c found." c)

---------------------------------------------------------
-- Auxiliary Functions

isOperand :: Alphabet -> Bool
isOperand (Operand _) = True
isOperand (Operator _) = False

isOperator :: Alphabet -> Bool
isOperator (Operand _) = False
isOperator (Operator _) = True

-- | (#Operands, #Operators)
sizes :: PolishExpression -> (Int, Int)
sizes pe =
  let nOperands = (length . filter isOperand) `applyTo` pe
      nOperators = (length `applyTo` pe) - nOperands
   in (nOperands, nOperators)

applyTo :: ([Alphabet] -> r) -> PolishExpression -> r
applyTo f = f . coerce

over :: ([Alphabet] -> [Alphabet]) -> PolishExpression -> PolishExpression
over f = coerce . f . coerce
