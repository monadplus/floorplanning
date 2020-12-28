{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Floorplan.PolishExpression
  ( -- * Polish Expression

    -- ** Data types
    Operator (..),
    Alphabet (..),
    PolishExpression (..),

    -- ** Functions
    parsePolishExpression,
    parsePolishExpression',
    validate,
    initialPE,
    perturbate,
    perturbateN,

    -- ** Utils
    takeOperators,
    applyTo,

    -- ** Internal
    move1,
    move1',
    move2,
    move2',
    move3,
  )
where

-----------------------------------------------------

import Control.Applicative
import Control.Exception (AssertionFailed (..), throwIO)
import Control.Monad (foldM, when)
import Control.Monad.IO.Class
import Data.Coerce
import qualified Data.List as List
import Data.Maybe (isJust)
import Floorplan.Types
import qualified System.Random.MWC as Random
import Text.Printf
import Text.Read (readMaybe)

-----------------------------------------------------

-- | H means a slice in the horizontal plane i.e. elements must be placed vertical.
--
-- V: * (original notation)
-- H: + (original notation)
data Operator = V | H
  deriving stock (Eq)

instance Show Operator where
  show V = "*"
  show H = "+"

data Alphabet
  = Operand ModuleIndex
  | Operator Operator
  deriving stock (Eq)

instance Show Alphabet where
  show (Operand i) = show i
  show (Operator op) = show op

{-|
Polish expression is sequence of elements from {1,2,...,n,*,+} with the following properties:

 * (i) Each block appears exactly once in the string an #operators = n - 1
 * (ii) Balloting property: forall positions in the string #operators < #operand
 * (iii) no consecutive operator of the same type: normality property.

A polish expression with no consecutive operators of the same type is called a __normalized polish expression__.

The traverse of a slicing tree in postorder gives a (proper) polish expression.

Theorem. 1-1 correspondence between the set of normalized polished expressions
of length 2n-1 and the set of skewed slicing trees with n leaves.
-}
newtype PolishExpression = PolishExpression {_pe :: [Alphabet]}
  deriving newtype (Eq)

instance Show PolishExpression where
  show (PolishExpression []) = ""
  show (PolishExpression (e : pe)) = List.foldl' go (show e) pe
    where
      go acc a = acc ++ (' ' : show a)

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
{-# INLINE initialPE #-}

-- | Perturbate a polish expression randomly applying one of the three valid moves.
--
-- Moves:
--  * M1: swap two adjacent operands
--  * M2: complementing (swap H and V) some chain of operators
--  * M3: swap a pair of adjacent operands and operators
--
-- In  case of M3, we need to validate properties (ii) and (iii).
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
{-# INLINE perturbate #-}


-- | Like 'perturbate' but n times
perturbateN :: (MonadIO m) => Int -> PolishExpression -> m PolishExpression
perturbateN n pe = do
  when (n < 0) $ (liftIO . throwIO) (AssertionFailed "Expected a positive number.")
  gen <- liftIO $ Random.createSystemRandom
  go n gen pe
  where
    go 0 _ pe = return pe
    go n gen pe = do
      pe' <- perturbate gen pe
      go (n - 1) gen pe'
    {-# INLINE go #-}
{-# INLINE perturbateN #-}

-- | Swap two adjacent operands
move1 :: (MonadIO m) => Random.GenIO -> PolishExpression -> m PolishExpression
move1 gen pe = do
  let (nOperands, _) = sizes pe
  i <- liftIO $ Random.uniformRM (0, nOperands - 2) gen
  return (move1' i pe)
{-# INLINE move1 #-}

-- | Swap operands i and i+1
move1' :: Int -> PolishExpression -> PolishExpression
move1' i pe = go i `over` pe
  where
    go _ [] = error "unexpected index"
    go 0 (x@(Operator _) : xs) = x : go 0 xs
    go 0 (e1 : xs) =
      let (xs', (e2 : rest)) = span isOperator xs
       in e2 : (xs' ++ (e1 : rest))
    go n (x : xs) = x : go (if isOperand x then n - 1 else n) xs
    {-# INLINE go #-}
{-# INLINE move1' #-}

-- | Complement some random chain of operators
move2 :: (MonadIO m) => Random.GenIO -> PolishExpression -> m PolishExpression
move2 gen pe = liftIO $ do
  let (_, nOperators) = sizes pe
  i <- Random.uniformRM (0, nOperators - 1) gen
  chainLength <- Random.uniformRM (0, nOperators - i - 1) gen
  return (move2' i chainLength pe)
{-# INLINE move2 #-}

-- | Complement the chain of operators of size _chainLength_ starting at _i_
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
    {-# INLINE go #-}

    complement :: Alphabet -> Alphabet
    complement x@(Operand _) = x
    complement (Operator H) = Operator V
    complement (Operator V) = Operator H
    {-# INLINE complement #-}
{-# INLINE move2' #-}

-- | Swap a pair of adjacent operands and operators
--
-- It may happen that after one iteration the polish expression is the same:
-- * If none of the swaps is picked
-- * If only the invalid swaps are picked
-- * If none of the swaps are valid
move3 :: (MonadIO m) => Random.GenIO -> PolishExpression -> m (Maybe PolishExpression)
move3 gen pe = liftIO $ do
  let len = length `applyTo` pe
  move3' len 10 -- #tries before failure
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
        {-# INLINE trySwap #-}

        trySwap' :: Int -> [Alphabet] -> IO (Maybe [Alphabet])
        trySwap' _ [] = return Nothing
        trySwap' _ (_ : []) = return Nothing
        trySwap' count xs@((Operator _) : (Operand _) : _) = doWithProb count xs
        trySwap' count xs@((Operand _) : (Operator _) : _)
          | count > 1 = doWithProb count xs
          | otherwise = next count xs
        trySwap' count xs = next count xs
        {-# INLINE trySwap' #-}

        doWithProb count xs@(e1 : e2 : rest) = do
          r <- Random.uniformRM (0, len -1) gen
          if r < 2
            then return $ Just (e2 : e1 : rest)
            else next count xs
        doWithProb _ _ = error "Check move3.doWithProb"
        {-# INLINE doWithProb #-}

        next count (e1@(Operator _) : e2@(Operator _) : rest) = fmap ([e1, e2] ++) <$> trySwap' (count - 2) rest
        next count (e1@(Operand _) : e2@(Operand _) : rest) = fmap ([e1, e2] ++) <$> trySwap' (count + 2) rest
        next count (e1 : e2 : rest) = fmap ([e1, e2] ++) <$> trySwap' count rest
        next _ _ = error "Check next in move3."
        {-# INLINE next #-}
    {-# INLINE move3' #-}
{-# INLINE move3 #-}

-- | Returns _m_ characters of the alphabet.
takeOperators :: Int -> [Alphabet] -> ([Alphabet], [Alphabet])
takeOperators _ [] = ([], [])
takeOperators 0 xs = ([], xs)
takeOperators n (x : xs) =
  let (xs', rest) = takeOperators (if isOperand x then n else n - 1) xs
   in (x : xs', rest)
{-# INLINE takeOperators #-}

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
    {-# INLINE propI #-}

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
    {-# INLINE propII #-}
{-# INLINE validate #-}

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

-- | Like 'parsePolishExpression' but throws.
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
{-# INLINE isOperand #-}

isOperator :: Alphabet -> Bool
isOperator (Operand _) = False
isOperator (Operator _) = True
{-# INLINE isOperator #-}

-- | (#Operands, #Operators)
sizes :: PolishExpression -> (Int, Int)
sizes pe =
  let nOperands = (length . filter isOperand) `applyTo` pe
      nOperators = (length `applyTo` pe) - nOperands
   in (nOperands, nOperators)
{-# INLINE sizes #-}

applyTo :: ([Alphabet] -> r) -> PolishExpression -> r
applyTo f = f . coerce
{-# INLINE applyTo #-}

over :: ([Alphabet] -> [Alphabet]) -> PolishExpression -> PolishExpression
over f = coerce . f . coerce
{-# INLINE over #-}
