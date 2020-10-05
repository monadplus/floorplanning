module Lib.Internal (
    mystery
  ) where

-- $setup
-- >>> import Data.Monoid

-- |
-- >>> mystery 5
-- 5
--
-- >>> mystery 3
-- 0
mystery :: Int -> Int
mystery x = case x of
  5 -> 5
  _ -> 0
