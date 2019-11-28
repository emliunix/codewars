module Fusc where

  fusc :: Integer -> Integer
  fusc i
      | i == 0 = 0
      | i == 1 = 1
      | otherwise = theF 1 0 i
      where
          theF a b i
              | i == 0 = b
              | i `mod` 2 == 0 = theF (a + b) b (i `div` 2)
              | i `mod` 2 == 1 = theF a (a + b) (i `div` 2)
  
  -- >>> (fusc 0, 0)
  -- (0,0)
  --
  -- >>> (fusc 1, 1)
  -- (1,1)
  --
  -- >>> (fusc ((2 ^ 1000) + 1), 1001)
  -- (1001,1001)
  --
  -- >>> (fusc ((2 ^ 1000) + (-1)), 1000)
  -- (1000,1000)
  --
  -- >>> (fusc ((2 ^ 1000) + 5), 2996)
  -- (2996,2996)
  --
  -- >>> (fusc ((2 ^ 1000) + 21), 7973)
  -- (7973,7973)
  --
  