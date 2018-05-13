module Fusc where

fusc :: Int -> Int
fusc 0 = 0
fusc 1 = 1

fusc n
  n | n `mod` 2 == 0 = fusc (n / 2)
    
  
