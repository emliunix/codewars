module Codewars.G964.Seven where

seven :: Integer -> (Integer, Int)
seven m =
  helper m 0
  where
    helper m n
      | m < 100 = (m, n)
      | otherwise =
        let (a,b) = parts m in
          helper (a - 2 * b) (n + 1)
    parts m = (m `div` 10, m `mod` 10)
  
