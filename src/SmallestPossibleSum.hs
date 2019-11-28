module SmallestPossibleSum where

import Data.List (sort, length)

smallestPossibleSum :: (Integral a) => [a] -> a
smallestPossibleSum l' =
    fromInteger $ (foldl1 gcd l) * toInteger (length l')
    where
        l = map toInteger l'

-- gcd :: Integer -> Integer -> Integer
-- gcd a b
--     | a == b = a
--     | a > b = gcd a (a `mod` b)
--     | a < b = gcd a (b `mod` a)

-- >>> (smallestPossibleSum [6,9,21], 9)
-- (9,9)
--
-- >>> ( smallestPossibleSum [1,21,55], 3)
-- (3,3)
--
-- >>> ( smallestPossibleSum [3,13,23,7,83], 5)
-- (5,5)
--
-- >>> ( smallestPossibleSum [4,16,24], 12)
-- (12,12)
--
-- >>> ( smallestPossibleSum [30,12], 12)
-- (12,12)
--
-- >>> ( smallestPossibleSum [60,12,96,48,60,24,72,36,72,72,48], 132)
-- (132,132)
--
-- >>> ( smallestPossibleSum [71,71,71,71,71,71,71,71,71,71,71,71,71], 923)
-- (923,923)
--
-- >>> ( smallestPossibleSum [11,22], 22)
-- (22,22)
--
-- >>> ( smallestPossibleSum [9], 9)
-- (9,9)
--
