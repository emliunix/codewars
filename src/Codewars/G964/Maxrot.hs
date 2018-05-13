module Codewars.G964.Maxrot where

import Text.Read (readMaybe)

maxRot :: Integer -> Integer
maxRot n =
  toInt $ maxStr $ show n
  where
    maxStr x = case x of
      (a:b:xs) | a > b -> x
      (a:b:xs) -> let subMax = b : (maxStr (xs ++ [a])) in
        if a == b then max x subMax else subMax
      otherwise -> x
    toInt s = case readMaybe s :: Maybe Integer of
      Just i -> i
      _ -> 0

rots :: Integer -> [Integer]
rots n = fmap toInt $ rotstrs "" $ show n
  where
    rotstrs pre x = case x of
      (a:b:xs) -> (pre ++ x) : (rotstrs (pre ++ [b]) (xs ++ [a]))
      otherwise -> [pre ++ x]
    toInt s = case readMaybe s :: Maybe Integer of
      Just i -> i
      _ -> 0
