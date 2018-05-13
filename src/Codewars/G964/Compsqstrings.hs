{-# LANGUAGE ParallelListComp #-}
module Codewars.G964.Compsqstrings where

import Data.List (intersperse)

compose :: String -> String -> String
compose s1 s2 =
  let src1 = parts s1
      src2 = reverse $ parts s2
  in
    foldl1 (++) (intersperse "\n" [a ++ b | a <- src1 | b <- src2])
  where
    parts ls = [take (i + 1) l | i <- (nseq 0) | l <- lines ls]
    nseq :: Int -> [Int]
    nseq n = n : nseq (n + 1)
