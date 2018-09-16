module MaxSequence where

-- https://www.codewars.com/kata/maximum-subarray-sum/haskell

-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence seq =
  let search mx last xs =
        case xs of
          [] -> mx
          x:xs ->
            let val = last + x
                newval = if val < 0 then 0 else val
                newmx = max mx newval
            in
              search newmx newval xs
  in
    search 0 0 seq
