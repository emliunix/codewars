module FormatDuration where

formatDuration :: (Integral i) => i -> String
formatDuration n =
  join $ filter (\(n,u) -> n /= 0) (zip (splitBy n [60, 60, 24, 365]) ["second", "minute", "hour", "day", "year"])
  where
  splitBy n (u:us)
    | n > 0 = n `mod` u : splitBy (n `div` u) us
    | otherwise = []
  splitBy n [] = [n]
  join (a:b:xs) = joinRest xs ++ (formatItem b) ++ " and " ++ (formatItem a)
  join (a:xs) = formatItem a
  join [] = "now"
  joinRest [] = ""
  joinRest xs = foldl1 (++) (fmap (\x -> (formatItem x) ++ ", ") $ reverse xs)
  formatItem (n,u) = (show $ toInteger n) ++ " " ++ u ++ (if n > 1 then "s" else "")
