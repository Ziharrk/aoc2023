module Day9 where

continueSequence :: [Integer] -> Integer
continueSequence xs = last xs +
  if all (== head diffs) diffs
    then head diffs
    else continueSequence diffs
  where
    diffs = getDiffs xs
    getDiffs (x:y:zs) = y - x : getDiffs (y:zs)
    getDiffs _ = []

day9 :: IO ()
day9 = do
  input <- map (map read . words) . lines <$> readFile "input/day9"
  putStr "Part 1: "
  print $ sum $ map continueSequence input
  putStr "Part 2: "
  print $ sum $ map (continueSequence . reverse) input
