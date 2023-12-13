module Day13 where

import Data.List.Extra (split, transpose)

mirrorAxis :: (Int -> Bool) -> [String] -> Integer
mirrorAxis p xs = fromIntegral $ case horizontal p xs 1 of
  Just x -> 100 * x
  Nothing -> case horizontal p (transpose xs) 1 of
    Just x -> x
    Nothing -> error "no solution"

horizontal :: (Int -> Bool) -> [String] -> Int -> Maybe Int
horizontal p xs = go
  where
    maxX = length xs
    go n  | n >= maxX = Nothing
          | p $ sum $ zipWith diff (reverse $ take n xs) (drop n xs)
                      = Just n
          | otherwise = go (n + 1)

diff :: String -> String -> Int
diff xs = sum . zipWith (\a b -> if a == b then 0 else 1) xs

day13 :: IO ()
day13 = do
  input <- readFile "input/day13"
  let lin = lines input
  let splitted = split null lin
  putStr "Part 1: "
  print $ sum $ map (mirrorAxis (==0)) splitted
  putStr "Part 2: "
  print $ sum $ map (mirrorAxis (==1)) splitted
