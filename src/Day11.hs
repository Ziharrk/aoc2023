module Day11 where

import Data.List (transpose)
import Control.Arrow (first)

expand :: Integer -> String -> [([(Char, Integer)],Integer)]
expand expansion s = map (first (`zip`nR)) nL
  where
    l = lines s
    nL = numberLines 0 l
    nR = map snd $ numberLines 0 (transpose l)
    numberLines _ [] = []
    numberLines n (x:xs)
      | all (== '.') x = (x, n) : numberLines (n + expansion + 1) xs
      | otherwise = (x, n) : numberLines (n + 1) xs

galaxies :: [([(Char, Integer)],Integer)] -> [(Integer, Integer)]
galaxies = concatMap $ \(l, y) -> map ((,y) . snd) $ filter ((=='#') . fst) l

pairUp :: [(Integer, Integer)] -> [((Integer, Integer), (Integer, Integer))]
pairUp [] = []
pairUp (x:xs) = map (x,) xs ++ pairUp xs

distance :: ((Integer, Integer), (Integer, Integer)) -> Integer
distance ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

day11 :: IO ()
day11 = do
  input <- readFile "input/day11"
  let expanded1 = expand 1 input
      expanded1M = expand 999999 input
  let galaxyList1 = galaxies expanded1
      galaxyList1M = galaxies expanded1M
  let paired1 = pairUp galaxyList1
      paired1M = pairUp galaxyList1M
  putStr "Part 1: "
  print $ sum $ map distance paired1
  putStr "Part 2: "
  print $ sum $ map distance paired1M
