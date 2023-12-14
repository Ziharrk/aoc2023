module Day14 where

import Data.List (transpose)
import qualified Data.Map as Map

calcLoad :: [String] -> Int
calcLoad = go 1 0 . reverse
  where
    go _ acc []     = acc
    go n acc (s:ss) = go (n+1) (acc + n * length (filter (== 'O') s)) ss

-- Rolls in direction: West
-- transpose first: North
-- map reverse first: East
-- transpose first and then map reverse: South
simulateRollingWest :: [String] -> [String]
simulateRollingWest = map column
  where
    column [] = []
    column ('#':s) = '#' : column s
    column s =
      let (untilCube, rest) = break (== '#') s
          numRocks = length $ filter (== 'O') untilCube
          numPlaces = length untilCube - numRocks
      in replicate numRocks 'O' ++ replicate numPlaces '.' ++ column rest

simulateRollingCycle :: [String] -> [String]
simulateRollingCycle =
  map reverse . transpose . map reverse .
  simulateRollingWest . map reverse . transpose .
  simulateRollingWest . map reverse . transpose .
  simulateRollingWest . transpose .
  simulateRollingWest

simulateRollingCycles :: Int -> [String] -> [String]
simulateRollingCycles maxCy = go 0 mempty . transpose
  where
    go n mp ss
      | n == maxCy = transpose ss
      | otherwise = case Map.lookup ss mp of
        Nothing -> do
          let ss' = simulateRollingCycle ss
          go (n+1) (Map.insert ss (n, ss') mp) ss'
        Just (n', ss') -> do
          let cycleLen = n - n'
              remainingCycles = (maxCy - n) `mod` cycleLen - 1
          go (maxCy - remainingCycles) mp ss'

day14 :: IO ()
day14 = do
  input <- lines <$> readFile "input/day14"
  let rolled = transpose $ simulateRollingWest (transpose input)
  putStr "Part 1: "
  print $ calcLoad rolled
  let cycled = simulateRollingCycles 1000000000 input
  putStr "Part 2: "
  print $ calcLoad cycled
