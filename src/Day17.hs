module Day17 where

import Algorithm.Search (aStar)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Direction = L | U | R | D
  deriving (Show, Eq, Ord, Enum)

bestRoute :: Vector (Vector Int) -> Int -> Int -> Maybe (Int, [(Int, Int, Int, Direction)])
bestRoute city maxS minS = aStar neigh trans est sol initial
  where
    sol (x, y, _, _) = x == mX - 1 && y == mY - 1
    initial = (0, 0, 0, D)
    est (x, y, _, _) = (mX - x) + (mY - y)
    mY = V.length city
    mX = V.length (city V.! 0)
    trans _ (x, y, _, _) = city V.! y V.! x
    neigh (x, y, s, d) = filter inBounds $
      (if s < maxS then (move (s+1) x y d:) else id)
      (if s < minS then [] else [ move 1 x y d' | m <- [-1,1], let d' = toEnum ((fromEnum d + m) `mod` 4)])
    move s x y d = case d of
      L -> (x - 1, y, s, L)
      R -> (x + 1, y, s, R)
      U -> (x, y - 1, s, U)
      D -> (x, y + 1, s, D)
    inBounds (x, y, _, _) = x >= 0 && x < mX && y >= 0 && y < mY

day17 :: IO ()
day17 = do
  putStr "Part 1: "
  city <- V.fromList . map (V.fromList . map (read . return)) . lines <$> readFile "input/day17"
  case bestRoute city 3 0 of
    Nothing -> putStrLn "No path found"
    Just (h, _) -> do
      print h
  putStr "Part 2: "
  case bestRoute city 10 4 of
    Nothing -> putStrLn "No path found"
    Just (h, _) -> do
      print h
