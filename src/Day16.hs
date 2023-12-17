module Day16 where

import Control.Monad.ST (runST)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data Direction = L | R | U | D
  deriving (Show, Eq, Enum)

passRay :: (Int, Int, Direction) -> Vector (Vector Char) -> Int
passRay start maze = runST $ do
  v <- sequence (V.replicate (V.length maze) (MV.replicate (V.length (maze V.! 0)) []))
  let go [] = V.foldM' (MV.foldr' (\xs -> if null xs then id else succ)) 0 v
      go ((x, y, d):xs) = case maze V.!? y >>= (V.!? x) of
        Nothing -> go xs
        Just c  -> do
          ds <- MV.read (v V.! y) x
          if d `elem` ds
            then go xs
            else do
              MV.write (v V.! y) x (d:ds)
              go (nextRayPositions x y d c xs)
  go [start]
  where
    nextRayPositions x y d c xs = case c of
      '.'  -> move x y d : xs
      '-'  -> case d of
        L -> move x y L : xs
        R -> move x y R : xs
        _ -> move x y L : move x y R : xs
      '|'  -> case d of
        U -> move x y U : xs
        D -> move x y D : xs
        _ -> move x y U : move x y D : xs
      '/'  -> case d of
        L -> move x y D : xs
        R -> move x y U : xs
        U -> move x y R : xs
        D -> move x y L : xs
      '\\' -> case d of
        L -> move x y U : xs
        R -> move x y D : xs
        U -> move x y L : xs
        D -> move x y R : xs
      _    -> error "Invalid maze"

    move x y d = case d of
      L -> (x - 1, y, L)
      R -> (x + 1, y, R)
      U -> (x, y - 1, U)
      D -> (x, y + 1, D)

day16 :: IO ()
day16 = do
  maze <- V.fromList . map V.fromList . lines <$> readFile "input/day16"
  let mY = V.length maze
      mX = V.length (maze V.! 0)
  putStr "Part 1: "
  print (passRay (0, 0, R) maze)
  putStr "Part 2: "
  print $ foldr (max . (`passRay` maze)) 0 $
    [(x, y, if y == 0 then D else U) | x <- [0..mX-1], y <- [0,mY-1]] ++
    [(x, y, if x == 0 then R else L) | y <- [0..mY-1], x <- [0,mX-1]]
