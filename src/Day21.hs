module Day21 where

import Control.Monad (filterM)
import Control.Monad.ST (runST)
import qualified Data.Sequence as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

simulateSteps :: Vector (Vector Char) -> (Int -> Int) -> (Int -> Bool) -> Int -> (Int, Int)
              -> Vector (Vector (Maybe Int))
simulateSteps mp f stop n' (startPosX, startPosY) = runST $ do
  v <- sequence (V.replicate (V.length mp) (MV.replicate (V.length (mp V.! 0)) Nothing))
  let go S.Empty = mapM V.freeze v
      go ((n, x, y) S.:<| xs) = case mp V.!? y >>= (V.!? x) of
        Just c | c `elem` ".S" -> do
          e <- MV.read (v V.! y) x
          case e of
            Nothing -> do
              MV.write (v V.! y) x (Just n)
              let newXs = if stop n then [] else [(f n, x + 1, y), (f n, x - 1, y), (f n, x, y + 1), (f n, x, y - 1)]
              newXs' <- filterM unvisited newXs
              go (xs S.>< S.fromList newXs')
            Just _ -> go xs
        _        -> go xs
      maxY = V.length mp
      unvisited (_, x', y')
        | y' < maxY && y' > 0 = (== Just Nothing) <$> MV.readMaybe (v V.! y') x'
        | otherwise = return False
  go (S.singleton (n', startPosX, startPosY))

day21 :: IO ()
day21 = do
  input <- readFile "input/day21"
  let mp = V.fromList . map V.fromList . lines $ input
      startPosY = V.head $ V.findIndices (elem 'S') mp
      startPosX = V.head $ V.findIndices (== 'S') (mp V.! startPosY)
  let stepMatrix = simulateSteps mp pred (==0) 64 (startPosX, startPosY)
      reachableEven = V.length $ V.filter even . V.mapMaybe id . V.concat $ V.toList stepMatrix
  putStr "Part 1: "
  print reachableEven
  let multiplier = 7
      bigmp = V.map (V.concat . replicate multiplier) $ V.concat (replicate multiplier mp)
      startPosXBig = startPosX + (multiplier `div` 2) * V.length (mp V.! 0)
      startPosYBig = startPosY + (multiplier `div` 2) * V.length mp
      stepMatrix1 = simulateSteps bigmp pred (==0) 65 (startPosXBig, startPosYBig)
      stepMatrix2 = simulateSteps bigmp pred (==0) 196 (startPosXBig, startPosYBig)
      stepMatrix3 = simulateSteps bigmp pred (==0) 327 (startPosXBig, startPosYBig)
      reachableEven1 = V.length $ V.filter even . V.mapMaybe id . V.concat $ V.toList stepMatrix1
      reachableEven2 = V.length $ V.filter even . V.mapMaybe id . V.concat $ V.toList stepMatrix2
      reachableEven3 = V.length $ V.filter even . V.mapMaybe id . V.concat $ V.toList stepMatrix3
      c = fromIntegral reachableEven1
      b = fromIntegral $ (4*reachableEven2 - 3*reachableEven1 - reachableEven3) `div` 2
      a = fromIntegral $ reachableEven2 - reachableEven1 - b
      f n = a*n^(2::Integer) + b*n + c
      steps = 26501365
  putStr "Part 2: "
  print $ f ((steps - (V.length mp `div` 2)) `div` V.length mp)
