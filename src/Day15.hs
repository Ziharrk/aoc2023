module Day15 where

import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

type Box = [(String, Int)]

hash :: String -> Int
hash = go 0
  where
    go n [] = n
    go n (x:xs) = go (((fromEnum x + n) * 17) `mod` 256) xs

insertLenses :: [String] -> Map Int Box
insertLenses = go Map.empty
  where
    go m [] = m
    go m (x:xs) =
      case break (\c -> c == '-' || c == '=') x of
        (label, "-") -> go (Map.alter (fmap (removeLabel label)) (hash label) m) xs
        (label, '=':val) -> go (Map.alter (addLabel label (read val)) (hash label) m) xs
        _ -> error "Invalid input"
    removeLabel label = filter (\(l, _) -> l /= label)
    addLabel label val Nothing = Just [(label, val)]
    addLabel label val (Just []) = Just [(label, val)]
    addLabel label val (Just ((l,v):xs))
      | label == l = Just $ (l, val) : xs
      | otherwise  = ((l,v):) <$> addLabel label val (Just xs)

focalLength :: (Int, (String, Int)) -> Int
focalLength (boxIdx, (label, f)) =
  (hash label + 1) * boxIdx * f

day15 :: IO ()
day15 = do
  input <- readFile "input/day15"
  let xs = splitOn "," $ filter (/='\n') input
  putStr "Part 1: "
  print $ sum $ map hash xs
  putStr "Part 2: "
  print $ sum $ map focalLength $ concatMap (zip [1..]) $ Map.elems $ insertLenses xs
