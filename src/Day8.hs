{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day8 where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

buildMap :: [String] -> Map String (String, String)
buildMap = go Map.empty
  where
    go m [] = m
    go m (x : xs) =
      case words x of
        [name,_,l,r] ->
          go (Map.insert name (l', r') m) xs
          where
            l' = init $ tail l
            r' = init r
        _ -> error "Invalid input"

goNetwork :: (String -> Bool) -> String -> Map String (String, String) -> [Char] -> (String, Integer)
goNetwork p loc mp dir
  | p loc     = (loc, 0)
  | otherwise = case Map.lookup loc mp of
    Just (l, r) ->
      case dir of
        'L':ds -> (1 +) <$> goNetwork p l mp ds
        'R':ds -> (1 +) <$> goNetwork p r mp ds
        _ -> error "Invalid direction"
    Nothing -> error "Invalid location"


-- This was set up for much more complex stuff, but the input has a secret that simplifies the problem.
type GhostInfo = Integer -- (Map Integer (Integer, Integer), Integer)

evalGhostInfo :: [GhostInfo] -> Integer
evalGhostInfo = foldr1 lcm

evalCycleLength :: String -> Map String (String, String) -> [Char] -> GhostInfo
evalCycleLength loc' mp dir =
  let ghostEnd = (== 'Z') . last
      (_, initialVal) = goNetwork ghostEnd loc' mp (cycle dir)
      -- (loc'', initialVal) = goNetwork ghostEnd loc' mp (cycle dir)
      -- dirLen = fromIntegral $ length dir
      -- initialMod = initialVal `mod` dirLen
      -- go loc stepMod acc = let (newLoc, newVal) = goNetwork ghostEnd loc mp (drop (fromInteger stepMod) (cycle dir))
      --                          newMod = (stepMod + newVal) `mod` dirLen
      --                          newMap = Map.insert stepMod (newVal, newMod) acc
      --                      in case Map.lookup newMod acc of
      --                           Just _ -> (newMap, newMod)
      --                           Nothing -> go newLoc newMod newMap
  -- in go loc'' initialMod (Map.singleton 0 (initialVal, initialMod))
  in initialVal

day8 :: IO ()
day8 = do
  input <- readFile "input/day8"
  case lines input of
    (dirs:_:network) -> do
      let netMap = buildMap network
          (_, steps) = goNetwork (=="ZZZ") "AAA" netMap (cycle dirs)
      putStr "Part 1: "
      print steps
      let ghostLocs = Set.filter ((== 'A') . last) $ Map.keysSet netMap
          ghostInfos = map (\loc -> evalCycleLength loc netMap dirs) $ Set.toList ghostLocs
      putStr "Part 2: "
      print $ evalGhostInfo ghostInfos
    _ -> putStrLn "Invalid input"
