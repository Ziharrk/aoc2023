module Day12 where

import Control.Monad.State.Strict (State, evalState, foldM, modify, get)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map as Map

splitOn :: Char -> String -> (String, [Int])
splitOn c s = (start, read ('[' : tail end ++ "]"))
  where
     (start, end) = break (== c) s

type Memo = Map (String, [Int]) Integer

updateAndReturn :: Ord k => k -> State (Map k v) v -> State (Map k v) v
updateAndReturn k mv = mv >>= \v -> modify (Map.insert k v) >> return v

calcPossibilities :: String -> [Int] -> State Memo Integer
calcPossibilities s ns = do
  memo <- get
  case Map.lookup (s, ns) memo of
    Just v -> return v
    Nothing -> updateAndReturn (s, ns) $ case (s, ns) of
      (_     , []   ) -> if all (`elem` "?.") s then return 1 else return 0
      ('.':ss, _:_  ) -> calcPossibilities ss ns
      ('?':ss, _:_  ) -> (+) <$> calcPossibilities ss ns <*> calcPossibilities ('#':ss) ns
      (_  :_ , n:ns')
        | all (`elem` "?#") broken &&
          length broken == n -> case ok of
            ('.':rest) -> calcPossibilities rest ns'
            ('?':rest) -> calcPossibilities rest ns'
            []         -> if null ns' then return 1 else return 0
            _          -> return 0
        | otherwise -> return 0
        where (broken, ok) = splitAt n s
      ([], _:_) -> return 0

extend :: (String, [Int]) -> (String, [Int])
extend (s, p) = (intercalate "?" $ replicate 5 s, concat $ replicate 5 p)

day12 :: IO ()
day12 = do
  input <- readFile "input/day12"
  let lin = map (splitOn ' ') $ lines input
  -- print lin
  let go (p1, p2) i = do
        !p1' <- uncurry calcPossibilities i
        !p2' <- uncurry calcPossibilities (extend i)
        return (p1 + p1', p2 + p2')
      (res1, res2) = evalState (foldM go (0, 0) lin) Map.empty
  putStrLn "Both parts computed simultaneously."
  putStr "Part 1: "
  print res1
  putStr "Part 2: "
  print res2
