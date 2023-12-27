module Day25 where

import Algorithm.Search (bfs)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec (char, letter, many, manyTill, newline, Parsec, parse, endBy1, count)

day25 :: IO ()
day25 = do
  input <- readFile "input/day25"
  case parse modules "" input of
    Left err -> print err
    Right ms -> do
      let revMap = Map.fromListWith (++) (concatMap (\(a, b) -> (,[a]) <$> b) (Map.toList ms))
          ms' = Map.unionWith (++) ms revMap
      case tryCutting ms' of
        Nothing -> print "No solution found"
        Just (cutTree1, cutTree2) -> do
          putStr "Part 1: "
          print (length cutTree1 * length cutTree2)

tryCutting :: Map String [String] -> Maybe ([String], [String])
tryCutting m'
  | (startNode : nodes) <- Map.keys m'
    = foldr ((<|>) . flowBetween startNode [] m') Nothing nodes
  | otherwise = Nothing
  where
    flowBetween s ps m f =
            case bfs (flip (Map.findWithDefault []) m) (==f) s of
              Nothing -> if length ps == 3
                then Just (getComponents m s ps)
                else Nothing
              Just p -> if length ps >= 3
                then Nothing
                else flowBetween s (p:ps) (removePath (s:p) m) f
    getComponents m s paths =
      let duoGraph = foldr removePath m paths
          component1 = floodfill s duoGraph
      in (component1, Map.keys $ foldr Map.delete m component1)
    floodfill start m = go Set.empty [start]
      where
        go visited [] = Set.toList visited
        go visited (n:ns)
          | n `Set.member` visited = go visited ns
          | otherwise = go (Set.insert n visited) (m Map.! n ++ ns)
    removePath []         m = m
    removePath [_]        m = m
    removePath (n1:n2:ns) m = removePath (n2:ns) $
                              Map.adjust (filter (/= n1)) n2 $
                              Map.adjust (filter (/= n2)) n1 m

modules :: Parsec String () (Map String [String])
modules = Map.fromList <$> mdl `endBy1` newline
  where
    mdl = (,) <$> manyTill letter (char ':')
              <*> many (char ' ' *> count 3 letter)
