{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day10 where

import Control.Monad (mplus)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Graph (graphFromEdges, Vertex)
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S
import Text.Parsec (Parsec, char, (<|>), parse)

type D2 = (Integer, Integer)

addD2 :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addD2 (n, m) (x, y) = (n + x, m + y)

type AdjList = [((), D2, [D2])]

enclosedTilesInLoop :: (Vertex -> ((), D2, [D2])) -> [Vertex] -> Integer
enclosedTilesInLoop f s =
  (abs (loopArea s) - fromIntegral (length s) + 3) `div` 2
  where
    loopArea (x' : y' : xs') =
      let (_,x,_) = f x'
          (_,y,_) = f y'
      in (snd x + snd y) * (fst y - fst x) + loopArea (y' : xs')
    loopArea [_] = 0
    loopArea []  = error "Empty list"

dfs :: Seq (Vertex, Integer, [Vertex])
    -> (Vertex -> ((), D2, [D2])) -> (D2 -> Maybe Vertex)
    -> Map Vertex (Integer, [Vertex]) -> Map Vertex (Integer, [Vertex])
dfs Empty _ _ m = m
dfs ((v, d, p) :<| vs) nodeFromVertex vertexFromKey m = case Map.lookup v m of
  Just _  -> dfs vs nodeFromVertex vertexFromKey m
  Nothing ->
    let (_, _, adj) = nodeFromVertex v
        vs' = S.fromList (filter (\(k,_,_) -> not $ k `Map.member` m)
                (mapMaybe (fmap (\v' -> (v',d+1,v':p)) . vertexFromKey) adj)) >< vs
    in dfs vs' nodeFromVertex vertexFromKey (Map.insert v (d, p) m)

day10 :: IO ()
day10 = do
  input <- readFile "input/day10"
  let lin = lines input
  let x = fromIntegral $ length (head lin)
      y = fromIntegral $ length lin
  let add (i, l) r = ((,i) . fromIntegral <$> elemIndex 'S' l) `mplus` r
      Just startIdx = foldr add Nothing (zip [0..] lin)
  let getD (i, j)
        | i >= 0 && i < x &&
          j >= 0 && j < y
          = Just $ lin !! fromIntegral j !! fromIntegral i
        | otherwise = Nothing
      checkNeigh (d, cs) = getD (addD2 d startIdx) >>= \c ->
        if c `elem` cs then Just (addD2 d startIdx) else Nothing
      startNeigh = mapMaybe checkNeigh
                      [ ((1, 0), ['J', '7', '-']), ((-1, 0), ['L', 'F', '-'])
                      , ((0, 1), ['7', 'F', '|']), ((0, -1), ['L', 'J', '|'])]
  case parse (parseGraph (x, y)) "" input of
    Left err -> print err
    Right graph -> do
      let (_, nodeFromVertex, vertexFromKey) = graphFromEdges (((), startIdx, startNeigh) : graph)
          Just startV = vertexFromKey startIdx
          info = dfs (S.singleton (startV, 0, [startV])) nodeFromVertex vertexFromKey mempty
      putStr "Part 1: "
      let (size, path) = maximumBy (compare `on` fst) info
      print (size `div` 2)
      putStr "Part 2: "
      print (enclosedTilesInLoop nodeFromVertex path)

parseGraph :: D2 -> Parsec String () AdjList
parseGraph (n, m) = concat <$> mapM (parseLine n) [0..(m-1)]

charMap :: [(Char, [D2])]
charMap = [('|', [(0, 1), (0, -1)]),
           ('-', [(1, 0), (-1, 0)]),
           ('L', [(0, -1), (1, 0)]),
           ('7', [(0, 1), (-1, 0)]),
           ('J', [(0, -1), (-1, 0)]),
           ('F', [(0, 1), (1, 0)]),
           ('.', []),
           ('S', [])]

parseLine :: Integer -> Integer -> Parsec String () AdjList
parseLine n y = concat <$> mapM (parseEntry . (,y)) [0..(n-1)] <* char '\n'

parseEntry :: D2 -> Parsec String () AdjList
parseEntry (n, m) = foldr ((<|>) . parseSym) (fail "Invalid line") charMap
  where
    parseSym (c, ds) = do
      _ <- char c
      if null ds
        then return []
        else return [((), (n, m), map (addD2 (n, m)) ds)]
