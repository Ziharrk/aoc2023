{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Day23 where

import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Tuple.Extra (both, swap)
import Data.Vector (Vector)
import qualified Data.Vector as V

findLongestPath :: forall s. CheckSlope s => Map (Int, Int) ([(Int, Int, Int)], [(Int, Int, Int)])
                -> (Int, Int) -> (Int, Int) -> Int
findLongestPath mp (sX, sY) (eX, eY) = go Map.empty [(sX, sY, 0, Set.empty)]
  where
    go memo [] = memo Map.! (eX, eY)
    go memo ((x, y, n, visited) : xs)
      | (x, y) == (eX, eY) = go (Map.insertWith max (x, y) n memo) xs
      | otherwise =
         let visited' = Set.insert (x, y) visited
             neigh = Map.findWithDefault ([], []) (x, y) mp
             reachable = map (\(n', x', y') -> (x', y', n+n', visited')) $ checkSlope @s neigh
             reachableUnvis = filter (\(x', y', _, _) -> Set.notMember (x', y') visited') reachable
         in go (Map.insertWith max (x, y) n memo) (reachableUnvis ++ xs)

-- UGLY CODE BELOW!
mkGraph :: Vector (Vector Char) -> Map (Int, Int) ([(Int, Int, Int)], [(Int, Int, Int)])
mkGraph mp = Map.fromList $ map junctionInfo $ Set.toList junctions
  where
    junctionInfo (x, y) =
      let neighs = [ (x', y') | (x', y') <- [ (x, y-1), (x, y+1), (x-1, y), (x+1, y) ]
                              , Just c' <- [mp V.!? y' >>= (V.!? x')]
                              , c' /= '#' ]
      in ((x,y), both concat $ unzip $ mapMaybe (junctionInfo' (x, y)) neighs)
    junctionInfo' j (x, y) = do
      (x', y', b', n) <- findNextJunction (x, y) (Set.singleton j) True 0
      if b' then return ([(n, x', y')], [(n, x', y')])
            else return ([], [(n, x', y')])
    findNextJunction (x, y) vis b n =
      let neigh = [ (x', y', b') | (x', y', c) <- [ (x, y-1, '^'), (x, y+1, 'v')
                                                  , (x-1, y, '<'), (x+1, y, '>') ]
                                , Just c' <- [mp V.!? y' >>= (V.!? x')]
                                , c' /= '#', Set.notMember (x', y') vis
                                , let b' = b && (c' == '.' || c' == c) ]
      in case neigh of
        [(x', y', b')]
          | (x', y') `Set.member` junctions -> Just (x', y', b', n+2)
          | otherwise -> findNextJunction (x', y') (Set.insert (x, y) vis) b' (n+1)
        [] -> Nothing
        xs -> error $ show (x, y, vis, xs)
    junctions = foldr (Set.insert . swap . fmap fst) Set.empty $
                  V.filter isJunction $ V.concat $ V.toList indexed
    indexed = V.zipWith (\i -> fmap (i,) . V.indexed) (V.fromList [0..]) mp
    isJunction (y, (x, c)) = c /= '#' && 2 /= length (
      filter (/= Just True) [ fmap (/= '#') ((mp V.!? y) >>= (V.!? (x-1)))
                            , fmap (/= '#') ((mp V.!? y) >>= (V.!? (x+1)))
                            , fmap (/= '#') ((mp V.!? (y-1)) >>= (V.!? x))
                            , fmap (/= '#') ((mp V.!? (y+1)) >>= (V.!? x))
                            ])

day23 :: IO ()
day23 = do
  mp <- lines <$> readFile "input/day23"
  case (elemIndex '.' (head mp), elemIndex '.' (last mp)) of
    (Just startX, Just endX) -> do
      let (startY, endY) = (0, length mp - 1)
      let g = mkGraph (V.fromList $ map V.fromList mp)
      putStr "Part 1: "
      print $ findLongestPath @Slope g (startX, startY) (endX, endY)
      putStr "Part 2: "
      print $ findLongestPath @NoSlope g (startX, startY) (endX, endY)

    _ -> print "No start/end found"

data SlopeType = Slope | NoSlope

class CheckSlope (a :: SlopeType) where
  checkSlope :: (y, y) -> y

instance CheckSlope 'Slope where
  checkSlope (s, _) = s

instance CheckSlope 'NoSlope where
  checkSlope (_, s) = s
