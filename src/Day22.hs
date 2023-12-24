{-# LANGUAGE PartialTypeSignatures #-}
module Day22 where

import Control.Monad.ST (runST)
import Data.List (sortOn, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Parsec (Parsec, char, digit, newline, endBy1, runParser, getState, putState, many1)

fallBricks :: [Brick] -> Map Int [Int]
fallBricks bricks = runST $ do
  v <- mk3D (succ xM) (succ yM) (succ zM) (Nothing :: Maybe Int)
  let go [] m = return m
      go (Brick i x1 y1 z1 x2 y2 z2 : bs) m = do
        touching <- fitZ v i z1 (z2 - z1) [(x, y) | x <- [x1..x2], y <- [y1..y2]]
        go bs (Map.insert i touching m)
  go bricks Map.empty
  where
    (xM,yM,zM) = foldr (\(Brick _ _ _ _ x1 y1 z1) (x2, y2, z2) ->
      (max x1 x2, max y1 y2, max z1 z2))
      (0,0,0) bricks

    mk3D x y z a = V.replicateM z (sequence (V.replicate y (MV.replicate x a)))

    fitZ v i z' zSize coords = do
      let addZ (x, y) = [ (x, y, z) | z <- [z'..z'+zSize] ]
          zCoords = concatMap addZ coords
      touching <- catMaybes <$> mapM (\(x, y, z) -> MV.read ((v V.! z) V.! y) x) zCoords
      if null touching && z' > 0
        then fitZ v i (z'-1) zSize coords
        else mapM_ (insertAt i v) zCoords >> return (nub touching)

    insertAt i v (x, y, z) = MV.write ((v V.! (z+1)) V.! y) x (Just i)

chainFalling :: Map Int [Int] -> Int -> Int
chainFalling mp = go 0 (Map.filter (not . null) mp) . Set.singleton
  where
    go acc m s | Just (i, is) <- Set.minView s =
      let newM = Map.delete i $ Map.map (filter (/=i)) m
          newFall = Map.keys (Map.filter null newM)
      in go (acc+length newFall) (Map.filter (not . null) newM) (foldr Set.insert is newFall)
    go acc _ _ = acc

day22 :: IO ()
day22 = do
  input <- readFile "input/day22"
  case runParser (brick `endBy1` newline) 0 "" input of
    Left err -> print err
    Right bricks -> do
      let sortedZ = sortOn (\(Brick _ _ _ z _ _ _) -> z) bricks
      let supportedBy = fallBricks sortedZ
      let oneSupport = Map.filter ((==1) . length) supportedBy
      let notDisintegratable = nub $ concat $ Map.elems oneSupport
      putStr "Part 1: "
      print (length bricks - length notDisintegratable)
      putStr "Part 2: "
      print $ sum $ map (chainFalling supportedBy) notDisintegratable

data Brick = Brick Int Int Int Int Int Int Int
  deriving (Show, Eq)

brick :: Parsec String Int Brick
brick = mkBrick <*> number <* comma <*>
                    number <* comma <*>
                    number <* char '~' <*>
                    number <* comma <*>
                    number <* comma <*>
                    number
  where
    number = read <$> many1 digit
    comma = char ','
    mkBrick = do
      i <- getState
      putState (i+1)
      return $ \x1 y1 z1 x2 y2 z2 ->
        Brick i (min x1 x2) (min y1 y2) (min z1 z2) (max x1 x2) (max y1 y2) (max z1 z2)
