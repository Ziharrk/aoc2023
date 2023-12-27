{-# LANGUAGE OverloadedRecordDot          #-}
{-# LANGUAGE LambdaCase                   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head"               #-}
module Day24 where

import Control.Monad (zipWithM)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec (Parsec, parse, endBy1, newline, many1, oneOf, char, space)
import Z3.Monad ( evalInt, evalZ3, mkAdd, mkEq, mkFreshIntVar, mkGe, mkInteger
                , mkMul, solverAssertCnstr, solverCheck, solverGetModel, Result(..) )

data Hail = Hail { x, y, z, dx, dy, dz :: Integer }
  deriving (Show, Eq)

intersectWith :: Hail -> Hail -> Maybe (Rational, Rational)
intersectWith a b =
  let dx = fromInteger $ b.x - a.x
      dy = fromInteger $ b.y - a.y
      det = fromInteger $ b.dx * a.dy - b.dy * a.dx
  in if det == 0
      then Nothing
      else Just ( (dy * fromInteger b.dx - dx * fromInteger b.dy) / det
                , (dy * fromInteger a.dx - dx * fromInteger a.dy) / det )

intersectIn :: Hail -> Hail -> Rational -> Rational -> Bool
intersectIn a b minXY maxXY = case intersectWith a b of
  Nothing     -> False
  Just (u, v) -> u >= 0 && v >= 0 &&
                 tX >= minXY && tX <= maxXY &&
                 tY >= minXY && tY <= maxXY
    where
      tX = fromInteger a.x + u * fromInteger a.dx
      tY = fromInteger a.y + u * fromInteger a.dy

trySolving :: [Hail] -> Maybe (Integer, Integer, Integer)
trySolving hs = unsafePerformIO $ evalZ3 $ do
  x <- mkFreshIntVar "x"
  y <- mkFreshIntVar "y"
  z <- mkFreshIntVar "z"
  dx <- mkFreshIntVar "dx"
  dy <- mkFreshIntVar "dy"
  dz <- mkFreshIntVar "dz"
  zero <- mkInteger 0
  mapM_ (\h -> do
    x' <- mkInteger h.x
    y' <- mkInteger h.y
    z' <- mkInteger h.z
    dx' <- mkInteger h.dx
    dy' <- mkInteger h.dy
    dz' <- mkInteger h.dz
    u <- mkFreshIntVar "u"
    solverAssertCnstr =<< mkGe u zero
    muls  <- sequence [mkMul [dx,u], mkMul [dy, u], mkMul [dz, u]]
    adds <- sequence [mkAdd [x, muls !! 0], mkAdd [y, muls !! 1], mkAdd [z, muls !! 2]]
    muls' <- sequence [mkMul [dx', u], mkMul [dy', u], mkMul [dz', u]]
    adds' <- sequence [mkAdd [x', muls' !! 0], mkAdd [y', muls' !! 1], mkAdd [z', muls' !! 2]]
    mapM solverAssertCnstr =<< zipWithM mkEq adds adds'
    ) hs
  solverCheck >>= \case
    Sat -> do
      m <- solverGetModel
      mx <- evalInt m x
      my <- evalInt m y
      mz <- evalInt m z
      return ((,,) <$> mx <*> my <*> mz)
    _   -> return Nothing

day24 :: IO ()
day24 = do
  input <- readFile "input/day24"
  case parse (hail `endBy1` newline) "" input of
    Left err -> print err
    Right hs -> do
      let intersections = [ intersectWith a b
                          | i <- [0 .. length hs-1]
                          , let a = hs !! i
                          , j <- [i+1 .. length hs-1]
                          , let b = hs !! j
                          , intersectIn a b minXY maxXY
                          ]
      putStr "Part 1: "
      print $ length intersections
      putStr "Part 2: "
      case trySolving (take 3 hs) of
        Nothing -> print "No solution found"
        Just (x, y, z) -> print (x + y + z)
  where
    minXY = 200000000000000
    maxXY = 400000000000000

hail :: Parsec String () Hail
hail = Hail <$> (number <* char ',' <* many1 space)
            <*> (number <* char ',' <* many1 space)
            <*> (number <* many1 space <* char '@' <* many1 space)
            <*> (number <* char ',' <* many1 space)
            <*> (number <* char ',' <* many1 space)
            <*> number
  where
    number = read <$> many1 (oneOf "-0123456789")
