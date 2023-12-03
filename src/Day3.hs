module Day3 where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec
    ( char, digit, oneOf, endBy1, many1, (<|>), parse, Parsec )

-- for part 1 and 2
filterAdjacient :: (Elem -> Bool) -> Board -> Board
filterAdjacient p b@(Board sX sY m) = Board sX sY $ V.imap (go 0 "") m
  where
    go :: Int -> String -> Int -> Vector Elem -> Vector Elem
    go x s y v
      | x >= sX   = v
      | otherwise = case v V.! x of
          Digit n  -> if any p $ adjacientLocs b x y
            then  let str = fromDigit <$> V.toList (V.takeWhile isDigit (V.drop x v))
                  in go (x+length str) "" y (v V.// [(x, Num $ read (reverse s ++ str))])
            else  go (x+1) (n:s) y v
          _ -> go (x+1) "" y v

-- for part 1 and 2
adjacientLocs :: Board -> Int -> Int -> [Elem]
adjacientLocs (Board sX sY m) x y =
  [get (x', y') | x' <- [x-1..x+1]
                , y' <- [y-1..y+1]
                , (x', y') /= (x, y)
                , x' >= 0 && x' < sX
                , y' >= 0 && y' < sY  ]
  where
    get (x', y') = m V.! y' V.! x'

-- for part 2
gearRatios :: Board -> Board
gearRatios b@(Board sX sY m) = Board sX sY $ V.imap (V.imap . go) m
  where
    go :: Int -> Int -> Elem -> Elem
    go y x Gear = case filter isNum (adjacientLocs b x y) of
      [Num r, Num s] -> Num $ r * s
      _              -> Empty
    go _ _ _ = Empty

-- for part 1 and 2
boardSum :: Board -> Integer
boardSum (Board _ _ m) = V.sum $ V.map (V.sum . V.map elemSum) m
  where
    elemSum (Num n) = n
    elemSum _  = 0

-- main function
day3 :: IO ()
day3 = do
  input <- readFile "input/day3"
  case parse board "" input of
    Left err -> print err
    Right parsed -> do
      putStr "Part 1: "
      print $ boardSum $ filterAdjacient isSymbol parsed
      putStr "Part 2: "
      print $ boardSum $ gearRatios $ filterAdjacient (==Gear) parsed

-- data types
data Board = Board { sizeX, sizeY :: Int
                   , boardMatrix :: Vector (Vector Elem) }
  deriving (Show, Eq)
data Elem = Empty | Symbol | Gear | Digit Char | Num Integer
  deriving (Show, Eq)

-- parser
board :: Parsec String () Board
board = mkBoard . V.fromList <$> (row `endBy1` char '\n')
  where
    mkBoard m = Board (V.length $ m V.! 0) (V.length m) m
    row = V.fromList <$> many1 boardElem
    boardElem  =  (Empty <$ char '.')
              <|> (Symbol <$ oneOf symbols)
              <|> (Digit <$> digit)
              <|> (Gear <$ char '*')

-- helpers
symbols :: [Char]
symbols = ['+', '@', '#', '$', '%', '^', '&', '=', '/', '-']

isSymbol, isNum, isDigit :: Elem -> Bool
isSymbol Symbol = True
isSymbol Gear   = True
isSymbol _      = False
isNum (Num _) = True
isNum _       = False
isDigit (Digit _) = True
isDigit _       = False

fromDigit :: Elem -> Char
fromDigit (Digit c) = c
fromDigit _         = error "Not a digit"
