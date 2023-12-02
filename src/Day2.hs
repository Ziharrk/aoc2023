module Day2 where

import Text.Parsec
import Prelude hiding (curry, uncurry)

import Tuple

data Game = Game { gameId :: Int, rveals :: [Reveal] }
  deriving (Show)

data Reveal = Reveal { red, green, blue :: Integer }
  deriving (Show)

game :: Parsec String () Game
game = Game <$> (string "Game " *> number)
            <*> (char ':' *> reveal `sepBy1` char ';')

reveal :: Parsec String () Reveal
reveal = uncurry Reveal . partitionColors
            <$> color `sepBy1` char ','

color :: Parsec String () (String, Integer)
color = flip (,) <$> (spaces *> number)
                 <*> (spaces *> many1 letter)

number :: (Num n, Read n) => Parsec String () n
number = read <$> many1 digit

partitionColors :: Num n => [(String, n)] -> (n, n, n)
partitionColors [] = (0, 0, 0)
partitionColors ((c, i):xs)
  | c == "red"   = (i+r, g, b)
  | c == "green" = (r, g+i, b)
  | c == "blue"  = (r, g, b+i)
  | otherwise    = error "Invalid color"
  where
    (r, g, b) = partitionColors xs

isPossible :: Game -> Bool
isPossible (Game _ reveals) = all isPossible' reveals
  where
    isPossible' (Reveal r g b) = r <= 12 && g <= 13 && b <= 14

minimizeGame :: Game -> Game
minimizeGame (Game idG reveals) = Game idG [foldr1 minimizeReveal reveals]
  where
    minimizeReveal (Reveal r g b) (Reveal r' g' b') =
      Reveal (max r r') (max g g') (max b b')

gamePower :: Game -> Integer
gamePower (Game _ reveals) = sum $ map revealPower reveals
  where
    revealPower (Reveal r g b) = r*g*b

day2 :: IO ()
day2 = do
  input <- readFile "input/day2"
  case parse (game `endBy1` newline) "" input of
    Left err -> print err
    Right parsed -> do
      putStr "Part 1: "
      print (sum $ map gameId $ filter isPossible parsed)
      putStr "Part 2: "
      print (sum $ map (gamePower . minimizeGame) parsed)
