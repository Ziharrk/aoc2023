module Day4 where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Parsec
    ( char, string, digit, endBy1, sepBy1, many, many1, parse, Parsec )

data Card = Card { idx :: Int
                 , winning, having :: [Int]
                 }
  deriving (Show, Eq)

card :: Parsec String () Card
card = Card <$> (string "Card" *> spaces *> number <* char ':')
            <*> (spaces *> number `endBy1` spaces <* char '|')
            <*> (spaces *> number `sepBy1` spaces)
  where number = read <$> many1 digit
        spaces = many $ char ' '

winningCard :: Card -> Int
winningCard (Card _ ws hs) = length $ filter (`elem` ws) hs

pointsFor :: Card -> Integer
pointsFor c = case winningCard c of
  0 -> 0
  n -> 2 ^ (n-1)

wonCards :: [Card] -> IntMap Integer
wonCards cs' = go (IntMap.fromList [(i, 1) | i <- [1.. length cs']]) cs'
  where
    go current [] = current
    go current (c:cs) =
      let new = foldr (\v -> incAt (idx c + v) (current IntMap.! idx c)) current [1..winningCard c]
      in go new cs

    incAt = IntMap.insertWith (+)


day4 :: IO ()
day4 = do
  input <- readFile "input/day4"
  case parse (card `endBy1` char '\n') "" input of
    Left err -> print err
    Right parsed -> do
      putStr "Part 1: "
      print $ sum $ map pointsFor parsed
      putStr "Part 2: "
      print $ sum $ wonCards parsed
