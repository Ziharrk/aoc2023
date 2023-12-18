module Day18 where

import Text.Parsec (char, hexDigit, space, choice, count, digit, newline, many1, endBy1, Parsec, parse)

data Direction = R | D | L | U
  deriving (Show, Eq, Enum)

data Dig = Dig { dir  :: Direction, len  :: Integer,
                 dirH :: Direction, lenH :: Integer }
  deriving (Show, Eq)

dig :: Parsec String () Dig
dig = dig' <$> oneDirection <*> (space *> number) <*> (space *> hex)
  where
    dig' d l h = Dig d l (toEnum (read (drop 5 h))) (read ("0x" ++ take 5 h))
    oneDirection = choice [ L <$ char 'L', R <$ char 'R', U <$ char 'U', D <$ char 'D' ]
    hex = char '(' *> char '#' *> count 6 hexDigit <* char ')'
    number = read <$> many1 digit

makePath :: (Dig -> Direction) -> (Dig -> Integer) -> [Dig] -> [(Integer, Integer)]
makePath getD getL = scanr path (0, 0)
  where
    path d (x, y) = case getD d of
      R -> (x + getL d, y)
      D -> (x, y - getL d)
      L -> (x - getL d, y)
      U -> (x, y + getL d)

fillPathSize :: [(Integer, Integer)] -> Integer
fillPathSize xs = (`div` 2) $
  2 + sum (zipWith dist xs (tail xs)) +
  abs (sum (zipWith cross xs (tail xs)))
  where
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

day18 :: IO ()
day18 = do
  input <- readFile "input/day18"
  case parse (dig `endBy1` newline) "" input of
    Left err -> print err
    Right d  -> do
      putStr "Part 1: "
      print (fillPathSize $ makePath dir len d)
      putStr "Part 2: "
      print (fillPathSize $ makePath dirH lenH d)
