module Day5 where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec
    ( char, string, digit, endBy1, sepBy1
    , many1, parse, space, try, choice, Parsec )

-- algorithm for both 1 and 2
followPlan :: Split n => n -> Map Specifier (Specifier, [Range]) -> n
followPlan = go Seed
  where
    go Location v _ = v
    go s v m = case M.lookup s m of
      Nothing -> error ("No mapping for " ++ show s)
      Just (s', ranges) -> go s' (translate ranges v) m

    translate [] v = v
    translate (Range destFrom sourceFrom size : rs) v =
      splitUsing (sourceFrom, translate rs)
                 (sourceFrom + size, \e -> destFrom + (e - sourceFrom))
                 (translate rs) v

class Split n where
  splitUsing :: (Integer, n -> n)
             -> (Integer, Integer -> Integer)
             -> (n -> n)
             -> n -> n

-- part 1 uses plain Integer where splitting is just a 3-way conditional
instance Split Integer where
  splitUsing (a, f) (b, g) h n
    | n < a = f n
    | n < b = g n
    | otherwise = h n

newtype IntegerRange = IntegerRange [(Integer, Integer)]
  deriving (Show, Eq)

-- part two uses range math, where a range is potentially split if it overlaps
instance Split IntegerRange where
  splitUsing _      _      _ (IntegerRange []) = IntegerRange []
  splitUsing (a, f) (b, g) h (IntegerRange ((m, n):xs))
    | n < a            = let IntegerRange xs' = splitUsing (a, f) (b, g) h (IntegerRange xs)
                             IntegerRange cur = f (IntegerRange [(m, n)])
                         in  IntegerRange (cur ++ xs')
    | n <= b && m >= a = let IntegerRange xs' = splitUsing (a, f) (b, g) h (IntegerRange xs)
                         in  IntegerRange ((g m, g n) : xs')
    | m > b            = let IntegerRange xs' = splitUsing (a, f) (b, g) h (IntegerRange xs)
                             IntegerRange cur = h (IntegerRange [(m, n)])
                         in  IntegerRange (cur ++ xs')
    --  overlaps with [a..b]
    | otherwise        = splitUsing (a, f) (b, g) h (IntegerRange (splitted++xs))
      where
        splitted = ([(m, a-1) | m < a]) ++
                   (if n > b then [(max a m,b), (b+1,n)] else [(max a m, n)])

-- main
day5 :: IO ()
day5 = do
  input <- readFile "input/day5"
  case prep <$> parse guide "" input of
    Left err -> print err
    Right (seeds, seedMapping) -> do
      putStr "Part 1: "
      print (minimum (map (`followPlan` seedMapping) seeds))
      putStr "Part 2: "
      let pairUp [] = []
          pairUp [_] = []
          pairUp (x:y:xs) = (x, y) : pairUp xs
          seedRanges = IntegerRange $ map (\(m, n) -> (m, m+n-1)) (pairUp seeds)
      let IntegerRange locationRanges = seedRanges `followPlan` seedMapping
      print (minimum (map fst locationRanges))

-- data types
data Guide = Guide [Integer] [Mapping]
  deriving (Show, Eq)

data Mapping = Mapping { from, to :: Specifier
                       , ranges :: [Range]
                       }
  deriving (Show, Eq)

data Specifier = Seed | Soil | Fertilizer | Water
               | Light | Temperature | Humidity
               | Location
  deriving (Show, Eq, Ord, Enum)

-- parsing
data Range = Range { destFrom, sourceFrom, size :: Integer }
  deriving (Show, Eq)

guide :: Parsec String () Guide
guide = Guide <$> (string "seeds: " *> integer `sepBy1` char ' ')
              <*> (string "\n\n" *> mapping `sepBy1` char '\n')

mapping :: Parsec String () Mapping
mapping = Mapping <$> specifier <* string "-to-" <*> specifier <* string " map:\n"
                  <*> range `endBy1` char '\n'

specifier :: Parsec String () Specifier
specifier = choice
  [s <$ try (string (map toLower (show s))) | s <- [Seed ..]]

range :: Parsec String () Range
range = Range <$> integer <* space
              <*> integer <* space
              <*> integer

integer :: Parsec String () Integer
integer = read <$> many1 digit

prep :: Guide -> ([Integer], Map Specifier (Specifier, [Range]))
prep (Guide seeds maps) = (seeds, M.fromList (map toMap maps))
  where
    toMap (Mapping from to ranges) = (from, (to, ranges))
