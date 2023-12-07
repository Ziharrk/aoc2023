{-# LANGUAGE DerivingStrategies #-}
module Day7 where

import Data.List (sort)

data Card = A | K | Q | J | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
  deriving (Eq, Ord, Enum)

data Type = Kind5 | Kind4 | FullHouse | Kind3 | TwoPair | OnePair | HighCard
  deriving (Eq, Ord)

data Hand = Hand [Card] Integer
  deriving Eq

instance Ord Hand where
  compare h1@(Hand cards1 _) h2@(Hand cards2 _) =
    case compare (handKind h1) (handKind h2) of
      EQ -> compare cards1 cards2
      x -> x

handKind :: Hand -> Type
handKind (Hand cards _)
  | kind5 cards = Kind5
  | kind4 cards = Kind4
  | fullHouse cards = FullHouse
  | kind3 cards = Kind3
  | twoPair cards = TwoPair
  | onePair cards = OnePair
  | otherwise = HighCard
  where
    kind5 (c:cs) = all (== c) cs
    kind5 _ = True
    kind4 (c1:c2:c3:cs)
      | c1 == c2  = if c2 == c3 then c1 `elem` cs
                                else all (== c1) cs
      | otherwise = all (== c1) (c3:cs) || all (== c2) (c3:cs)
    kind4 _ = True
    fullHouse cs = kind3 cs && twoPair cs
    kind3 (c1:c2:cs) = (c1 == c2 && c1 `elem` cs) || kind3 (c1:cs) || kind3 (c2:cs)
    kind3 _ = False
    twoPair cs = or $ zipWith (\idx c -> c `elem` rmIdx idx cs && onePair (filter (/=c) cs)) [0..] cs
    onePair cs = or $ zipWith (\idx c -> c `elem` rmIdx idx cs) [0..] cs

    rmIdx idx xs = take idx xs ++ drop (idx + 1) xs

newtype HandWithJoker = HJ Hand
  deriving Eq

newtype CardWithJoker = CJ Card
  deriving Eq

instance Ord CardWithJoker where
  compare (CJ J) (CJ J) = EQ
  compare (CJ J) (CJ _) = GT
  compare (CJ _) (CJ J) = LT
  compare (CJ c1) (CJ c2) = compare c1 c2

instance Ord HandWithJoker where
  compare (HJ h1@(Hand cards1 _)) (HJ h2@(Hand cards2 _)) =
    case compare (handKind' h1) (handKind' h2) of
      EQ -> compare (map CJ cards1) (map CJ cards2)
      x -> x
    where
      handKind' = minimum . map handKind . instantiate
      instantiate (Hand cards v) = replaceJ cards [[]]
        where
          replaceJ (J:cs) acc = replaceJ cs [xs ++ [c] | xs <- acc , c <- [A ..]]
          replaceJ (c:cs) acc = replaceJ cs [xs ++ [c] | xs <- acc]
          replaceJ [] acc = map (`Hand` v) acc

day7 :: IO ()
day7 = do
  input <- readFile "input/day7"
  let readHand s = Hand (map (read . return) $ take 5 s) (read $ drop 6 s)
      hands = map readHand $ lines input
  putStr "Part 1: "
  print $ sum $ zipWith (\(Hand _ v) r -> r * v) (sort hands) (reverse [1.. fromIntegral (length hands)])
  putStr "Part 2: "
  print $ sum $ zipWith (\(HJ (Hand _ v)) r -> r * v) (sort (map HJ hands)) (reverse [1.. fromIntegral (length hands)])

instance Read Card where
  readsPrec _ ('A':xs) = [(A, xs)]
  readsPrec _ ('K':xs) = [(K, xs)]
  readsPrec _ ('Q':xs) = [(Q, xs)]
  readsPrec _ ('J':xs) = [(J, xs)]
  readsPrec _ ('T':xs) = [(Ten, xs)]
  readsPrec _ ('9':xs) = [(Nine, xs)]
  readsPrec _ ('8':xs) = [(Eight, xs)]
  readsPrec _ ('7':xs) = [(Seven, xs)]
  readsPrec _ ('6':xs) = [(Six, xs)]
  readsPrec _ ('5':xs) = [(Five, xs)]
  readsPrec _ ('4':xs) = [(Four, xs)]
  readsPrec _ ('3':xs) = [(Three, xs)]
  readsPrec _ ('2':xs) = [(Two, xs)]
  readsPrec _ _ = []
