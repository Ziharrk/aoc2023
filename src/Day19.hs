{-# LANGUAGE OverloadedRecordDot #-}
module Day19 where

import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Tuple.Extra (both)
import Text.Parsec (Parsec, parse, char, string, digit, letter, newline, manyTill, endBy1, sepBy1, many1, oneOf, optional, try, (<|>))

data Process = Process { name :: String, workflows :: [Workflow] }

data Workflow = Workflow { condition :: Item -> Bool
                         , conditionAttr, conditionOp :: Char
                         , value :: Integer
                         , act :: String }
              | Always { act :: String }

data Item = Item { coolness, musicalness, aerodinamicness, shinyness :: Integer }
  deriving Show

followProcess :: Map String [Workflow] -> Item -> Bool
followProcess mp = go "in"
  where
    go nm i = case Map.lookup nm mp of
      Nothing -> error "invalid process name"
      Just ws -> case followWorkflows ws i of
        "A" -> True
        "R" -> False
        nm' -> go nm' i

followWorkflows :: [Workflow] -> Item -> String
followWorkflows [] i = error "No way to proceeed for: " ++ show i
followWorkflows (w:ws) i = case w of
  Always s -> s
  Workflow cond _ _ _ tA -> if cond i then tA else followWorkflows ws i

type Range = (Integer, Integer)
type ItemSpec = (Range, Range, Range, Range)

followProcessSymbolic :: Map String [Workflow] -> [ItemSpec]
followProcessSymbolic mp = go "in" ((1, 4000), (1, 4000), (1, 4000), (1, 4000))
  where
    go nm range = case Map.lookup nm mp of
      Nothing -> error "invalid process name"
      Just ws ->
        let (acceptedSpecs, furtherSpecs) = followWorkflowsSymbolic ws range
            moreSpecs = concatMap (uncurry go) furtherSpecs
        in acceptedSpecs ++ moreSpecs

followWorkflowsSymbolic :: [Workflow] -> ItemSpec -> ([ItemSpec], [(String, ItemSpec)])
followWorkflowsSymbolic [] _ = ([],[])
followWorkflowsSymbolic (w:ws) r@(x, m , a, s) = dispatch w.act $ case w of
  Always _                 -> (Just r, Nothing)
  Workflow _ attr op val _ -> modifyAt attr (withOp op val)
  where
    dispatch act (mt, mf) = case act of
      "A" -> (maybe remainingA (:remainingA) mt, remainingS)
      "R" -> (remainingA, remainingS)
      str -> (remainingA, maybe remainingS ((:remainingS). (str,)) mt)
      where
        (remainingA, remainingS) = bimap concat concat $ unzip $
                                    map (followWorkflowsSymbolic ws) (maybeToList mf)
    withOp '<' val (low, up)
      | val > up  = (Just (low, up), Nothing)
      | val < low = (Nothing, Just (low, up))
      | otherwise = (Just (low, val-1), Just (val, up))
    withOp '>' val (low, up)
      | val < low  = (Just (low, up), Nothing)
      | val > up = (Nothing, Just (low, up))
      | otherwise = (Just (val+1, up), Just (low, val))
    withOp _ _ _ = error "invalid operator"
    modifyAt 'x' f = both (fmap (,m,a,s)) (f x)
    modifyAt 'm' f = both (fmap (x,,a,s)) (f m)
    modifyAt 'a' f = both (fmap (x,m,,s)) (f a)
    modifyAt 's' f = both (fmap (x,m,a,)) (f s)
    modifyAt _ _ = error "invalid attribute"

partValue :: Item -> Integer
partValue (Item x m a s) = x + m + a + s

valueCount :: ItemSpec -> Integer
valueCount (x, m, a, s) = diff x * diff m * diff a * diff s
  where
    diff (l, u) = u - l + 1

day19 :: IO ()
day19 = do
  input <- readFile "input/day19"
  case parse ((,) <$> process `endBy1` newline <*> (newline *> item `endBy1` newline)) "" input of
    Left err -> print err
    Right (ps, is) -> do
      let mp = foldr (\p -> Map.insert p.name p.workflows) Map.empty ps
      let accepted = filter (followProcess mp) is
      putStr "Part 1: "
      print $ sum $ map partValue accepted
      putStr "Part 2: "
      print $ sum $ map valueCount $ followProcessSymbolic mp

process :: Parsec String () Process
process = Process <$> manyTill letter (char '{') <*> workflow `sepBy1` char ',' <* optional (char '}')
  where
    workflow = try (mkWorkflow <$> oneOf "xmas"
                           <*> oneOf "<>"
                           <*> number
                           <*> (char ':' *> many1 letter))
            <|> (Always <$> manyTill letter (char '}'))
    mkWorkflow what op val = Workflow (flip (parseOp op) val  . parseWhat what) what op val
    parseOp '<' = (<)
    parseOp '>' = (>)
    parseOp _ = error "Invalid operator"
    parseWhat 'x' = coolness
    parseWhat 'm' = musicalness
    parseWhat 'a' = aerodinamicness
    parseWhat 's' = shinyness
    parseWhat _ = error "Invalid attribute"

item :: Parsec String () Item
item = Item <$> (string "{x=" *> number)
            <*> (string ",m=" *> number)
            <*> (string ",a=" *> number)
            <*> (string ",s=" *> number <* char '}')

number :: Parsec String () Integer
number = read <$> many1 digit
