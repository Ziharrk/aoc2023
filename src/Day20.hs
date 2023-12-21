module Day20 where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Parsec (Parsec, parse, anyChar, letter, string, newline, many1, endBy1, manyTill, sepBy1)

data Module = Module { name :: String, outputs :: [String], moduleType :: ModuleType }
  deriving (Show, Eq)

data Pulse = Low | High
  deriving (Show, Eq)

data ModuleType = Broadcaster | FlipFlop | Conjunction
  deriving (Show, Eq)

data ModuleState = BroadcasterState
                 | FlipFlopState Bool
                 | ConjunctionState (Map String Pulse)
  deriving (Show, Eq)

pressUntil :: (Integer -> Map String (ModuleState, Seq (String, Pulse)) -> Bool)
           -> Map String Module -> (Integer, Integer, Integer)
pressUntil f mp = go (0, 0, 0) Map.empty
  where
    go (accl, acch, n) state
      | f n state = (accl, acch, n)
      | otherwise = let (accl', acch', state') = pressButton (f n) mp state
                    in go (accl + accl', acch + acch', n + 1) state'

pressButton :: (Map String (ModuleState, Seq (String, Pulse)) -> Bool)
            -> Map String Module -> Map String (ModuleState, Seq (String, Pulse))
            -> (Integer, Integer, Map String (ModuleState, Seq (String, Pulse)))
pressButton f mp st = go (1, 0) (Map.insert "broadcaster" (BroadcasterState, Seq.singleton ("button", Low)) st)
  where
    go (accl, acch) state = if Map.foldr ((+) . Seq.length . snd) 0 state == 0 || f state
      then (accl, acch, state)
      else let (outputs, states') = Map.mapAccumWithKey runModule Map.empty state
               mp' = Map.unionWith addSignals states' (Map.mapMaybeWithKey addInitState outputs)
           in go (accl + count (==Low) outputs, acch + count (==High) outputs) mp'
    count p = Map.foldr ((+) . fromIntegral . Seq.length . Seq.filter (p . snd)) 0
    addSignals (s, xs) (_, ys) = (s, xs Seq.>< ys)

    addInitState n s = (, s) . moduleState <$> Map.lookup n mp
      where moduleState m = case moduleType m of
              Broadcaster -> BroadcasterState
              FlipFlop    -> FlipFlopState False
              Conjunction -> ConjunctionState (Map.fromList $ map (,Low) $ reverseMap Map.! n)
    reverseMap = Map.foldrWithKey addEntry Map.empty mp
    addEntry inp m mp' = foldr (\o -> Map.insertWith (++) o [inp]) mp' (outputs m)

    runModule acc _ (s, Seq.Empty) = (acc, (s, Seq.empty))
    runModule acc n (s, (origin,x) Seq.:<| xs) = case s of
        BroadcasterState    ->
          (foldr (\k m -> Map.insertWith (Seq.><) k (Seq.singleton (n, x)) m) acc out, (BroadcasterState, xs))
        FlipFlopState    b -> if x == Low
            then (foldr (\k m -> Map.insertWith (Seq.><) k (Seq.singleton (n, p')) m) acc out, (FlipFlopState (not b), xs))
            else (acc, (FlipFlopState b , xs))
          where p' = if b then Low else High
        ConjunctionState sm ->
          let sm' = Map.insert origin x sm
              p = if all (==High) sm' then Low else High
          in (foldr (\k m -> Map.insertWith (Seq.><) k (Seq.singleton (n, p)) m) acc out, (ConjunctionState sm', xs))
      where out = maybe [] outputs $ Map.lookup n mp

day20 :: IO ()
day20 = do
  input <- readFile "input/day20"
  case parse (modl `endBy1` newline) "" input of
    Left err -> print err
    Right ms -> do
      let mp = Map.fromList $ map (\m -> (name m, m)) ms
      let (l, h, _) = pressUntil (\n _ -> n == 1000) mp
      putStr "Part 1: "
      print (l*h)
      let rxPrevNodes = name <$> Map.filter (elem "rx" . outputs) mp
          rxPrevInputs = nub $ name <$> Map.elems (Map.filter ((`elem` rxPrevNodes) . head . outputs) mp)
          hasHighPress n s = case Map.lookup n s of
            Nothing      -> False
            Just (_, xs) -> Low `elem` fmap snd xs
          (_,_,ns) = unzip3 $ map (\n -> pressUntil (\_ -> hasHighPress n) mp) rxPrevInputs
      if null ns then
        putStrLn "Input does not contain module \"rx\".\nPart 2 is not applicable."
      else do
        putStr "Part 2: "
        print (foldr1 lcm ns)

modl :: Parsec String () Module
modl = mkM <$> manyTill anyChar (string " -> ") <*> many1 letter `sepBy1` string ", "
  where
    mkM str out = case str of
      "broadcaster" -> Module str out Broadcaster
      '%':s         -> Module s   out FlipFlop
      '&':s         -> Module s   out Conjunction
      _ -> error "Unknown module type"
