module Day1 where

import Data.Char ( isDigit )
import Data.List (isPrefixOf)

lineValue :: String -> Int
lineValue s = read [d1, d2]
  where
    d1 = head $ dropWhile (not . isDigit) s
    d2 = head $ dropWhile (not . isDigit) $ reverse s

preprocess :: String -> String
preprocess [] = []
preprocess s@(x:xs)
  | "one"   `isPrefixOf` s = "1" ++ preprocess xs
  | "two"   `isPrefixOf` s = "2" ++ preprocess xs
  | "three" `isPrefixOf` s = "3" ++ preprocess xs
  | "four"  `isPrefixOf` s = "4" ++ preprocess xs
  | "five"  `isPrefixOf` s = "5" ++ preprocess xs
  | "six"   `isPrefixOf` s = "6" ++ preprocess xs
  | "seven" `isPrefixOf` s = "7" ++ preprocess xs
  | "eight" `isPrefixOf` s = "8" ++ preprocess xs
  | "nine"  `isPrefixOf` s = "9" ++ preprocess xs
  | otherwise = x : preprocess xs

day1 :: IO ()
day1 = do
  input <- readFile "input/day1"
  let linesInput = lines input
  putStrLn $ "Part 1: " ++ show (sum $ map lineValue linesInput)
  putStrLn $ "Part 2: " ++ show (sum $ map (lineValue . preprocess) linesInput)
