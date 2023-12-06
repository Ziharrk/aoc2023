module Day6 where

import Data.Char (isDigit)

-- dist+1 = (time - x) * x
-- dist+1 = time*x - x^2
-- x^2 - time*x + dist+1 = 0
-- x = time/2 +- sqrt(time^2/4 - dist - 1))
-- For integral solutions, we round down and up, respectively.
-- Important: `div` also needs to round up in the second component.
calcZeroPoints :: Integer -> Integer -> (Integer, Integer)
calcZeroPoints time dist =
  let root = sqrt (((fromInteger time ^ (2 :: Integer)) / 4)  - fromInteger dist - 1) :: Double
  in  ((time  `div` 2) - floor root, (time - 1)  `div` 2 + ceiling root)

parseWith :: (String -> [String]) -> String -> [String] -> IO ()
parseWith parse part input = case map (map read . parse . dropWhile (not . isDigit)) input of
  [times, distances :: [Integer]] -> putStr part >> print (product (zipWith (\t d -> calcPossibles (calcZeroPoints t d)) times distances))
    where calcPossibles (least, most) = most - least + 1
  _ -> error "invalid input"

day6 :: IO ()
day6 = do
  input <- lines <$> readFile "input/day6"
  -- parse as multiple numbers
  parseWith words "Part 1:" input
  -- parse as one number
  parseWith (return . filter (/= ' ')) "Part 2:" input
