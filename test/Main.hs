module Main(main) where

import AllDays (allDays)

main :: IO ()
main = sequence_ allDays
