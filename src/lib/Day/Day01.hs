module Day.Day01 where

import Data.List (sort)
import Data.List.Extra (splitOn, takeEnd)
import Test.HUnit ((@=?))

parse :: [Char] -> [[Int]]
parse = fmap (fmap read . words) . splitOn "\n\n"

solveA :: [[Int]] -> Int
solveA = maximum . fmap sum

solveB :: [[Int]] -> Int
solveB = sum . takeEnd 3 . sort . fmap sum

run :: String -> IO ()
run xs = do
  print xs
  let parsed = parse xs
  print parsed
  let resA = solveA parsed
  print resA

  resA @=? 75501

  let resB = solveB parsed
  print resB

  resB @=? 215594
