module Day.Day11 (run) where

import Test.HUnit ((@=?))

parse = id

solveA = id

solveB = id

run :: String -> IO ()
run xs = do
  print xs
  let parsed = parse xs
  print parsed
  let resA = solveA parsed
  print resA

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
