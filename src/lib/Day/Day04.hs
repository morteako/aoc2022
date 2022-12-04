module Day.Day04 where

import Data.List.Extra
import Test.HUnit ((@=?))
import Utils (countP, readInt)

parseSections :: String -> [((Int, Int), (Int, Int))]
parseSections = fmap (parseSection . splitOn ",") . lines
 where
  parseSection = toSection . fmap (fmap readInt . splitOn "-")
  toSection [[a, b], [c, d]] = ((a, b), (c, d))
  toSection _ = error ""

isContained :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Bool
isContained (a1, a2) (b1, b2) = a1 >= b1 && a2 <= b2

-- countIntervals pred = length . filter predBothWays
--   where
--     predBot

countFullyContained :: [((Int, Int), (Int, Int))] -> Int
countFullyContained = length . filter anyIsContained
 where
  anyIsContained (x, y) = isContained x y || isContained y x

isOverlapping (a1, a2) (b1, b2)
  | a1 == b1 = True
  | a1 < b1 && a2 >= b1 = True
  | a1 > b1 && a1 <= b2 = True
  | otherwise = False

countOverlaps :: [((Int, Int), (Int, Int))] -> Int
countOverlaps = length . filter anyIsOverlapping
 where
  anyIsOverlapping (x, y) = isOverlapping x y || isOverlapping y x

run :: String -> IO ()
run xs = do
  let parsed = parseSections xs

  let resA = countFullyContained parsed
  print resA

  resA @=? 511

  let resB = countOverlaps parsed
  print resB

  resB @=? 821
