module Day.Day04 where

import Data.List.Extra (splitOn)
import Test.HUnit ((@=?))
import Utils (countP, readInt)

parseSections :: String -> [((Int, Int), (Int, Int))]
parseSections = fmap (parseSection . splitOn ",") . lines
 where
  parseSection = toSection . fmap (fmap readInt . splitOn "-")
  toSection [[a, b], [c, d]] = ((a, b), (c, d))
  toSection _ = error ""

isContained :: (Int, Int) -> (Int, Int) -> Bool
isContained (a1, a2) (b1, b2) = a1 >= b1 && a2 <= b2

countIntervals :: (t -> t -> Bool) -> [(t, t)] -> Int
countIntervals pred = length . filter predBoth
 where
  predBoth (x, y) = pred x y || pred y x

isOverlapping :: Ord a => (a, a) -> (a, a) -> Bool
isOverlapping (a1, a2) (b1, b2)
  | a1 == b1 = True
  | a1 < b1 && a2 >= b1 = True
  | a1 > b1 && a1 <= b2 = True
  | otherwise = False

run :: String -> IO ()
run xs = do
  let parsed = parseSections xs

  let resA = countIntervals isContained parsed
  print resA

  resA @=? 511

  let resB = countIntervals isOverlapping parsed
  print resB

  resB @=? 821
