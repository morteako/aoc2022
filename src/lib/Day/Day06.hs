module Day.Day06 where

import Data.List (find, nub, tails)
import Test.HUnit ((@=?))

isUniq :: Ord a => [a] -> Bool
isUniq = (==) <*> nub

-- findMarkerIndex :: Ord a => Int -> [a] -> Int
findMarkerIndex i =
  fmap fst
    . find (isUniq . snd)
    . zip [i ..]
    . fmap (take i)
    . tails

run :: String -> IO ()
run xs = do
  let signal = xs

  let resA = findMarkerIndex 4 signal
  print resA

  resA @=? Just 1287

  let resB = findMarkerIndex 14 signal
  print resB

  resB @=? Just 3716
