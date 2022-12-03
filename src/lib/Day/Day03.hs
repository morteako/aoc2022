module Day.Day03 where

import Control.Lens
import Data.List.Extra (chunksOf)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Test.HUnit ((@=?))

letters :: Map.Map Char Integer
letters = Map.fromList $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 ..]

priority :: Char -> Integer
priority x = letters Map.! x

parseBackpacksInHalves :: String -> [(Set.Set Char, Set.Set Char)]
parseBackpacksInHalves = fmap into2 . lines
 where
  into2 xs = over each Set.fromList $ splitAt (length xs `div` 2) xs

sumOfCommonItemPrioritiesInHalves :: [(Set.Set Char, Set.Set Char)] -> Integer
sumOfCommonItemPrioritiesInHalves = sumOf (folded . folded . to priority) . fmap (uncurry Set.intersection)

parseBackpacksInTriples :: String -> [[Set.Set Char]]
parseBackpacksInTriples = chunksOf 3 . fmap Set.fromList . lines

sumOfCommonItemPrioritiesInTriples :: [[Set.Set Char]] -> Integer
sumOfCommonItemPrioritiesInTriples = sumOf (folded . folded . to priority) . fmap (foldr1 Set.intersection)

run :: String -> IO ()
run xs = do
  let parsed = parseBackpacksInHalves xs
  let resA = sumOfCommonItemPrioritiesInHalves parsed
  print resA

  resA @=? 8233

  let parsed = parseBackpacksInTriples xs
  let resB = sumOfCommonItemPrioritiesInTriples parsed
  print resB

  resB @=? 2821
