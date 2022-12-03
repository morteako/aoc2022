module Day.Day03Monoid where

import Algebra.Lattice (Meet (Meet))
import Control.Lens
import Data.List.Extra (chunksOf)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Test.HUnit ((@=?))



newtype IntersectionSet a = IntersectionSet (Set.Set a)
  deriving (Semigroup, Monoid) via Meet (Set.Set a)
  deriving (Foldable)

toIntersectionSet :: Ord a => [a] -> IntersectionSet a
toIntersectionSet x = IntersectionSet . Set.fromList $ x

priority :: Char -> Integer
priority x = letters Map.! x
  where
    letters = Map.fromList $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 ..]

parseBackpacksInHalves :: String -> [(IntersectionSet Char, IntersectionSet Char)]
parseBackpacksInHalves = fmap into2 . lines
 where
  into2 xs = over each toIntersectionSet $ splitAt (length xs `div` 2) xs

type Triple a = (a, a, a)

toTriple :: [c] -> Triple c
toTriple [a, b, c] = (a, b, c)
toTriple _ = error "toTriple"

parseBackpacksInTriples :: String -> [Triple (IntersectionSet Char)]
parseBackpacksInTriples = fmap (toTriple) . chunksOf 3 . fmap toIntersectionSet . lines

sumOfCommonItemPriorities ::
  ( Each a a (IntersectionSet Char) (IntersectionSet Char)
  ) =>
  [a] ->
  Integer
sumOfCommonItemPriorities x = sumOf (folded . to (foldOf each) . folded . to priority) x

run :: String -> IO ()
run xs = do
  let parsed = parseBackpacksInHalves xs
  let resA = sumOfCommonItemPriorities parsed
  print resA

  resA @=? 8233

  let parsed = parseBackpacksInTriples xs
  let resB = sumOfCommonItemPriorities parsed
  print resB

  resB @=? 2821
