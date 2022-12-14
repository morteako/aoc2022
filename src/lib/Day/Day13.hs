{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Day.Day13 (run) where

import Control.Lens
import Data.List (elemIndex, sort)
import Data.List.Extra (chunksOf, sumOn')
import Data.Semigroup (Product (Product))
import Day.Day13TH (listsFromTH)
import GHC.Exts (IsList (..))
import Test.HUnit ((@=?))

data P = S Int | L [P] deriving (Eq, Show)

instance Num P where
  fromInteger = S . fromInteger

instance IsList P where
  type Item P = P

  fromList = L

instance Ord P where
  compare :: P -> P -> Ordering
  compare (S n) (S i) = compare n i
  compare (S n) (L ps) = compare (L [S n]) (L ps)
  compare (L ps) (S n) = compare (L ps) (L [S n])
  compare (L ps) (L ps') = compare ps ps'

solveA :: [P] -> Int
solveA = sumOf (to (chunksOf 2) . ifolded . filtered f . asIndex . to succ)
 where
  f [a, b] = a <= b

divs :: [P]
divs = [[[2]], [[6]]]

solveB :: [P] -> Maybe Int
solveB ((++ divs) -> sort -> res) = product . fmap succ <$> traverse (\x -> elemIndex x res) divs

inputLists :: [P]
inputLists = $(listsFromTH)

run :: String -> IO ()
run _ = do
  let resA = solveA inputLists
  print resA
  resA @=? 5684

  let resB = solveB inputLists
  print resB
  resB @=? Just 22932
