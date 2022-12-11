module Day.Day09 (run) where

import Control.Lens (view)
import Data.Containers.ListUtils (nubOrd)
import Data.Map qualified as Map
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Utils (readInt)
import Utils qualified

data Dir = R | L | D | U deriving (Read, Enum, Show)

data Move = Dir :> Int deriving (Show)

parseMoves :: String -> [Move]
parseMoves = fmap parseMove . lines
 where
  parseMove (words -> [l, readInt -> n]) = read @Dir l :> n

dirToVec :: Num a => Dir -> V2 a
dirToVec R = V2 1 0
dirToVec L = V2 (-1) 0
dirToVec D = V2 0 (-1)
dirToVec U = V2 0 1

data Label = H | T | Lab Int deriving (Eq, Ord)

instance Show Label where
  show H = "H"
  show T = "T"
  show (Lab n) = show n

data Dot = Dot Label (V2 Int)

doDirNew :: [Move] -> [Dot] -> [[Dot]]
doDirNew [] ht = [ht]
doDirNew ((_ :> 0) : rest) ht = doDirNew rest ht
doDirNew ddd@(d :> (subtract 1 -> n) : rest) ht@(Dot hlab hpos : ts) = ht : doDirNew (d :> n : rest) newHT
 where
  newHPos = hpos + dirToVec d
  newH = Dot hlab newHPos

  newHT = getTails hpos newH ts

  getTails prevHead d@(Dot _ curHeadPos) [] = [d]
  getTails prevHead d@(Dot _ curHeadPos) (tt@(Dot tlab tpos) : tts) =
    let res =
          if
              | newHPos == tpos ->
                  Dot tlab tpos
              | curHeadPos == tpos ->
                  Dot tlab tpos
              | prevHead == curHeadPos ->
                  Dot tlab tpos
              | prevHead == tpos ->
                  Dot tlab tpos
              | Just nextTPos <- isTwoDirAway curHeadPos tpos ->
                  Dot tlab nextTPos
              | isLineTouch curHeadPos tpos ->
                  Dot tlab tpos
              | Just dirV <- isDiag prevHead curHeadPos ->
                  Dot tlab (tpos + dirV)
              | isTouching curHeadPos tpos ->
                  Dot tlab tpos
              | isDoubleDiag curHeadPos tpos ->
                  Dot tlab prevHead
              | otherwise -> undefined
     in d : getTails tpos res tts
doDirNew _ _ = undefined

isDiag a b = case abs (a - b) of
  V2 1 1 ->
    Just $ fmap (negate) (a - b)
  _ -> Nothing

isLineTouch a b = case abs (a - b) of
  V2 1 0 -> True
  V2 0 1 -> True
  _ -> False

isTouching a b = case abs (a - b) of
  V2 1 1 ->
    True
  V2 1 0 -> True
  V2 0 1 -> True
  _ -> False

isDoubleDiag a b = case abs (a - b) of
  V2 2 1 -> True
  V2 1 2 -> True
  _ -> False

isTwoDirAway a b = case (a - b) of
  V2 2 0 -> Just $ a - V2 1 0
  V2 (-2) 0 -> Just $ a + V2 1 0
  V2 0 (-2) -> Just $ a + V2 0 1
  V2 0 2 -> Just $ a - V2 0 1
  _ -> Nothing

countLastTailPoses :: [[Dot]] -> Int
countLastTailPoses = length . nubOrd . fmap getDotPos . fmap last
 where
  getDotPos (Dot _ p) = p

countTailPositions :: [Move] -> Int -> Int
countTailPositions moves numTails = countLastTailPoses $ doDirNew moves $ [Dot H 0] ++ fmap (\x -> Dot (Lab x) 0) [1 .. numTails]

run :: String -> IO ()
run xs = do
  let parsed = parseMoves xs

  let resA = countTailPositions parsed 1
  print resA
  resA @=? 6269

  let resB = countTailPositions parsed 9
  print resB

  resB @=? 2557