module Day.Day09 (run) where

import Data.Containers.ListUtils (nubOrd)
import Linear (V2 (..))
import Test.HUnit ((@=?))

-- Horrible stuff 🙃

data Dir = R | L | D | U deriving (Read, Enum, Show)

data Move = Dir :> Int deriving (Show)

parseMoves :: String -> [Move]
parseMoves = fmap parseMove . lines
 where
  parseMove (words -> [l, read -> n]) = read l :> n

dirToVec :: Num a => Dir -> V2 a
dirToVec R = V2 1 0
dirToVec L = V2 (-1) 0
dirToVec D = V2 0 (-1)
dirToVec U = V2 0 1

doDirNew :: [Move] -> [V2 Int] -> [[V2 Int]]
doDirNew [] ht = [ht]
doDirNew ((_ :> 0) : rest) ht = doDirNew rest ht
doDirNew (d :> (subtract 1 -> n) : rest) ht@(hpos : ts) = ht : doDirNew (d :> n : rest) newHT
 where
  newHPos = hpos + dirToVec d
  newH = newHPos

  newHT = getTails hpos newH ts

  getTails prevHead curHeadPos [] = [curHeadPos]
  getTails prevHead curHeadPos (tpos : tts) =
    let res =
          if
              | newHPos == tpos ->
                  tpos
              | curHeadPos == tpos ->
                  tpos
              | prevHead == curHeadPos ->
                  tpos
              | prevHead == tpos ->
                  tpos
              | Just nextTPos <- isTwoDirAway curHeadPos tpos ->
                  nextTPos
              | isLineTouch curHeadPos tpos ->
                  tpos
              | Just dirV <- isDiag prevHead curHeadPos ->
                  tpos + dirV
              | isTouching curHeadPos tpos ->
                  tpos
              | isDoubleDiag curHeadPos tpos ->
                  prevHead
              | otherwise -> undefined
     in curHeadPos : getTails tpos res tts
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

countLastTailPoses :: [[V2 Int]] -> Int
countLastTailPoses = length . nubOrd . fmap last

countTailPositions :: [Move] -> Int -> Int
countTailPositions moves numTails = countLastTailPoses $ doDirNew moves $ 0 : fmap (const 0) [1 .. numTails]

run :: String -> IO ()
run xs = do
  let parsed = parseMoves xs

  let resA = countTailPositions parsed 1
  print resA
  resA @=? 6269

  let resB = countTailPositions parsed 9
  print resB

  resB @=? 2557