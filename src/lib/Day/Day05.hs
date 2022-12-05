module Day.Day05 where

import Data.Char qualified as Char
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List.Extra (foldl', splitOn, transpose)
import Test.HUnit ((@=?))
import Utils (readInt)

data To = Int :-> Int deriving (Show)

data Move = Move Int To deriving (Show)

parse :: [Char] -> (IntMap [Char], [Move])
parse = parseStacksAndMoves . splitOn "\n\n"
 where
  parseStacksAndMoves [stacks, moves] = (parseStacks stacks, fmap parseMove $ lines moves)
  parseStacksAndMoves _ = error ""

  parseStacks x = IntMap.fromList $ zip [1 ..] $ fmap init $ removeNonStacks transpose $ lines x
   where
    removeNonStacks = fmap (filter (Char.isLetter . head) . filter (not . null) . fmap (filter $ not . Char.isSpace))

  parseMove (words -> ["move", a, "from", b, "to", c]) = Move (readInt a) (readInt b :-> readInt c)
  parseMove _ = error ""

doMoveSingle :: Move -> IntMap [a] -> IntMap [a]
doMoveSingle (Move i (from :-> to)) stack = foldl' f stack [1 .. i]
 where
  f curStacks _
    | (top : _) <- curStacks IntMap.! from
    , s' <- IntMap.adjust tail from curStacks =
        IntMap.adjust (top :) to s'
    | otherwise = error ""

doMoveAll :: Move -> IntMap [a] -> IntMap [a]
doMoveAll (Move i (from :-> to)) stack
  | tops <- take i $ stack IntMap.! from
  , s' <- IntMap.adjust (drop i) from stack =
      IntMap.adjust (tops ++) to s'

rearrangeStacks mover stacks moves = foldMap (take 1) $ foldl' (flip mover) stacks moves

run :: String -> IO ()
run xs = do
  let (stacks, moves) = parse xs

  let resA = rearrangeStacks doMoveSingle stacks moves
  print resA
  resA @=? "RNZLFZSJH"

  let resB = rearrangeStacks doMoveAll stacks moves
  print resB

  resB @=? "CNSFCGJSM"
