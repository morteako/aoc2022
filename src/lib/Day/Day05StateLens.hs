module Day.Day05StateLens where

import Control.Lens
import Control.Monad.State
import Data.Char qualified as Char
import Data.Foldable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List.Extra (foldl', splitOn, transpose)
import Test.HUnit ((@=?))
import Utils (readInt)

data To = Int :-> Int deriving (Show)

data Move = Move Int To deriving (Show)

type Stack = IntMap String

parse :: [Char] -> (IntMap [Char], [Move])
parse = parseStacksAndMoves . splitOn "\n\n"
 where
  parseStacksAndMoves [stacks, moves] = (parseStacks stacks, fmap parseMove $ lines moves)
  parseStacksAndMoves _ = error ""

  parseStacks x = IntMap.fromList $ zip [1 ..] $ fmap init $ removeNonStacks $ transpose $ lines x
   where
    removeNonStacks = (filter (Char.isLetter . head) . filter (not . null) . fmap (filter (not . Char.isSpace)))

  parseMove (words -> ["move", a, "from", b, "to", c]) = Move (readInt a) (readInt b :-> readInt c)
  parseMove _ = error ""

doMoveSingle :: Move -> State Stack ()
doMoveSingle (Move i (from :-> to)) = replicateM_ i f
 where
  f = do
    top <- head <$> gets (IntMap.! from)
    modify (IntMap.adjust tail from)
    modify (IntMap.adjust (top :) to)

-- Lens
doMoveAll :: Move -> State Stack ()
doMoveAll (Move i (from :-> to)) = do
  tops <- uses (ix from) (take i)
  ix from %= drop i
  ix to %= (tops ++)

rearrangeStacks :: (Move -> State Stack b) -> Stack -> [Move] -> [Char]
rearrangeStacks mover stacks moves = foldMap (take 1) $ execState (traverse_ mover moves) stacks
run :: String -> IO ()
run xs = do
  let (stacks, moves) = parse xs

  let resA = rearrangeStacks doMoveSingle stacks moves
  print resA
  resA @=? "RNZLFZSJH"

  let resB = rearrangeStacks doMoveAll stacks moves
  print resB

  resB @=? "CNSFCGJSM"

-- let resB = rearrangeStacks' doMoveAll' stacks moves
-- print resB

-- resB @=? "CNSFCGJSM"
