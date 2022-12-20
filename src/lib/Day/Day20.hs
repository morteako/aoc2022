module Day.Day20 (run) where

import Data.Foldable (Foldable (foldl', toList))
import Data.Foldable.Extra (sumOn')
import Data.Sequence qualified as Seq
import Test.HUnit ((@=?))
import Utils (readInt)

newtype StartPos = Pos Int deriving (Num, Enum, Show, Eq, Ord)

parse :: String -> Seq.Seq (StartPos, Int)
parse = Seq.fromList . zip [0 :: StartPos ..] . fmap readInt . lines

move :: (StartPos, Int) -> Seq.Seq (StartPos, Int) -> Seq.Seq (StartPos, Int)
move m@(pos, 0) xs = xs
move m@(pos, n) xs =
  Seq.insertAt newI' m $ Seq.deleteAt i xs
 where
  Just i = Seq.findIndexL (\(p, _) -> p == pos) xs
  newI = n + i
  newI' = (if newI < 0 then l + newI else newI) `mod` l
  l = Seq.length xs - 1

getNs :: (Eq b, Num b) => Seq.Seq (a, b) -> b
getNs xs = sumOn' (snd . Seq.index xs . getNIndex) [1000, 2000, 3000]
 where
  getNIndex n = (n + wrapIndex) `mod` length xs
  Just wrapIndex = Seq.findIndexL (\(_, n) -> n == 0) xs

rotateNTimes :: Maybe Int -> Int -> Seq.Seq (StartPos, Int) -> Int
rotateNTimes decryptionKey n seq = getNs $ (!! n) $ iterate doOne newM
 where
  doOne w = foldl' (flip move) w $ newM
  q = toList newM
  newM = maybe id (\key -> (fmap . fmap) (* key)) decryptionKey seq

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = rotateNTimes Nothing 1 parsed
  print $ resA
  resA @=? 7153

  let resB = rotateNTimes (Just 811589153) 10 parsed
  print resB
  resB @=? 6146976244822
