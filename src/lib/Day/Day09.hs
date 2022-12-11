{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Day.Day09 (run) where

import Control.Lens (view)
import Data.Containers.ListUtils (nubOrd)
import Data.List
import Data.List.Extra
import Data.Map qualified as Map
import Debug.Trace
import Linear
import Test.HUnit ((@=?))
import Utils (readInt)
import Utils qualified

data Dir = R | L | D | U deriving (Read, Enum, Show)

instance Show Dot where
  show (Dot l (V2 x y)) = "D" ++ show l ++ " " ++ show (x, y)

data Move a = a :> Int deriving (Show, Functor)

parse = fmap parseMove . lines
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

data Dot = Dot Label (V2 Int) deriving ()

getDotPos (Dot _ p) = p

doDirNew [] ht = [ht]
doDirNew ((_ :> 0) : rest) ht = doDirNew rest ht
doDirNew ddd@(d :> (subtract 1 -> n) : rest) ht@(Dot hlab hpos : ts) = ht : doDirNew (d :> n : rest) newHT
 where
  moveDir = dirToVec d
  newHPos = hpos + moveDir
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
              | otherwise -> error $ "hmm" ++ show (ddd, prevHead, curHeadPos, tt)
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

-- \| view _x h ==

more = length . nub . fmap getDotPos . fmap last

-- solveA moves = more id $ doDirTwo moves [0, 0]

solveA moves = length . nub . fmap getDotPos . fmap last $ doDirNew moves $ [Dot H 0] ++ fmap (\x -> Dot (Lab x) 0) [1]

solveB moves = more $ doDirNew moves $ [Dot H 0] ++ fmap (\x -> Dot (Lab x) 0) [1 .. 9]

-- f !(cur : (traceLab "cd" -> cs)) (traceLab "f" -> dir) = doDir dir cur ++ cur : cs

makeMap :: [Dot] -> Map.Map (V2 Int) [Label]
makeMap = Map.unionWith (<>) defMap . Map.fromListWith (<>) . fmap (\(Dot l pos) -> (pos, pure l))

defMap = Map.insertWith (++) 0 [T] $ Map.fromList $ fmap (,[]) xs
 where
  xs = V2 <$> [-11 .. 14] <*> [-5 .. 15]

  limU = 5
  limD = 5

fp x = case x of
  [] -> "."
  [T] -> "s"
  labs -> show $ minimum labs

printMap :: Map.Map (V2 Int) [Label] -> IO ()
printMap m = do
  putStrLn "--------"
  let xs = Map.toList $ Map.mapKeys (\(V2 x y) -> V2 y x) m
  let g = groupOn (\(V2 x y, _) -> x) xs
  let gg = reverse $ (fmap . concatMap) (fp . snd) g

  mapM_ (print . sort) $ Map.filter (\x -> length x > 1) m
  mapM_ putStrLn gg
  putStrLn ""

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 6269

  let resB = solveB parsed
  print resB

  resB @=? 2557