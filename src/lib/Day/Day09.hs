{-# LANGUAGE MultiWayIf #-}

module Day.Day09 (run) where

import Control.Lens (view)
import Data.Containers.ListUtils (nubOrd)
import Data.List
import Data.List.Extra
import Debug.Trace
import Linear
import Test.HUnit ((@=?))
import Utils (readInt)
import Utils qualified

data Dir = R | L | D | U deriving (Read, Enum, Show)

instance Show Dot where
  show (Dot l (V2 x y)) = show l ++ " " ++ show (x, y)

data Move a = a :> Int deriving (Show, Functor)

parse = fmap parseMove . lines
 where
  parseMove (words -> [l, readInt -> n]) = read @Dir l :> n

dirToVec :: Num a => Dir -> V2 a
dirToVec R = V2 1 0
dirToVec L = V2 (-1) 0
dirToVec D = V2 0 (-1)
dirToVec U = V2 0 1

data Label = H | T | Lab Int

instance Show Label where
  show H = "H"
  show T = "T"
  show (Lab n) = show n

data Dot = Dot Label (V2 Int) deriving ()

getDotPos (Dot _ p) = p

un = undefined

debug = False

traceLab s x = if debug then Utils.traceLab s x else x

doDirNew [] ht = traceLab "_____" [ht]
doDirNew ((_ :> 0) : rest) ht = doDirNew rest ht
-- doDirNew (dn : _) ht
--   | traceShow ("hop", dn, ht) False = undefined
doDirNew (d :> (subtract 1 -> n) : rest) ht@(Dot hlab hpos : ts) = ht : doDirNew (d :> n : rest) newHT
 where
  moveDir = dirToVec d
  newHPos = hpos + moveDir
  newH = Dot hlab newHPos

  newHT = getTails hpos newH ts

  getTails prevHead d@(Dot curHeadLab curHeadPos) [] = [d]
  getTails prevHead d@(Dot curHeadLab curHeadPos) (tt@(Dot tlab tpos) : tts) =
    let res =
          if
              | curHeadPos == tpos ->
                  traceLab "same spot - stay" $ Dot tlab tpos
              | isTouching curHeadPos tpos ->
                  traceLab "isTouching stay" $ Dot tlab tpos
              | Just nextTPos <- isTwoDirAway curHeadPos tpos ->
                  traceLab "sameDir *2" $ Dot tlab nextTPos
              | isDoubleDiag curHeadPos tpos ->
                  traceLab "doubleDiag usePrev" $ Dot tlab prevHead
              | otherwise ->
                  error "hmm"
     in d : getTails tpos res tts
doDirNew _ _ = undefined

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

more f = length . nub . fmap f . fmap last

-- solveA moves = more id $ doDirTwo moves [0, 0]

solveA moves = length . nub . fmap getDotPos . fmap last $ doDirNew moves $ [Dot H 0] ++ fmap (\x -> Dot (Lab x) 0) [1]

solveB moves = id $ doDirNew moves $ [Dot H 0] ++ fmap (\x -> Dot (Lab x) 0) [1 .. 9]
 where
  m = length . nub . fmap getDotPos . fmap last

-- f !(cur : (traceLab "cd" -> cs)) (traceLab "f" -> dir) = doDir dir cur ++ cur : cs

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print parsed
  -- print resA

  let resA = solveA parsed
  print resA
  -- mapM_ print $ groupOn (view _x) $ sort $ nub $ fmap snd $ solveA parsed
  -- print "--"
  -- mapM print $ sort $ fmap snd $ solveA parsed

  let resB = solveB parsed
  mapM print resB

  -- mapM_ print $ zip resA resA'

  pure ()