{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day.Day14 (run) where

import Control.Lens
import Data.List.Extra (splitOn)
import Data.List.Split (splitOneOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (Max (Max))
import Linear
import Test.HUnit ((@=?))

data Stru = Rock | RestSand deriving (Eq, Ord)

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map (V2 Int) a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton
 where
  asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

parseIntoGrid :: Part' part => String -> Grid part
parseIntoGrid = makeGridFromRockCoords . parseRockCoords

parseRockCoords :: String -> [[V2 Int]]
parseRockCoords = fmap (fmap (toCoord . splitOn ",") . splitOn " -> ") . lines
 where
  toCoord (fmap read -> [a, b]) = V2 a b
  toCoord xs = error $ show xs

makeLine :: Point -> Point -> [Point]
makeLine a b = go a b
 where
  d = fmap (negate . mami1) (a - b)
  go m n | m == n = [m]
  go m n = m : go (m + d) n

mami1 x = max (-1) (min 1 x)

type Point = V2 Int

type Grid (part :: Part) = G part (Map Point Stru)

data G (part :: Part) g = g :~ Int deriving (Functor, Show)

data Part = Part1 | Part2 deriving (Eq)

class Part' a where
  partFallForever :: Int -> Int -> Bool
  partFloorCheck :: Int -> Int -> Bool

instance Part' Part1 where
  partFloorCheck _ _ = False
  partFallForever y lim = y > (lim)

-- test

instance Part' Part2 where
  partFloorCheck y lim = y + 1 == (lim + 2)
  partFallForever _ _ = False

getGrid :: G p g -> g
getGrid (g :~ _) = g

makeGridFromRockCoords :: [[Point]] -> Grid part
makeGridFromRockCoords rockLines = ((Map.fromList $ (,Rock) <$> rockCoords) :~ ylim)
 where
  rockCoords = concatMap oneLine rockLines
  Max ylim = foldMap (Max . view _y) rockCoords
  oneLine rl = concat $ zipWith makeLine rl (tail rl)

oneDown, oneLeft, oneRight :: V2 Int -> V2 Int
oneDown = (+ V2 0 1)
oneLeft = (+ V2 (-1) 0)
oneRight = (+ V2 1 0)

moveOne :: forall p. Part' p => (Grid p) -> Either (Grid p) (Grid p)
moveOne = moveOneSand (V2 500 0)
 where
  moveOneSand (V2 _ y) g@(_ :~ lim) | partFallForever @p y lim = Left g
  moveOneSand p@(V2 _ y) g@(grid :~ lim) = case mapMaybe check moves of
    [] ->
      (if p == V2 500 0 then Left else Right) $ Map.insert p RestSand <$> g
    k : _ ->
      moveOneSand k g
   where
    moves = [next, nextLeft, nextRight]

    next = oneDown p
    nextLeft = oneLeft $ oneDown p
    nextRight = oneRight $ oneDown p

    check k | partFloorCheck @p y lim = Nothing
    check k | Map.member k grid = Nothing
    check k = Just k

-- hmm. is this a lib function?
untilLeft :: (t -> Either a t) -> Either a t -> a
untilLeft f (Left x) = x
untilLeft f (Right x) = untilLeft f $ f x

countSands :: Part' p => Grid p -> Int
countSands = Map.size . Map.filter (== RestSand) . getGrid . untilLeft moveOne . Right

run :: String -> IO ()
run xs = do
  let parsedA = parseIntoGrid xs
  let resA = countSands @Part1 parsedA
  print resA
  resA @=? 897

  let parsedB = parseIntoGrid xs
  let resB = countSands @Part2 parsedB
  print resB

  resB @=? 26683
