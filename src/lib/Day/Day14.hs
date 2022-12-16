module Day.Day14 (run) where

import Test.HUnit ((@=?))

import Control.Lens
import Data.List.Extra
import Data.List.Split (splitOneOf)
import Data.Map (Map)
import Data.Map qualified as Map

import Control.Monad
import Data.Maybe
import Debug.Trace
import Linear

data Stru = Rock | MovSand | RestSand | Air deriving (Eq, Ord)

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map (V2 Int) a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton
 where
  asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

instance Show Stru where
  show :: Stru -> String
  show Rock = "#"
  show MovSand = "+"
  show RestSand = "o"
  show Air = "."

sandPoint = V2 500 0

parse = fmap (fmap (toCoord . splitOn ",") . splitOn " -> ") . lines
 where
  toCoord [a, b] = V2 (read @Int a) (read b)
  toCoord xs = error $ show xs

getDir a b = fmap (negate . mami1) (a - b)

makeLine :: Point -> Point -> [Point]
makeLine a b = go a b
 where
  d = getDir a b
  go m n | m == n = [m]
  go m n = m : go (m + d) n

mami1 x = max (-1) (min 1 x)

type Point = V2 Int

type Grid = G (Map Point Stru)

data G g = g :~ Int deriving (Functor, Show)

getGrid (g :~ _) = g

makeGridFromLines :: [[Point]] -> Grid
makeGridFromLines rockLines = ((Map.fromList $ (,Rock) <$> concatMap oneLine rockLines) :~ ylim)
 where
  ss = concatMap oneLine rockLines
  ylim = maximum $ fmap (view _y) ss
  oneLine rl = concatMap f $ zip rl (tail rl)
  f (a, b) = makeLine a b

-- data SandGrid = Point :=> Map Point Stru

oneDown = (+ V2 0 1)
oneLeft = (+ V2 (-1) 0)
oneRight = (+ V2 1 0)

moveOne = moveOneSand sandPoint

moveOneSand :: V2 Int -> Grid -> Either Grid Grid
moveOneSand (V2 _ y) g@(_ :~ lim) | y > lim = Left g
moveOneSand p g@(grid :~ _) = case mapMaybe check moves of
  [] ->
    (if p == V2 500 0 then Left else Right) $ Map.insert p RestSand <$> g
  k : _ -> moveOneSand k g
 where
  moves = [next, nextLeft, nextRight]

  next = oneDown p
  nextLeft = oneLeft $ oneDown p
  nextRight = oneRight $ oneDown p

  check k = case Map.lookup k grid of
    Nothing -> Just k
    Just Air -> Just k
    Just _ -> Nothing

untilLeft f (Left x) = x
untilLeft f (Right x) = untilLeft f $ f x

solveA = Map.size . Map.filter (== RestSand) . getGrid . untilLeft moveOne . Right . makeGridFromLines

solveB = id

run :: String -> IO ()
run xs = do
  print xs
  let parsed = parse xs
  print parsed
  let resA = solveA parsed
  -- mapM_ print $ Map.toList resA
  print resA

-- print $ makeLine (V2 3 2) (V2 1 2)
-- print $ makeLine (V2 3 0) (V2 3 5)

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
