module Day.Day08 (run) where

import Control.Lens
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup (Sum (Sum))
import Linear (V2 (..))
import Test.HUnit ((@=?))

type Point = V2 Int

data Grid = Grid Point (Map Point Int) deriving (Show)

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton
 where
  asciiGrid :: IndexedFold Point String Char
  asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

parseGrid :: String -> Grid
parseGrid = getMaxes . parseAsciiMap (Just . digitToInt)
 where
  getMaxes m = Grid (get m) m
  get (Map.maxViewWithKey -> Just ((k, _), _)) = k

getAllSides :: (Num a, Enum a) => V2 a -> V2 a -> [[V2 a]]
getAllSides (V2 maxx maxy) (V2 px py) =
  reverse (fmap (V2 px) [0 .. py - 1])
    : fmap (V2 px) [py + 1 .. maxy]
    : reverse (fmap (flip V2 py) [0 .. px - 1])
    : [fmap (flip V2 py) [px + 1 .. maxy]]

takeWhileInclude :: (a -> Bool) -> [a] -> [a]
takeWhileInclude p (span p -> (ps, ns)) =
  case ns of
    [] -> ps
    (n : _) -> ps ++ [n]

solveA :: Grid -> Sum Int
solveA g@(Grid maxXY grid) = foldMap (Sum . fromEnum) $ Map.mapWithKey f grid
 where
  f p height
    | let sides = fmap (fmap (grid Map.!)) $ getAllSides maxXY p =
        any (all (height >)) sides

solveB :: Grid -> Int
solveB g@(Grid maxXY grid) = maximum $ Map.mapWithKey f grid
 where
  f p height
    | let sides = fmap (fmap (grid Map.!)) $ getAllSides maxXY p =
        (product $ fmap (length . takeWhileInclude (height >)) sides)

run :: String -> IO ()
run xs = do
  let parsed = parseGrid xs

  let resA = solveA parsed
  print resA
  resA @=? 1792

  let resB = solveB parsed
  print resB
  resB @=? 334880
