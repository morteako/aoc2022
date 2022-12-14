module Day.Day14 (run) where

import Test.HUnit ((@=?))

import Control.Lens
import Data.List.Extra
import Data.List.Split (splitOneOf)
import Data.Map
import Data.Map qualified as Map
import GHC.RTS.Flags (MiscFlags (installSEHHandlers))
import Linear

data Stru = Rock | Sand | Air deriving (Eq, Ord)

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
  show Sand = "+"
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

makeGridFromLines :: [[Point]] -> Map Point Stru
makeGridFromLines rockLines = Map.fromList $ (,Rock) <$> concatMap oneLine rockLines
 where
  oneLine rl = concatMap f $ zip rl (tail rl)
  f (a, b) = makeLine a b

solveA = makeGridFromLines

solveB = id

run :: String -> IO ()
run xs = do
  print xs
  let parsed = parse xs
  print parsed
  let resA = solveA parsed
  print resA

  print $ makeLine (V2 3 2) (V2 1 2)
  print $ makeLine (V2 3 0) (V2 3 5)

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
