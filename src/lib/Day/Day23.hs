module Day.Day23 (run) where

import Control.Lens
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.HashSet (HashSet)
import Data.HashSet as Set
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map hiding (null)
import Data.Maybe (isJust)
import Data.Semigroup (Max (Max), Min (Min))
import Data.Set (Set)
import Linear (V2 (..))
import Test.HUnit ((@=?))

type Point = V2 Int

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton
 where
  asciiGrid :: IndexedFold Point String Char
  asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

parse = Set.fromList . Map.keys . parseAsciiMap parseElf
 where
  parseElf '#' = Just ()
  parseElf _ = Nothing

data Dir = N | S | W | E deriving (Show, Enum, Eq)

getDirV :: Dir -> V2 Int
getDirV N = V2 0 (-1)
getDirV S = V2 0 1
getDirV W = V2 (-1) 0
getDirV E = V2 1 0

ords :: [([Dir], Dir)]
ords = [([N, W, E], N), ([S, W, E], S), ([N, S, W], W), ([N, S, E], E)]

cyc :: Int -> [a] -> [a]
cyc (flip mod 4 -> id -> n) xs =
  drop n $ take (4 + n) $ cycle xs

getNeighs :: Int -> V2 Int -> HashSet (V2 Int) -> _
getNeighs i v m = if all isJust res then Nothing else fmap fst $ asum $ res
 where
  res = fmap (traverse $ traverse f) $ cyc i $ getNeighs' v

  f d | Set.member d m = Nothing
  f d = Just ()

getNeighs' :: V2 Int -> [(Dir, [V2 Int])]
getNeighs' v@(V2 x y) = fmap f ords
 where
  f (gs, dir) = (dir, fmap (g dir) gs)

  g dir d | d == dir = v + getDirV d
  g dir d = v + getDirV d + getDirV dir

oneRound :: (Int, HashSet (V2 Int)) -> (Int, HashSet (V2 Int))
oneRound (i, m) = (i + 1, Map.foldMapWithKey f newPoses)
 where
  f k [x] = Set.singleton k
  f _ xs = Set.fromList xs

  newPoses = Map.unionsWith (++) $ foldMap (\k -> [Map.singleton (move k) [k]]) m

  move v = case getNeighs i v m of
    Nothing -> v
    Just d -> getDirV d + v

getEdges :: HashSet (V2 Int) -> (Int, Int, Int, Int)
getEdges = coerce . foldMap f
 where
  f (V2 x y) = (Min x, Max x, Min y, Max y)

solveA :: HashSet (V2 Int) -> Int
solveA grid = rect - Set.size res
 where
  (_, res) = (!! 10) $ iterate oneRound (0, grid)
  (ia, ax, iy, ay) = getEdges res

  rect = (1 + ax - ia) * (1 + ay - iy)

solveB :: HashSet (V2 Int) -> Maybe Int
solveB grid = fmap succ $ List.findIndex (uncurry (==)) $ ps
 where
  ps = (zip <*> tail) $ snd <$> iterate oneRound (0, grid)

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 3762

  let resB = solveB parsed
  print resB
  resB @=? Just 997
