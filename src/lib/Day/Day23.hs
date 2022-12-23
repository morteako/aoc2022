module Day.Day23 (run) where

import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map hiding (null)
import Data.Maybe
import Data.Semigroup
import Data.Set hiding (drop, take)
import Data.Set qualified as Set hiding (drop, null, take)
import Debug.Trace
import Linear hiding (E, trace)
import Test.HUnit ((@=?))
import Utils (printMap, traceLab)

type Point = V2 Int

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton
 where
  asciiGrid :: IndexedFold Point String Char
  asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

parse = Map.keysSet . parseAsciiMap parseElf
 where
  parseElf '#' = Just ()
  parseElf _ = Nothing

data Dir = N | S | W | E deriving (Show, Enum, Eq)

getDirV :: Dir -> V2 Int
getDirV N = V2 0 (-1)
getDirV S = V2 0 1
getDirV W = V2 (-1) 0
getDirV E = V2 1 0

ords = [([N, W, E], N), ([S, W, E], S), ([N, S, W], W), ([N, S, E], E)]

cycords = cycle ords

cyc (flip mod 4 -> id -> n) xs =
  drop n $ take (4 + n) $ cycle xs

getNeighs :: Int -> V2 Int -> Set (V2 Int) -> _
getNeighs i v m = if length r == 4 then Nothing else fmap fst $ asum $ res
 where
  r = catMaybes res
  res = fmap (traverse $ traverse f) $ cyc i $ getNeighs' v
  g xs = if length (catMaybes xs) == 4 then Nothing else Just (xs)

  f d | Set.member d m = Nothing
  f d = Just ()

getNeighs' :: V2 Int -> [(Dir, [V2 Int])]
getNeighs' v@(V2 x y) = do
  (gs, dir) <- ords
  let dv = getDirV dir

  pure $ (,) dir $ do
    d <- gs
    pure $
      if d == dir
        then v + getDirV d
        else v + getDirV d + dv

-- findNewDir d m =
--   where
--     ns = getNeighs d m

-- t = (fromList [V2 0 2, V2 1 5, V2 2 0, V2 3 3, V2 4 1])

-- tt = move 3 (V2 2 5) (fromList [V2 1 2, V2 2 1, V2 2 5, V2 3 1, V2 4 3])

move i v m = case getNeighs i v m of
  Nothing -> v
  Just d -> getDirV d + v

-- delRem m s = Set.fromList (Map.elems m) <> Set.difference s (Map.keysSet m)

oneRound (i, m) = (i + 1, Map.foldMapWithKey f newPoses)
 where
  f k [x] = Set.singleton k
  f _ xs = Set.fromList xs

  -- newValidPos = Map.mapMaybe onlySingl newPoses
  -- onlySingl [x] = Just x
  -- onlySingl _ = Nothing
  newPoses = Map.unionsWith (++) $ foldMap (\k -> [Map.singleton (move i k m) [k]]) m

getEdges :: Set (V2 Int) -> (Int, Int, Int, Int)
getEdges = coerce . foldMap f
 where
  f (V2 x y) = (Min x, Max x, Min y, Max y)

makeMap s = foldMap (\v -> Map.singleton v True) s <> Map.fromList (fmap (,False) coords)
 where
  coords = V2 <$> [ia .. ax] <*> [iy .. ay]
  (ia, ax, iy, ay) = getEdges s

solveA grid = (count, c, res) -- last $ take 11 $ iterate oneRound (0, grid)
 where
  (c, res) = last $ take 11 $ iterate oneRound (0, grid)
  i@(ia, ax, iy, ay) = getEdges res

  rect = (1 + ax - ia) * (1 + ay - iy)
  count = rect - Set.size res

solveB grid = asum $ ps
 where
  ps = (zipWith f <*> tail) $ iterate oneRound (0, grid)
  f (_, xs) (i, ys) | xs == ys = Just i
  f _ _ = Nothing

-- -- solveB = id

toDots True = '#'
toDots False = '.'

prMap = printMap . fmap toDots . makeMap . snd

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  -- mapM_ print resA

  print resA

  print "----"

  -- print tt

  -- mapM_ print $ fmap cyc [1 .. 10]

  -- resA @=? 1715
  let resB = solveB parsed
  print resB

-- resB @=? 1739
