module Day.Day12 (run) where

import Control.Lens (
  folded,
  folding,
  ifoldMapOf,
  lined,
  reindexed,
  (<.),
  (<.>),
 )
import Data.Foldable
import Data.Graph.Inductive (Graph (mkGraph))
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query (sp)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Letter (Letter, toLetter)
import Linear (V2 (..))
import Test.HUnit ((@=?))

type Point = V2 Int

data S = Start | End | Char Char deriving (Show, Eq)

getVal :: S -> Char
getVal Start = 'a'
getVal End = 'z'
getVal (Char c) = c

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton
 where
  asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

parse :: String -> Map Point S
parse s = grid
 where
  grid = parseAsciiMap (Just . f) s
  f 'S' = Start
  f 'E' = End
  f x = Char x

get3 (_, _, x) = x

toTup (x, y, _) = (x, y)

data StartEnd = [Int] :=> Int

makeGraph :: [S] -> Map (V2 Int) _ -> (Gr (V2 Int) Int, _)
makeGraph startSymbols grid =
  ( mkGraph (swap <$> Map.toList nodeMap) (fmap (makeLedge . toTup) <$> remNotValid . fold $ gridWithHeighDiffs)
  , start :=> end
  )
 where
  charGrid = fmap getVal grid
  remNotValid = filter ((<= 1) . get3)
  gridWithHeighDiffs = Map.mapWithKey getHeighDiffs charGrid
  getHeighDiffs k v = mapMaybe (\neigh -> (\nV -> diff k neigh v nV) <$> Map.lookup neigh charGrid) $ neighCoords k

  diff k newK curV newV = (k, newK, diff' curV newV)
  diff' b a = fromEnum a - fromEnum b

  makeLedge (x, y) = (nodeMap Map.! x, nodeMap Map.! y, 1)

  start' = Map.keys $ Map.filter (`elem` startSymbols) grid
  (end', _) = Map.findMin $ Map.filter (== End) grid

  start = (nodeMap Map.!) <$> start'
  end = nodeMap Map.! end'

  nodeMap = Map.fromList $ flip zip [0 ..] $ Map.keys charGrid

neighCoords :: V2 Int -> [V2 Int]
neighCoords v2 = (v2 +) <$> [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]

getShortestPathtoEnd :: [S] -> Map (V2 Int) S -> Int
getShortestPathtoEnd startSymbols grid = minimum $ fmap (pred . length) res
 where
  res = mapMaybe (\s -> sp s end g) starts
  (g, starts :=> end) = makeGraph startSymbols grid

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = getShortestPathtoEnd [Start] parsed
  print $ resA
  resA @=? 528

  let resB = getShortestPathtoEnd [Start, Char 'a'] parsed
  print resB
  resB @=? 522
