module Day.Day07 (run) where

import Data.List
import Data.List.Extra (groupBy, splitOn, stripPrefix, sumOn')
import Data.Map (Map)
import Data.Map qualified as Map

import Test.HUnit ((@=?))

data FileDir = File Int String | Dir String deriving (Show)

data Path' = DotDot | Outer | Specific String deriving (Show)

data Execution = CD Path' | LS [FileDir] deriving (Show)

data Path = Root | Path :/ String deriving (Eq, Ord)

moveUp :: Path -> Path
moveUp Root = Root
moveUp (p :/ _) = p

parse :: String -> [Execution]
parse = fmap parseExec . groupBy f . lines
 where
  f _ b = not (isPrefixOf "$" b)

  parseExec [s]
    | Just path <- stripPrefix "$ cd " s =
        CD $ case path of
          "/" -> Outer
          ".." -> DotDot
          _ -> Specific path
  parseExec ("$ ls" : rest) = LS $ fmap parseFileDir rest
  parseExec s = error $ show s

  parseFileDir s
    | Just d <- stripPrefix "dir " s = Dir d
    | [size, name] <- words s =
        File (read size) name
  parseFileDir s = error $ show s

makeMap :: [Execution] -> Map Path [FileDir]
makeMap = Map.fromList . getPaths' Root
 where
  getPaths' curPath [] = []
  getPaths' curPath ((LS fds) : rest) = (curPath, fds) : getPaths' curPath rest
  getPaths' curPath ((CD pa) : rest) = getPaths' newP rest
   where
    newP = case pa of
      DotDot -> moveUp curPath
      Outer -> Root
      Specific s -> curPath :/ s

countSize :: Path -> Map Path [FileDir] -> Int
countSize path m
  | Just fd <- Map.lookup path m = sumOn' f fd
 where
  f (File i _) = i
  f (Dir s) = countSize (path :/ s) m
countSize _ _ = error ""

sumOfSmallDirs :: [Execution] -> Int
sumOfSmallDirs = sum . filter (<= 100000) . getSizes . makeMap
 where
  getSizes m = fmap (\path -> countSize path m) $ Map.keys m

findSmallestDirSizeThatFreesSpace :: [Execution] -> Maybe Int
findSmallestDirSizeThatFreesSpace execs = getFirst sizes
 where
  getFirst = find (\x -> x + targetMin >= 30000000)
  targetMin = 70000000 - maximum sizes

  fileMap = makeMap execs

  sizes = sort $ fmap (\path -> countSize path fileMap) $ Map.keys fileMap

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = sumOfSmallDirs parsed
  print resA

  resA @=? 1118405
  let resB = findSmallestDirSizeThatFreesSpace parsed
  print resB

  resB @=? Just 12545514
