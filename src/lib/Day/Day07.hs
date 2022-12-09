{-# LANGUAGE GADTs #-}

module Day.Day07 where

import Control.Lens
import Data.List
import Data.List.Extra (groupBy, splitOn, stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace
import Test.HUnit ((@=?))

data FileDir a = File Int String | Dir a deriving (Show)

data Path = DotDot | Outer | Specific String deriving (Show)

data Execution a = CD Path | LS [FileDir a] deriving (Show)

sDollar ('$' : _) = True
sDollar _ = False

parse :: String -> [Execution String]
parse = fmap parseExec . groupBy f . lines
 where
  f _ b = not (sDollar b)

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

-- newtype M = M (Map String (FileDir M)) deriving (Show)

getPaths = getPaths' []
 where
  getPaths' curPath [] = []
  getPaths' curPath ((LS fds) : rest) = (curPath, fds) : getPaths' curPath rest
  getPaths' curPath ((CD pa) : rest) = getPaths' newP rest
   where
    newP = case pa of
      DotDot -> (tail curPath)
      Outer -> []
      (Specific s) -> (s : curPath)

makeMap = Map.fromList . getPaths

countSize :: [String] -> Map [String] [FileDir String] -> Int
countSize path m
  | Just fd <- Map.lookup path m =
      sum $ fmap f fd
 where
  f (File i _) = i
  f (Dir s) = countSize (s : path) m
countSize _ _ = error ""

-- \| otherwise = error $ show (path,m)
-- where
--   f k (Dir d) = countSizes ((d:k) Map.! m)

solveA = sum . filter (<= 100000) . q . makeMap
 where
  q m = fmap (flip countSize m) $ Map.keys m

maxFolder = q . makeMap
 where
  q m = countSize [] m

solveB execs = (getFirst . q . makeMap $ execs, targetMin)
 where
  getFirst = head . filter (\x -> x + targetMin >= 30000000)
  targetMin = 70000000 - traceShowId (maxFolder execs)

  q m = sort $ fmap (flip countSize m) $ Map.keys m

type MM a = Map Int a

run :: String -> IO ()
run xs = do
  -- print xs
  let parsed = parse xs
  mapM_ print parsed

  mapM_ print $ getPaths parsed
  print $ makeMap parsed

  let resA = solveA parsed
  print resA

  -- resA @=? 1715
  let resB = solveB parsed
  print resB

-- resB @=? 1739
