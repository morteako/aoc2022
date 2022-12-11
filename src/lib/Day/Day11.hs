{-# LANGUAGE TemplateHaskell #-}

module Day.Day11 (run) where

import Control.Lens hiding (levels)
import Control.Monad.State
import Data.Foldable (Foldable (toList), for_)
import Data.IntMap
import Data.IntMap qualified as IntMap
import Data.IntMap qualified as Map
import Data.List.Extra
import Data.Ord (Down (Down))
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Test.HUnit ((@=?))
import Utils

-- newtype DivTest = DivTest Int deriving (Eq, Ord, Num, Show)

-- newtype IfTrue = IfTrue Int deriving (Eq, Ord, Num, Show)
-- newtype IfFalse = IfFalse Int deriving (Eq, Ord, Num, Show)

data Op = Old | Lit Int | Op :+ Op | Op :* Op deriving (Show)

data Monkey = Monkey
  { _levels :: [Int]
  , _op :: Op
  , _divtest :: Int
  , _ifTrue :: Int
  , _ifFalse :: Int
  }
  deriving (Show)
makeLenses ''Monkey

parseMonkies = Map.fromList . zip [0 ..] . fmap (parseMonkey . fmap trimStart . lines) . splitOn "\n\n"
 where
  parseMonkey :: [String] -> _
  parseMonkey [index, startings, oper, test, iftrue, ifalse]
    | items <- (fmap read . splitOn ",") $ dropPrefix "Starting items: " startings
    , t <- read $ dropPrefix "Test: divisible by " test
    , op <- parseOp oper
    , ift <- readInt $ dropPrefix "If true: throw to monkey " iftrue
    , iff <- readInt $ dropPrefix "If false: throw to monkey " ifalse =
        -- , på <- _
        Monkey items op (t) (ift) (iff)
  parseMonkey _ = undefined

  parseOp (dropPrefix "Operation: new = " -> words -> ["old", "+", r]) =
    case r of
      "old" -> Old :+ Old
      _ -> Old :+ Lit (read r)
  parseOp (dropPrefix "Operation: new = " -> words -> ["old", "*", r]) =
    case r of
      "old" -> Old :* Old
      _ -> Old :* Lit (read r)
  parseOp _ = undefined

getNewLevel :: Op -> Int -> Int
getNewLevel Old old = old
getNewLevel (Lit n) old = n
getNewLevel (Old :+ op2) old = old + getNewLevel op2 old
getNewLevel (Old :* op2) old = old * getNewLevel op2 old
getNewLevel _ _ = undefined

divisible :: Integral a => a -> a -> Bool
divisible x div = mod x div == 0

getIftest :: Int -> Monkey -> Int
getIftest worryLevel Monkey{_divtest, _ifTrue, _ifFalse} =
  if worryLevel `divisible` _divtest
    then _ifTrue
    else _ifFalse

doRound :: (Int -> Int) -> Int -> State (IntMap Monkey, IntMap Int) ()
doRound f (monkeyIx) = do
  m@Monkey{_op, _levels} <- gets ((IntMap.! monkeyIx) . fst)
  _1 . ix monkeyIx . levels .= []
  for_ _levels $ \i -> do
    _2 %= IntMap.insertWith (+) monkeyIx 1
    let newLevel = getNewLevel _op i
    let newLevel3 = f newLevel
    let newMonkeyIndex = getIftest newLevel3 m
    _1 . ix newMonkeyIndex . levels %= (++ [newLevel3])

getMonkeyLevelAfter20Rounds :: IntMap Monkey -> Int
getMonkeyLevelAfter20Rounds m = doAllRounds 20 div3 m
 where
  div3 x = div x 3

getMonkeyLevelAfterNRounds :: IntMap Monkey -> Int
getMonkeyLevelAfterNRounds m = doAllRounds 10000 divver m
 where
  primeProduct = product $ fmap _divtest m
  divver x = mod x primeProduct

doAllRounds num f monkeys = product . take 2 . (sortOn Down) . IntMap.elems . snd . flip execState (monkeys, mempty) $ replicateM_ num $ do
  keys <- gets (IntMap.keys . fst)
  for_ keys (doRound f)

run :: String -> IO ()
run xs = do
  let parsed = parseMonkies xs
  let resA = getMonkeyLevelAfter20Rounds parsed
  print resA

  resA @=? 58786
  let resB = getMonkeyLevelAfterNRounds parsed
  print resB

  resB @=? 14952185856