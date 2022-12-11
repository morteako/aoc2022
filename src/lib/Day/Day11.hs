{-# LANGUAGE TemplateHaskell #-}

module Day.Day11 (run) where

import Control.Lens hiding (levels)
import Control.Monad.State
import Data.Foldable (Foldable (toList), for_)
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.Extra (dropPrefix, sortOn, splitOn, trimStart)
import Data.Ord (Down (Down))
import Test.HUnit ((@=?))

data LeftArg = LOld

data RightArg = ROld | Lit Int

data Op = LeftArg :+ RightArg | LeftArg :* RightArg

data Monkey = Monkey
  { _levels :: ![Int]
  , _op :: !Op
  , _divtest :: !Int
  , _ifTrue :: !Int
  , _ifFalse :: !Int
  }
makeLenses ''Monkey

parseMonkies :: String -> IntMap Monkey
parseMonkies = IntMap.fromList . zip [0 ..] . fmap (parseMonkey . fmap trimStart . lines) . splitOn "\n\n"
 where
  parseMonkey :: [String] -> _
  parseMonkey [index, startings, oper, test, iftrue, ifalse]
    | items <- (fmap read . splitOn ",") $ dropPrefix "Starting items: " startings
    , t <- read $ dropPrefix "Test: divisible by " test
    , op <- parseOp oper
    , ift <- read $ dropPrefix "If true: throw to monkey " iftrue
    , iff <- read $ dropPrefix "If false: throw to monkey " ifalse =
        -- , på <- _
        Monkey items op (t) (ift) (iff)
  parseMonkey _ = undefined

  parseOp (dropPrefix "Operation: new = " -> words -> ["old", "+", r]) =
    case r of
      "old" -> LOld :+ ROld
      _ -> LOld :+ Lit (read r)
  parseOp (dropPrefix "Operation: new = " -> words -> ["old", "*", r]) =
    case r of
      "old" -> LOld :* ROld
      _ -> LOld :* Lit (read r)
  parseOp _ = undefined

getNewLevel :: Op -> Int -> Int
getNewLevel (LOld :+ Lit n) old = old + n
getNewLevel (LOld :+ ROld) old = old + old
getNewLevel (LOld :* Lit n) old = old * n
getNewLevel (LOld :* ROld) old = old * old

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
    let newLevelModded = f $ getNewLevel _op i
    let newMonkeyIndex = getIftest newLevelModded m
    _1 . ix newMonkeyIndex . levels %= (++ [newLevelModded])

getMonkeyLevelAfter20Rounds :: IntMap Monkey -> Int
getMonkeyLevelAfter20Rounds m = doAllRounds 20 div3 m
 where
  div3 x = div x 3

getMonkeyLevelAfterNRounds :: IntMap Monkey -> Int
getMonkeyLevelAfterNRounds m = doAllRounds 10000 divver m
 where
  primeProduct = product $ fmap _divtest m
  divver x = mod x primeProduct

doAllRounds :: Int -> (Int -> Int) -> IntMap Monkey -> Int
doAllRounds num f monkeys = product . take 2 . (sortOn Down) . IntMap.elems . snd . flip execState (monkeys, mempty) $ replicateM_ num $ do
  for_ (IntMap.keys monkeys) (doRound f)

run :: String -> IO ()
run xs = do
  let parsed = parseMonkies xs
  let resA = getMonkeyLevelAfter20Rounds parsed
  print resA

  resA @=? 58786
  let resB = getMonkeyLevelAfterNRounds parsed
  print resB

  resB @=? 14952185856