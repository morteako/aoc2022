{-# LANGUAGE PartialTypeSignatures #-}

module Day.Day02 where

import Control.Lens
import Control.Lens (from)
import Test.HUnit ((@=?))

data Opp
  = Rock
  | Paper
  | Scissor
  deriving (Show, Eq, Ord, Enum)

data Result = Loss | Draw | Win deriving (Show, Enum)

data XYZ = X | Y | Z deriving (Read, Show, Enum)

parse :: String -> [(Opp, XYZ)]
parse = fmap toTup . lines
 where
  toTup [a, ' ', z] = (abc a, read $ pure z)
  toTup _ = error "game"

  abc 'A' = Rock
  abc 'B' = Paper
  abc 'C' = Scissor
  abc _ = error "abc"

lost, draw, won :: Int
lost = 0
draw = 3
won = 6

scoreYou :: Opp -> Int
scoreYou x = succ $ fromEnum x

convEnum :: (Enum c, Enum p) => p -> c
convEnum x = toEnum . fromEnum $ x

scoreGame :: (Opp, Opp) -> Int
scoreGame (op, you)
  | you == winner op = scoreYou you + won
  | you == op = scoreYou you + draw
  | otherwise = scoreYou you + lost

solveA :: [(Opp, XYZ)] -> Int
solveA = sum . fmap scoreGame . fmap (over _2 convEnum)

winner :: Opp -> Opp
winner Rock = Paper
winner Paper = Scissor
winner Scissor = Rock

loser :: Opp -> Opp
loser Paper = Rock
loser Scissor = Paper
loser Rock = Scissor

fix :: (Opp, Result) -> (Opp, Opp)
fix (op, Draw) = (op, op)
fix (op, Win) = (op, winner op)
fix (op, Loss) = (op, loser op)

solveB :: [(Opp, XYZ)] -> Int
solveB = sum . fmap (scoreGame . fix . over _2 convEnum)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA

  resA @=? 15632

  let resB = solveB parsed
  print resB

  resB @=? 14416
