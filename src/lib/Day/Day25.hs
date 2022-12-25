module Day.Day25 (run) where

import Test.HUnit ((@=?))
import Data.Digits 
import Control.Lens

data Snafu = Zero | One | Two | DoubleMin | Min
  deriving (Show,Enum)



parse = fmap parseSnafuNum . lines
  where
    parseSnafuNum = fmap parseSnafu

    parseSnafu '0' = Zero
    parseSnafu '1' = One
    parseSnafu '2' = Two
    parseSnafu '-' = Min
    parseSnafu '=' = DoubleMin

fives = (5^) <$> [0..]

snafuToDecimal :: [Snafu] -> Int
snafuToDecimal (reverse -> xs) = sum $ zipWith f fives xs
  where
    f five (Min) = -five
    f five (DoubleMin) = -five * 2
    f five (n) = five * fromEnum n


decToSnafu = fmap f . over reversed inc . digits 5
  where
    f 0 = Zero
    f 1 = One
    f 2 = Two
    f 3 = DoubleMin
    f 4 = Min
    f x = error $ show x

incNext (x:xs) = (succ x : xs)
incNext [] = [1]

inc (5:xs) = 0 : inc (incNext xs)   
inc (4:xs) = 4 : inc (incNext xs)   
inc (3:xs) = 3 : inc (incNext xs)
inc (x:xs) = x : inc xs
inc [] = []

showSnafu = map f
  where
    f (Zero) = '0'
    f (One) = '1'
    f (Two) = '2'
    f (Min) = '-'
    f (DoubleMin) = '='


solveA :: [[Snafu]] -> String
solveA = showSnafu . decToSnafu . sum  . fmap snafuToDecimal

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  putStrLn $ resA
  resA @=? "121=2=1==0=10=2-20=2"
