module Day.Day10 (run) where

import Control.Lens (Ixed (ix), over)
import Data.Char (toLower, toUpper)
import Data.List.Extra (chunksOf)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)
import Test.HUnit ((@=?))

data Instr = Noop | Addx Int deriving (Show, Read)

parseInstr :: [Char] -> [Instr]
parseInstr = fmap (read . over (ix 0) toUpper) . lines . fmap toLower

data CycleRange = Int :> Int deriving (Show)

runInstr :: Int -> Int -> [Instr] -> [(Int, CycleRange)]
runInstr x cycle [] = [(x, cycle :> maxBound)]
runInstr x cycle (Noop : rest) =
  (x, cycle :> cycle) : runInstr x (cycle + 1) rest
runInstr x cycle ((Addx n) : rest) =
  (x, cycle :> (cycle + 1)) : runInstr (x + n) (cycle + 2) rest

calculateSignalStrengths :: [Instr] -> Int
calculateSignalStrengths is = sum $ do
  let ranges = runInstr 1 1 is
  cyc <- [20, 60 .. 220]
  pure $ cyc * findSignalStr cyc ranges
 where
  getSignalRange cyc (x, mi :> ma)
    | mi <= cyc && ma >= cyc = Just x
    | otherwise = Nothing

  findSignalStr cyc res = case mapMaybe (getSignalRange cyc) res of
    [x] -> x
    _ -> error "wtf"

drawCrtImage :: [Instr] -> [[Char]]
drawCrtImage = chunksOf 40 . fmap (drawSprite . uncurry isInSprit) . concatMap flattenRange . runInstr 1 1
 where
  flattenRange (reg, a :> b) = fmap (reg,) $ fmap (flip mod 40) [a .. min b 240]

  isInSprit :: (Enum a, Ord a, Num a) => a -> a -> Bool
  isInSprit (succ -> x) sprite = abs (x - sprite) <= 1

  drawSprite x = if x then '#' else '.'

run :: String -> IO ()
run xs = do
  let instrs = parseInstr xs
  let resA = calculateSignalStrengths instrs
  print resA

  resA @=? 14240

  let resB = drawCrtImage instrs
  mapM_ putStrLn resB

  resB
    @=? [ "###..#....#..#.#....#..#.###..####.#..##"
        , "#..#.#....#..#.#....#.#..#..#....#.#..#."
        , "#..#.#....#..#.#....##...###....#..####."
        , "###..#....#..#.#....#.#..#..#..#...#..#."
        , "#....#....#..#.#....#.#..#..#.#....#..##"
        , "#....####..##..####.#..#.###..####.#..#."
        ]
