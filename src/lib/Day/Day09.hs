module Day.Day09 (run) where

import Control.Lens (view)
import Data.Containers.ListUtils (nubOrd)
import Data.List
import Data.List.Extra
import Debug.Trace
import Linear
import Test.HUnit ((@=?))
import Utils (readInt)
import Utils qualified

data Dir = R | L | D | U deriving (Show, Read, Enum)

data Move a = a :> Int deriving (Show, Functor)

parse = fmap parseMove . lines
 where
  parseMove (words -> [l, readInt -> n]) = read @Dir l :> n

dirToVec :: Num a => Dir -> V2 a
dirToVec R = V2 1 0
dirToVec L = V2 (-1) 0
dirToVec D = V2 0 (-1)
dirToVec U = V2 0 1

-- isDia a b =
--   elem a $ map (b +) $ V2 <$> [1, (-1)] <*> [1, (-1)]

-- getWeirdLine target tail =
--   elem tail $ map (target +) $ map dirToVec [R .. U]

-- getWeirdDiagonal target tail
--   | tail + V2 2 1 == target = Just $ tail + 1
--   | tail + V2 2 (-1) == target = Just $ tail + V2 1 (-1)
--   | tail + V2 (-2) (-1) == target = Just $ tail + V2 (-1) (-1)
--   | tail + V2 (-2) (1) == target = Just $ tail + V2 (-1) (1)
-- getWeirdDiagonal _ _ = Nothing

-- doDirTwo [] ht = traceLab "_____" [ht]
-- doDirTwo ((_ :> 0) : rest) ht = doDirTwo rest ht
-- doDirTwo (dn : _) ht
--   | traceShow (dn, ht) False = undefined
-- doDirTwo (d :> (subtract 1 -> n) : rest) [h, t] = [h, t] : doDirTwo (d :> n : rest) newHT
--  where
--   dv = dirToVec d

--   newHT = [h', t']
--   h' = h + dv
--   t' = getTail ()

--   getTail ()
--     | h == t = traceLab "start" t -- start
--     | h' == t = traceLab "overlap" t -- overlap
--     | getWeirdLine h' t = traceLab "weird" t -- weird
--     | isDia h' t = traceLab "newDirCase" t -- newDirCase
--     | h == t + dv = traceLab "normal case" h -- normal case
--     | isDia h t = traceLab "diagonal" h -- diagonal
--     -- \| hx' == tx || hy' == ty = t + d
--     | otherwise = error $ "hmmm " <> show (d, h, t)
-- doDirTwo _ _ = undefined

data NDir
  = NL
  | NR
  | NU
  | ND
  | NUR
  | NUL
  | NDR
  | NDL
  | Stay
  deriving (Show)

getNDirVec :: NDir -> V2 Int
getNDirVec NR = V2 1 0
getNDirVec NL = V2 (-1) 0
getNDirVec ND = V2 0 (-1)
getNDirVec NU = V2 0 1
getNDirVec NUR = 1
getNDirVec NUL = V2 (-1) 1
getNDirVec NDR = V2 1 (-1)
getNDirVec NDL = -1
getNDirVec Stay = 0

toNDir :: Dir -> NDir
toNDir R = NR
toNDir L = NL
toNDir D = ND
toNDir U = NU

data Label = H | T | Lab Int

instance Show Label where
  show H = "H"
  show T = "T"
  show (Lab n) = show n

data Dot = Dot Label (V2 Int) deriving (Show)

getDotPos (Dot _ p) = p

un = undefined

debug = True

traceLab s x = if debug then Utils.traceLab s x else x

doDir [] ht = traceLab "_____" [ht]
doDir ((_ :> 0) : rest) ht = doDir rest ht
doDir (dn : _) ht
  | traceShow (dn, ht) False = undefined
doDir (d :> (subtract 1 -> n) : rest) ht@[Dot hlab hpos, tdot] = ht : doDir (d :> n : rest) newHT
 where
  moveDir = dirToVec d

  newHT = [Dot hlab newHPos, t']
  newHPos = hpos + moveDir

  newH = Dot hlab newHPos

  t' = getTail hpos newH tdot

  getTail prevHead (Dot curHeadLab curHeadPos) tt@(Dot tlab tpos)
    | curHeadPos == tpos = traceLab "same spot - stay" $ Dot tlab tpos
    | isTouching curHeadPos tpos = traceLab "isTouching stay" $ Dot tlab tpos
    | Just nextTPos <- isTwoDirAway curHeadPos tpos = traceLab "sameDir *2" $ Dot tlab nextTPos
    | isDoubleDiag curHeadPos tpos = traceLab "doubleDiag usePrev" $ Dot tlab prevHead
    | otherwise = error $ "hmmm " <> show (d, curHeadPos, tt)
doDir _ _ = undefined

a .+ dir = a + getNDirVec dir

isTouching a b = case abs (a - b) of
  V2 1 1 -> True
  V2 1 0 -> True
  V2 0 1 -> True
  _ -> False

isDoubleDiag a b = case abs (a - b) of
  V2 2 1 -> True
  V2 1 2 -> True
  _ -> False

isTwoDirAway a b = case (a - b) of
  V2 2 0 -> Just $ a - V2 1 0
  V2 (-2) 0 -> Just $ a + V2 1 0
  V2 0 (-2) -> Just $ a + V2 0 1
  V2 0 2 -> Just $ a - V2 0 1
  _ -> Nothing

-- \| view _x h ==

more f = length . nub . fmap f . fmap last

-- solveA moves = more id $ doDirTwo moves [0, 0]

solveA' moves = more getDotPos $ doDir moves [Dot H 0, Dot T 0]

-- f !(cur : (traceLab "cd" -> cs)) (traceLab "f" -> dir) = doDir dir cur ++ cur : cs

solveB = id

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print parsed
  -- print resA

  let resA = solveA' parsed
  -- mapM_ print $ groupOn (view _x) $ sort $ nub $ fmap snd $ solveA parsed
  -- print "--"
  -- mapM print $ sort $ fmap snd $ solveA parsed

  print resA

  -- mapM_ print $ zip resA resA'

  pure ()