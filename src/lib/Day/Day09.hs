module Day.Day09 (run) where

import Control.Lens (view)
import Data.Containers.ListUtils (nubOrd)
import Data.List
import Data.List.Extra
import Debug.Trace
import Linear
import Test.HUnit ((@=?))
import Utils

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

isDia a b =
  elem a $ map (b +) $ V2 <$> [1, (-1)] <*> [1, (-1)]

getWeirdLine target tail =
  elem tail $ map (target +) $ map dirToVec [R .. U]

getWeirdDiagonal target tail
  | tail + V2 2 1 == target = Just $ tail + 1
  | tail + V2 2 (-1) == target = Just $ tail + V2 1 (-1)
  | tail + V2 (-2) (-1) == target = Just $ tail + V2 (-1) (-1)
  | tail + V2 (-2) (1) == target = Just $ tail + V2 (-1) (1)
getWeirdDiagonal _ _ = Nothing

doDirTwo [] ht = traceLab "_____" [ht]
doDirTwo ((_ :> 0) : rest) ht = doDirTwo rest ht
doDirTwo (dn : _) ht
  | traceShow (dn, ht) False = undefined
doDirTwo (d :> (subtract 1 -> n) : rest) [h, t] = [h, t] : doDirTwo (d :> n : rest) newHT
 where
  dv = dirToVec d

  newHT = [h', t']
  h' = h + dv
  t' = getTail ()

  getTail ()
    | h == t = traceLab "start" t -- start
    | h' == t = traceLab "overlap" t -- overlap
    | getWeirdLine h' t = traceLab "weird" t -- weird
    | isDia h' t = traceLab "newDirCase" t -- newDirCase
    | h == t + dv = traceLab "normal case" h -- normal case
    | isDia h t = traceLab "diagonal" h -- diagonal
    -- \| hx' == tx || hy' == ty = t + d
    | otherwise = error $ "hmmm " <> show (d, h, t)
doDirTwo _ _ = undefined

data NDir
  = NL
  | NR
  | NU
  | ND
  | NUR
  | NUL
  | NDR
  | NDL
  | Same
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
getNDirVec Same = 0

toNDir :: Dir -> NDir
toNDir R = NR
toNDir L = NL
toNDir D = ND
toNDir U = NU

data Dot = V2 Int :# NDir deriving (Show)

doDir [] ht = traceLab "_____" [ht]
doDir ((_ :> 0) : rest) ht = doDir rest ht
doDir (dn : _) ht
  | traceShow (dn, ht) False = undefined
doDir (d :> (subtract 1 -> n) : rest) ht@[h :# _, tdot] = ht : doDir (d :> n : rest) newHT
 where
  dv = dirToVec d

  newHT = [h' :# toNDir d, t']
  h' = h + dv
  t' = getTail h' tdot

  getTail h' (t :# ndir)
    -- h' == t + dir * 2
    -- \| h == t = traceLab "start" t :# Same -- start
    | h' == t = traceLab "overlap" t :# Same -- overlapping
    | getWeirdLine h' t = traceLab "weird" t -- weird
    | isDia h' t = traceLab "newDirCase" t -- newDirCase
    | h == t + getNDirVec ndir = traceLab "normal case" h -- normal case
    | isDia h t = traceLab "diagonal" h -- diagonal
    -- \| hx' == tx || hy' == ty = t + d
    | otherwise = error $ "hmmm " <> show (d, h, t)
doDir _ _ = undefined

-- \| view _x h ==

solveA moves = length $ nub $ fmap last $ doDirTwo moves [0, 0]

-- f !(cur : (traceLab "cd" -> cs)) (traceLab "f" -> dir) = doDir dir cur ++ cur : cs

solveB = id

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print parsed
  let resA = solveA parsed
  -- print resA

  print $ solveA parsed
  -- mapM_ print $ groupOn (view _x) $ sort $ nub $ fmap snd $ solveA parsed
  -- print "--"
  -- mapM print $ sort $ fmap snd $ solveA parsed

  resA @=? 6269
  -- let resB = solveB parsed
  -- print resB
  -- resB @=? 1739

  pure ()