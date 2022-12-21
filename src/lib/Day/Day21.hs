module Day.Day21 (run) where

import Control.Lens
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit ((@=?))
import Utils

parse = Map.fromListWith undefined . fmap f . lines
 where
  f = g . splitOn ": "
  g [n, words -> xs] =
    (,) n $ case xs of
      [num] -> Lit (readInt num)
      [a, op, b] -> Bin (getOp op) a b

  getOp "+" = Plu
  getOp "*" = Mult
  getOp "-" = Minus
  getOp "/" = Div

data Exp = Lit Int | Bin Op String String deriving (Show)

data Op = Plu | Minus | Div | Mult deriving (Show)

type S = Map String Exp

evalOp :: Op -> (Int -> Int -> Int)
evalOp Plu = (+)
evalOp Minus = (-)
evalOp Div = div
evalOp Mult = (*)

eval :: Exp -> State S Int
eval (Lit n) = pure n
eval (Bin op l r) = do
  stat <- get
  ll <- eval $ stat Map.! l
  ix l .= Lit ll
  rr <- eval $ stat Map.! r
  ix r .= Lit rr
  pure $ evalOp op ll rr

solveA m = evalState (eval (m Map.! "root")) m

solveB m = (l, r)
 where
  Bin _ l r = m Map.! "root"

run :: String -> IO ()
run xs = do
  print xs
  let parsed = parse xs
  print parsed
  let resA = solveA parsed
  -- mapM_ print resA
  print resA

  -- resA @=? 1715
  let resB = solveB parsed
  print resB

-- resB @=? 1739
