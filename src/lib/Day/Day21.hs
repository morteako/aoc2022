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

data Op = Plu | Minus | Div | Mult deriving (Show, Eq, Ord)

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

data ExpB = LitI Int | LitU | LitS String | BinB Op ExpB ExpB deriving (Show)

-- data OpB = BPlu | BMinus | BDiv | BMult deriving (Show)

type SB = Map String ExpB

-- evalOpB :: OpB -> (Int -> Int -> Int)
-- evalOpB BPlu = (+)
-- evalOpB BMinus = (-)
-- evalOpB BDiv = div
-- evalOpB BMult = (*)

eval' :: ExpB -> State SB ExpB
eval' (LitI n) = pure $ LitI n
eval' (LitU) = pure LitU
eval' (LitS s) = do
  q <- gets (Map.! s)
  qq <- eval' q
  ix s .= qq
  pure qq
eval' (BinB op l r) = do
  ll <- eval' l
  -- ix l' .= ll
  rr <- eval' r
  -- ix r .= Lit rr
  pure $ BinB op ll rr

toB :: Exp -> ExpB
toB (Lit n) = LitI n
toB (Bin op str cs) = BinB op (LitS str) (LitS cs)

-- toBOP :: Op -> B

-- data Eqq = ExpB := ExpB deriving Show

simpl :: ExpB -> ExpB
simpl (LitI n) = LitI n
simpl LitU = LitU
simpl b@(BinB op eb eb') =
  let l = simpl eb
      r = simpl eb'
   in case (l, r) of
        (LitI li, LitI ri) -> LitI $ evalOp op li ri
        _ -> BinB op l r
simpl (LitS str) = error "STR"

toRE :: ExpB -> RE
toRE (LitI n) = LI n
toRE LitU = LU
toRE (LitS s) = undefined
toRE (BinB op eb eb') = Bi op (toRE eb) (toRE eb')

data RE = LI Int | LU | Bi Op RE RE deriving (Show, Eq, Ord)

-- pattern

-- i @= LI r = (i, r)
i @= (Bi Plu (LI l) r) = (i - l) @= r
i @= (Bi Minus (LI l) r) = (l - i) @= r
i @= (Bi Div (LI l) r) = (l `div` i) @= r
i @= (Bi Mult (LI l) r) = (div i l) @= r
i @= (Bi Plu l (LI r)) = (i - r) @= l
i @= (Bi Minus l (LI r)) = (i + r) @= l
i @= (Bi Div l (LI r)) = (i * r) @= l
i @= (Bi Mult l (LI r)) = (div i r) @= l
i @= other = (i, other)

-- le @= Bi op (LI l) (LI r) = le @= LI (evalOp op l r)
-- Bi op (LI l) (LI r) @= re = LI (evalOp op l r) @= re
-- le @= re
--   | Just (x, y) <- one re le = x @= y
--   -- \| Just (x, y) <- one re le = undefined
--   | otherwise = error $ show (le, "@=", re)

-- one (Bi Plu (LI l) r) le = Just (Bi Minus le (LI l), r)
-- one _ _ = Nothing

solveB (fmap toB -> Map.insert "humn" LitU -> m) = fi . fmap (toRE . simpl) $ evalState (traverse eval' [l, r]) m
 where
  BinB _ l r = m Map.! "root"

  fi (sort -> [LI a, b]) = a @= b

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
  -- mapM_ print resB
  print "---"

-- mapM_ (print) resB

-- resB @=? 1739
