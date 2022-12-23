module Day.Day21 (run) where

import Control.Lens
import Control.Monad.State (State, evalState, gets)
import Data.List.Extra (sort, splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit ((@=?))
import Utils (readInt)

-- horrible error handling, but

parse :: String -> Map String Exp
parse = Map.fromListWith undefined . fmap parseMonkey . lines
 where
  parseMonkey = parseExp . splitOn ": "
  parseExp [n, words -> xs] =
    (,) n $ case xs of
      [num] -> LitI (readInt num)
      [a, op, b] -> BinB (getOp op) (LitS a) (LitS b)
      _ -> undefined
  parseExp _ = undefined

  getOp "+" = Plus
  getOp "*" = Mult
  getOp "-" = Minus
  getOp "/" = Div
  getOp _ = undefined

data Op = Plus | Minus | Div | Mult deriving (Show, Eq, Ord)

evalOp :: Op -> (Int -> Int -> Int)
evalOp Plus = (+)
evalOp Minus = (-)
evalOp Div = div
evalOp Mult = (*)

solveA :: Map String Exp -> Int
solveA m = evalExpToInt $ evalState (evalExp (m Map.! "root")) m
 where
  evalExpToInt :: Exp -> Int
  evalExpToInt (LitI n) = n
  evalExpToInt (BinB op eb eb') = evalOp op (evalExpToInt eb) (evalExpToInt eb')
  evalExpToInt LitU = error "nop"
  evalExpToInt (LitS str) = error "nop"

data Exp = LitI Int | LitU | LitS String | BinB Op Exp Exp deriving (Show)

evalExp :: Exp -> State (Map String Exp) Exp
evalExp l@LitI{} = pure l
evalExp LitU = pure LitU
evalExp (LitS s) = do
  e <- gets (Map.! s) >>= evalExp
  ix s .= e
  pure e
evalExp (BinB op l r) = do
  BinB op <$> evalExp l <*> evalExp r

toExp' :: Exp -> Exp'
toExp' (LitI n) = LI n
toExp' LitU = LU
toExp' (BinB op eb eb') = case (toExp' eb, toExp' eb') of
  (LI i, LI r) -> LI $ evalOp op i r
  (LI i, r) -> BiL op i r
  (l, LI r) -> BiR op l r
  _ -> error "wrong"
toExp' (LitS s) = error "wrong"

data Exp' = LI Int | LU | BiL Op Int Exp' | BiR Op Exp' Int deriving (Show, Eq, Ord)

(@=) :: Int -> Exp' -> Int
i @= re =
  case re of
    BiL Plus l r -> (i - l) @= r
    BiL Minus l r -> (l - i) @= r
    BiL Div l r -> (l `div` i) @= r
    BiL Mult l r -> (i `div` l) @= r
    BiR Plus l r -> (i - r) @= l
    BiR Minus l r -> (i + r) @= l
    BiR Div l r -> (i * r) @= l
    BiR Mult l r -> (div i r) @= l
    LU -> i
    LI m -> error "mm"

solveB (Map.insert "humn" LitU -> m) =
  fixEquation $
    fmap toExp' $
      evalState (traverse evalExp [l, r]) m
 where
  BinB _ l r = m Map.! "root"

  fixEquation (sort -> [LI a, b]) = a @= b

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA

  resA @=? 142707821472432
  let resB = solveB parsed
  print resB

  resB @=? 3587647562851
