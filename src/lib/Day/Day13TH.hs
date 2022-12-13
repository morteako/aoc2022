module Day.Day13TH where

import Data.Either (fromRight)
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH (Exp (ListE), Q, runIO)

listsFromTH :: Q Exp
listsFromTH = runIO $ do
    inp <- filter (not . null) . lines <$> readFile "inputs/13"
    pure $ ListE $ either (\x -> error $ show x) id . parseExp <$> inp