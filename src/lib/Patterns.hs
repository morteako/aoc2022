{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Patterns where

import Data.Foldable (Foldable (foldl'))
import Data.List.Extra hiding (foldl1')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (Sum (Sum, getSum))
import Debug.Trace
import GHC.Base (Semigroup)
import Linear (V2 (V2))

-- a = Map.findMin

data MapP k v = MapEmpty | MapMin k v (MapP k v) -- \| MapMax k v m
data MapPMax k v = MapMaxEmpty | MapMax k v (MapP k v) -- \| MapMax k v m

-- pattern MapE :: MapP k v
-- pattern MapE <- e where
--     MapE = MapEmpty

-- pattern MapMinP :: Ord k => k -> v -> Map.Map k v -> MapP k v
-- pattern MapMinP k v m <- (Map.minViewWithKey -> fromJust -> MapMin k v m)
--     where
--         MapMinP = MapMin

f = \case
    MapEmpty -> 0
    MapMin k v m -> 1 + f m