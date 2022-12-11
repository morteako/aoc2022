module Utils where

import Data.Foldable (Foldable (foldl'))
import Data.List.Extra hiding (foldl1')
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (Sum, getSum))
import Debug.Trace
import GHC.Base (Semigroup)
import Linear (V2 (V2))

readInt :: String -> Int
readInt = read

(?:) :: Maybe c -> c -> c
(?:) = flip fromMaybe

(=:) :: a -> b -> (a, b)
(=:) = (,)

count :: Eq a => a -> [a] -> Int
count x = getSum . foldMap (Sum . fromEnum . (== x))

countP :: Foldable f => (a -> Bool) -> f a -> Int
countP p = getSum . foldMap (Sum . fromEnum . p)

semiFoldMapl' :: (Semigroup v, Functor f, Foldable f) => (a -> v) -> f a -> v
semiFoldMapl' av = foldl1' (<>) . fmap av

semiFoldMapr :: (Semigroup v, Functor f, Foldable f) => (a -> v) -> f a -> v
semiFoldMapr av = foldr1 (<>) . fmap av

foldl1' :: Foldable t => (a -> a -> a) -> t a -> a
foldl1' f xs =
    fromMaybe
        (errorWithoutStackTrace "foldl1': empty structure")
        (foldl' mf Nothing xs)
  where
    mf m y =
        Just
            ( case m of
                Nothing -> y
                Just x -> f x y
            )

printMap :: (Show a, Eq k) => Map.Map (V2 k) a -> IO ()
printMap m = do
    putStrLn "--------"
    let xs = Map.toList m
    let g = groupOn (\(V2 x _, _) -> x) xs
    let gg = fmap (fmap snd) g
    mapM_ print gg
    putStrLn ""

traceLab :: Show a => [Char] -> a -> a
traceLab s x = trace (s ++ ": " ++ show x) x

traceOn :: Show a => (a -> [Char]) -> a -> a
traceOn f x = trace (f x) x

(.?) :: Show t1 => (t2 -> t1) -> (t1 -> t3) -> t2 -> t3
(.?) f g = \x -> g $ traceShowId (f x)

-- makeMap :: [Dot] -> Map.Map (V2 Int) [Label]
-- makeMap = Map.unionWith (<>) defMap . Map.fromListWith (<>) . fmap (\(Dot l pos) -> (pos, pure l))

-- defMap = Map.insertWith (++) 0 [T] $ Map.fromList $ fmap (,[]) xs
--  where
--   xs = V2 <$> [-11 .. 14] <*> [-5 .. 15]

--   limU = 5
--   limD = 5

-- fp x = case x of
--   [] -> "."
--   [T] -> "s"
--   labs -> show $ minimum labs

-- printMap :: Map.Map (V2 Int) [Label] -> IO ()
-- printMap m = do
--   putStrLn "--------"
--   let xs = Map.toList $ Map.mapKeys (\(V2 x y) -> V2 y x) m
--   let g = groupOn (\(V2 x y, _) -> x) xs
--   let gg = reverse $ (fmap . concatMap) (fp . snd) g

--   mapM_ (print . sort) $ Map.filter (\x -> length x > 1) m
--   mapM_ putStrLn gg
--   putStrLn ""
