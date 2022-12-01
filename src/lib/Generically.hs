{-# LANGUAGE UndecidableInstances #-}

module Generically where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Data.Kind
import GHC.Generics

-- From base 4.17 ,in case I need it 🙃

newtype Generically a = Generically a

instance (Generic a, Semigroup (Rep a ())) => Semigroup (Generically a) where
    (<>) :: Generically a -> Generically a -> Generically a
    Generically a <> Generically b = Generically (to (from a <> from b :: Rep a ()))

instance (Generic a, Monoid (Rep a ())) => Monoid (Generically a) where
    mempty :: Generically a
    mempty = Generically (to (mempty :: Rep a ()))

    mappend :: Generically a -> Generically a -> Generically a
    mappend = (<>)

type Generically1 :: forall k. (k -> Type) -> (k -> Type)
newtype Generically1 f a where
    Generically1 :: forall {k} f a. f a -> Generically1 @k f a

instance (Generic1 f, Functor (Rep1 f)) => Functor (Generically1 f) where
    fmap :: (a -> a') -> (Generically1 f a -> Generically1 f a')
    fmap f (Generically1 as) =
        Generically1
            (to1 (fmap f (from1 as)))

    (<$) :: a -> Generically1 f b -> Generically1 f a
    a <$ Generically1 as =
        Generically1
            (to1 (a <$ from1 as))

-- | @since 4.17.0.0
instance (Generic1 f, Applicative (Rep1 f)) => Applicative (Generically1 f) where
    pure :: a -> Generically1 f a
    pure a =
        Generically1
            (to1 (pure a))

    (<*>) :: Generically1 f (a1 -> a2) -> Generically1 f a1 -> Generically1 f a2
    Generically1 fs <*> Generically1 as =
        Generically1
            (to1 (from1 fs <*> from1 as))

    liftA2 ::
        (a1 -> a2 -> a3) ->
        (Generically1 f a1 -> Generically1 f a2 -> Generically1 f a3)
    liftA2 (·) (Generically1 as) (Generically1 bs) =
        Generically1
            (to1 (liftA2 (·) (from1 as) (from1 bs)))

-- | @since 4.17.0.0
instance (Generic1 f, Alternative (Rep1 f)) => Alternative (Generically1 f) where
    empty :: Generically1 f a
    empty =
        Generically1
            (to1 empty)

    (<|>) :: Generically1 f a -> Generically1 f a -> Generically1 f a
    Generically1 as1 <|> Generically1 as2 =
        Generically1
            (to1 (from1 as1 <|> from1 as2))