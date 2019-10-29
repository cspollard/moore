{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Both where

import GHC.Generics
import Data.Bitraversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Biapplicative
import Data.Bifunctor.Join
import Data.Bifunctor.Biff
import Data.Bifunctor.Tannen
import Data.Profunctor
import Control.Lens hiding ((<.>))
import Data.Functor.Identity
import Data.Functor.Apply
import Data.Functor.Bind


data Both a b = Both !a !b deriving (Generic, Show)


instance (Semigroup a, Semigroup b) => Semigroup (Both a b) where
  Both a b <> Both a' b' = Both (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Both a b) where
  mempty = Both mempty mempty

instance Bitraversable Both where
  bitraverse f g (Both a b) = Both <$> f a <*> g b
  {-# INLINE bitraverse  #-}

instance Bifunctor Both where
  bimap = bimapDefault
  {-# INLINE bimap  #-}

instance Bifoldable Both where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap  #-}

instance Biapplicative Both where
  bipure = Both 
  {-# INLINE bipure  #-}
  Both f g <<*>> Both a b = Both (f a) (g b)
  {-# INLINE (<<*>>)  #-}


instance Functor (Both a) where
  fmap = second
  {-# INLINE fmap  #-}

instance Semigroup s => Apply (Both s) where
  Both s f <.> Both t x = Both (s <> t) (f x)
  {-# INLINE (<.>)  #-}

instance Monoid s => Applicative (Both s) where
  pure = Both mempty
  {-# INLINE pure  #-}

  (<*>) = (<.>)
  {-# INLINE (<*>) #-}

instance Semigroup s => Bind (Both s) where
  Both s a >>- f = let Both t b = f a in Both (s <> t) b
  {-# INLINE (>>-) #-}

instance Monoid s => Monad (Both s) where
  (>>=) = (>>-)


toTuple :: Both a b -> (a, b)
toTuple (Both a b) = (a, b)

fromTuple :: (a, b) -> Both a b
fromTuple (a, b) = Both a b


instance Field1 (Both a b) (Both a' b) a a' where
  _1 k (Both a b) = k a <&> \a' -> Both a' b
  {-# INLINE _1 #-}

instance Field2 (Both a b) (Both a b') b b' where
  _2 k (Both a b) = k b <&> \b' -> Both a b'
  {-# INLINE _2 #-}


first'' :: Strong p => p a a' -> p (Both a b) (Both a' b)
first'' f = dimap (\(Both a b) -> (a, b)) (\(a, b) -> Both a b) $ first' f     
     
     
second'' :: Strong p => p b b' -> p (Both a b) (Both a b')
second'' f = dimap (\(Both a b) -> (a, b)) (\(a, b) -> Both a b) $ second' f     
    


type TF = Join Both

type Cut f = Tannen (Both String) (Biff Both Identity f)

type PassFail f g = Biff Both f g


pattern TF :: a -> a -> TF a
pattern TF x y = Join (Both x y)


_Join :: Iso' (Join p a) (p a a)
_Join = coerced

choose :: Bool -> Lens' (TF a) a
choose True = true
choose False = false


true :: Lens' (TF a) a
true = _Join . _1

false :: Lens' (TF a) a
false = _Join . _2


data Either' a b
  = Left' !a
  | Right' !b
  deriving (Generic, Show)


makePrisms ''Either'

instance Bitraversable Either' where
  bitraverse f _ (Left' a) = Left' <$> f a
  bitraverse _ g (Right' b) = Right' <$> g b
  {-# INLINE bitraverse #-}

instance Bifunctor Either' where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance Bifoldable Either' where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

lazyEither :: Either' a b -> Either a b
lazyEither (Left' a) = Left a
lazyEither (Right' b) = Right b

strictEither :: Either a b -> Either' a b
strictEither (Left a) = Left' a
strictEither (Right b) = Right' b
