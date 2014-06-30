module BeforeFix.FreeMonad where

import Control.Applicative
import Control.Monad (ap)

data FreeMonad f a = Pure a | Impure (f (FreeMonad f a))

instance Functor f => Functor (FreeMonad f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Impure t) = Impure (fmap (fmap f) t)

instance Functor f => Applicative (FreeMonad f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (FreeMonad f) where
  return = pure
  (Pure x)    >>= f = f x
  (Impure t)  >>= f = Impure (fmap (>>= f) t)

fromView :: FreeMonad f a -> FreeMonad f a
fromView = id

toView :: FreeMonad f a -> FreeMonad f a
toView = id
