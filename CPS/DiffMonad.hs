{-# LANGUAGE TypeSynonymInstances,Rank2Types,FlexibleInstances,UndecidableInstances #-}
module CPS.DiffMonad where
-- I.e. the codensity monad transformer

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (liftM, ap)

newtype DiffMonad m a = DiffMonad {getDM :: forall b. (a -> m b) -> m b} 

abs :: Monad m => DiffMonad m a -> m a
abs (DiffMonad m) = m return

rep :: Monad m => m a -> DiffMonad m a
rep m = DiffMonad (m >>=)

instance Monad m => Functor (DiffMonad m) where
  fmap = liftM

instance Monad m => Applicative (DiffMonad m) where
  pure = return
  (<*>) = ap  

instance Monad m => Monad (DiffMonad m) where
  return x = rep (return x)
  (DiffMonad m) >>= f  = DiffMonad $ \k -> m (\a -> getDM (f a) k)
