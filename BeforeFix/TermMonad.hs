{-# LANGUAGE GADTs #-}
module BeforeFix.TermMonad where

import Control.Monad (liftM, ap)
import Control.Applicative (Applicative, pure, (<*>))

-- An innefficient term monad, as can be found for example in the paper on the Constrained Monad Problem
-- Bind traverses its left argument but not its right
-- Unimo and the operational hackage package also use a term monad,
-- but a smarter construction, in which the sequence is presented as a tree instead

data TermM r a where
  Bind :: r w -> (w -> TermM r a) -> TermM r a
  Return :: a -> TermM r a

instance Functor (TermM r) where
  fmap = liftM

instance Applicative (TermM r) where
  pure = return
  (<*>) = ap

instance Monad (TermM r) where
  return = Return
  (Return a) >>= f = f a
  (Bind r c) >>= f = Bind r ((>>= f) . c)

