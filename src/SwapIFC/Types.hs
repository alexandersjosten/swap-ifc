{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module SwapIFC.Types where

import Control.Applicative
import Control.Monad
import Data.IORef

data High

data Low

newtype Flow tag a = Flow (IO a)

newtype FlowRef tag a = FlowRef (IORef a)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Monad , Functor and Applicative instances for High flows
instance Monad (Flow High) where
  -- return :: a -> Flow High a
  return = Flow . return

  -- (>>=) :: Flow High a -> (a -> Flow High b) -> Flow High b
  (Flow ioa) >>= f = Flow $ do
    a <- ioa
    case (f a) of
      Flow iob -> iob

instance Functor (Flow High) where
  -- fmap :: (a -> b) -> Flow High a -> Flow High b
  fmap f ioa = ioa >>= return . f

instance Applicative (Flow High) where
  -- pure :: a -> Flow High a
  pure = return

  -- (<*>) :: Flow High (a -> b) -> Flow High a -> Flow High b
  (<*>) = ap

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Monad , Functor and Applicative instances for Low flows
instance Monad (Flow Low) where
  -- return :: a -> Flow t a
  return = Flow . return

  -- (>>=) :: Flow t a -> (a -> Flow t b) -> Flow t b
  (Flow ioa) >>= f = Flow $ do
    a <- ioa
    case (f a) of
      Flow iob -> iob

instance Functor (Flow Low) where
  -- fmap :: (a -> b) -> Flow Low a -> Flow Low b
  fmap f ioa = ioa >>= return . f

instance Applicative (Flow Low) where
  -- pure :: a -> Flow Low a
  pure = return

  -- (<*>) :: Flow Low (a -> b) -> Flow Low a -> Flow Low b
  (<*>) = ap

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Class and instance declarations for the valid flows, low can flow to low,
-- low can flow to high and high can flow to high. Flow from high to low is not
-- allowed

class ValidFlow t1 t2 where
  
instance ValidFlow Low Low
instance ValidFlow Low High
instance ValidFlow High High

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Class and instance declarations for combining flows, low combined with low
-- yields a low flow, low combined with high (in any order) yields a high flow,
-- high combined with high yields a high flow

class CombineFlow t1 t2 t3 | t1 t2 -> t3 where
  
instance CombineFlow Low  Low  Low
instance CombineFlow Low  High High
instance CombineFlow High Low  High
instance CombineFlow High High High

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
