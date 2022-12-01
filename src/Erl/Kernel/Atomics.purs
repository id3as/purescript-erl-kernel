module Erl.Kernel.Atomics
  ( AtomicsRef
  , Arity(..)
  , Index(..)
  , Signedness(..)
  , new
  , put
  , get
  , add
  , addGet
  , sub
  , subGet
  , exchange
  , compareExchange
  , info
  ) where

import Prelude

import Data.Either (Either)
import Effect (Effect)

data AtomicsRef

data Signedness
  = Signed
  | Unsigned

newtype Arity = Arity Int
newtype Index = Index Int

foreign import new :: Arity -> Signedness -> Effect AtomicsRef
foreign import put :: AtomicsRef -> Index -> Int -> Effect Unit
foreign import get :: AtomicsRef -> Index -> Effect Int
foreign import add :: AtomicsRef -> Index -> Int -> Effect Unit
foreign import addGet :: AtomicsRef -> Index -> Int -> Effect Int
foreign import sub :: AtomicsRef -> Index -> Int -> Effect Unit
foreign import subGet :: AtomicsRef -> Index -> Int -> Effect Int
foreign import exchange :: AtomicsRef -> Index -> Int -> Effect Int
foreign import compareExchange :: AtomicsRef -> Index -> Int -> Int -> Effect (Either Int Int)
foreign import info :: AtomicsRef -> Effect { size :: Arity, max :: Int, min :: Int, memory :: Int }
