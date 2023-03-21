module Erl.Kernel.Ets where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Atom (atom, Atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, tuple2, Tuple3)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UpdateOp :: Type

newtype CreateOption
  = CreateOption Atom

foreign import data Ref :: Type

namedTable :: CreateOption
namedTable = CreateOption $ (atom "named_table")

public :: CreateOption
public = CreateOption $ (atom "public")

orderedSet :: CreateOption
orderedSet = CreateOption $ (atom "ordered_set")

private :: CreateOption
private = CreateOption $ (atom "private")

protected :: CreateOption
protected = CreateOption $ (atom "protected")

updateOp :: Int -> Int -> UpdateOp
updateOp pos inc = unsafeCoerce $ tuple2 pos inc

newtype Table
  = Table Atom

foreign import new :: Table -> List CreateOption -> Effect Ref

foreign import insert2 :: forall k v. Table -> Tuple2 k v -> Effect Unit

foreign import insert3 :: forall k v v2. Table -> Tuple3 k v v2 -> Effect Unit

foreign import updateCounter :: forall k. Table -> k -> UpdateOp -> Effect Int

-- Cheeky hack so we can increment non-counter
foreign import increment :: forall k v. Table -> k -> Int -> v -> Effect v

foreign import lookupElement :: forall k v. Table -> k -> Int -> Effect v

foreign import updateElement :: forall k v. Table -> k -> Int -> v -> Effect Boolean

-- Statement of intent, we should probably look at batched updates
foreign import updateElements :: forall k v. Table -> k -> List (Tuple2 Int v) -> Effect Boolean

-- Shrug again
foreign import match :: forall match result. Table -> match -> Effect (List result)

-- More Shrug again
foreign import select :: forall result. Table -> SelectOp -> Effect (List result)

foreign import selectOne :: forall result. Table -> SelectOp -> Effect (Maybe result)

foreign import selectOp :: forall k v. k -> v -> SelectOp

foreign import data SelectOp :: Type


foreign import delete :: Table -> Effect Unit

foreign import delete2 :: forall k. Table -> k -> Effect Unit
