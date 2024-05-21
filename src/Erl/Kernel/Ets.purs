module Erl.Kernel.Ets where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Atom (atom, Atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2)
import Foreign (Foreign, unsafeToForeign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UpdateOp :: Type

newtype CreateOption = CreateOption Foreign

foreign import data Ref :: Type

namedTable :: CreateOption
namedTable = CreateOption $ unsafeToForeign (atom "named_table")

public :: CreateOption
public = CreateOption $ unsafeToForeign (atom "public")

orderedSet :: CreateOption
orderedSet = CreateOption $ unsafeToForeign (atom "ordered_set")

private :: CreateOption
private = CreateOption $ unsafeToForeign (atom "private")

protected :: CreateOption
protected = CreateOption $ unsafeToForeign (atom "protected")

readConcurrency :: Boolean -> CreateOption
readConcurrency v = CreateOption $ unsafeToForeign (tuple2 (atom "read_concurrency") v)

writeConcurrency :: Boolean -> CreateOption
writeConcurrency v = CreateOption $ unsafeToForeign $ (tuple2 (atom "read_concurrency") v)

updateOp :: Int -> Int -> UpdateOp
updateOp pos inc = unsafeCoerce $ tuple2 pos inc

newtype Table = Table Atom

foreign import new :: Table -> List CreateOption -> Effect Ref

foreign import insert2 :: forall k v. Table -> Tuple2 k v -> Effect Unit

foreign import insertNew2 :: forall k v. Table -> Tuple2 k v -> Effect Boolean

foreign import insert3 :: forall k v v2. Table -> Tuple3 k v v2 -> Effect Unit

foreign import insertList2 :: forall k v. Table -> List (Tuple2 k v) -> Effect Unit

foreign import updateCounter :: forall k. Table -> k -> UpdateOp -> Effect Int

-- Cheeky hack so we can increment non-counter
foreign import increment :: forall k v. Table -> k -> Int -> v -> Effect v

foreign import lookupElement :: forall k v. Table -> k -> Int -> Effect v

foreign import updateElement :: forall k v. Table -> k -> Int -> v -> Effect Boolean

-- Statement of intent, we should probably look at batched updates
foreign import updateElements :: forall k v. Table -> k -> List (Tuple2 Int v) -> Effect Boolean

-- Shrug again
foreign import match :: forall match result. Table -> match -> Effect (List result)

foreign import matchObject :: forall match result. Table -> match -> Effect (List result)

foreign import matchDelete :: forall match. Table -> match -> Effect Unit

-- More Shrug again
foreign import select :: forall result. Table -> SelectOp -> Effect (List result)

foreign import selectOne :: forall result. Table -> SelectOp -> Effect (Maybe result)

foreign import selectOp :: forall k v. k -> v -> SelectOp

foreign import data SelectOp :: Type

foreign import delete :: Table -> Effect Unit

foreign import delete2 :: forall k. Table -> k -> Effect Unit

foreign import lookup :: forall k v. Table -> k -> Effect (List v)

foreign import toList :: forall tuple. Table -> Effect (List tuple)

