-- | Functions for working with Erlang/OTP exceptions. The `Effect.Exception` module deals with exceptions which are generated and consumed
-- | within PureScript in a simplistic way, while this module exposes the different exception classes in the underlying platform.
module Erl.Kernel.Exceptions
  ( CatchResult
  , ErrorType(..)
  , StackTrace
  , error
  , exit
  , throw
  , try
  , tryError
  , tryExit
  , tryNamedError
  , tryNoproc
  , tryTimeout
  , tryThrown
  )
  where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Atom (Atom)
import Foreign (Foreign)

foreign import throw :: Foreign -> Effect Unit

foreign import error :: Foreign -> Effect Unit

foreign import exit :: Foreign -> Effect Unit

data ErrorType 
  = Error
  | Exit
  | Throw

foreign import data StackTrace :: Type

foreign import showStack :: StackTrace -> String

instance Show StackTrace where
  show = showStack

type CatchResult = { class :: ErrorType, reason :: Foreign, stack :: StackTrace }

-- | Try evaluating the given expression, catching any exception regardless of class (`error`, `exit`, or `throw`)
foreign import try :: forall a. Effect a -> Effect (Either CatchResult a)

-- | Try evaluating the given expression, catching any exception of class `error` (and passing through any others)
foreign import tryError :: forall a. Effect a -> Effect (Either Foreign a)

-- | Try evaluating the given expression, catching any exception of class `exit` (and passing through any others)
foreign import tryExit :: forall a. Effect a -> Effect (Either Foreign a)

-- | Try evaluating the given expression, catching any exception of class `throw` (and passing through any others)
foreign import tryThrown :: forall a. Effect a -> Effect (Either Foreign a)

-- | Try evaluating the given expression, catching any exit matching `{noproc,_}` (and passing through any others)
foreign import tryNoproc :: forall a. Effect a -> Effect (Maybe a)

-- | Try evaluating the given expression, catching any exit matching `{timeout,_}` (and passing through any others)
foreign import tryTimeout :: forall a. Effect a -> Effect (Maybe a)

-- | Try evaluating the given expression, catching any error of the given name (and passing through any others)
foreign import tryNamedError :: forall a. Atom -> Effect a -> Effect (Maybe a)