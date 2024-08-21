module Erl.Kernel.Os
  ( cmd
  , osType
  , getEnv
  , setEnv
  , OsType(..)
  , OsFamily(..)
  , OsName(..)
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Atom (Atom)

cmd :: String -> Effect String
cmd = cmdImpl

foreign import cmdImpl :: String -> Effect String

data OsFamily
  = Unix
  | Windows

derive instance Eq OsFamily

data OsName
  = Linux
  | Darwin
  | WindowsNt
  | Other Atom

derive instance Eq OsName

data OsType = OsType OsFamily OsName

derive instance Eq OsType

foreign import osType :: Effect OsType

foreign import getEnv :: String -> Effect (Maybe String)

foreign import setEnv :: String -> String -> Effect Unit
