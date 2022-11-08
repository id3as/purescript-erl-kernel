module Erl.Kernel.Code
  ( loadModule
  , isLoaded
  ) where

import Prelude
import Effect (Effect)
import Erl.ModuleName (ModuleName, NativeModuleName, nativeModuleName)

loadModule :: ModuleName -> Effect Unit
loadModule = loadModule_ <<< nativeModuleName

isLoaded :: ModuleName -> Effect Boolean
isLoaded = isLoaded_ <<< nativeModuleName

foreign import loadModule_ :: NativeModuleName -> Effect Unit
foreign import isLoaded_ :: NativeModuleName -> Effect Boolean
