module Erl.Kernel.Code
  ( loadModule
  ) where

import Prelude
import Effect (Effect)
import Erl.ModuleName (ModuleName, NativeModuleName, nativeModuleName)

loadModule :: ModuleName -> Effect Unit
loadModule = loadModule_ <<< nativeModuleName

foreign import loadModule_ :: NativeModuleName -> Effect Unit
