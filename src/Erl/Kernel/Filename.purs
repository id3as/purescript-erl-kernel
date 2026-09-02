-- | The currency `file` and `filelib` actually deal in: a name handed to the
-- | runtime, nothing more.
-- |
-- | This is deliberately *not* a path type. It composes nothing — there is no
-- | join, no parent, no extension — because composition is where traversal bugs
-- | come from and a bindings library has no business ruling on them. OTP's own
-- | `filename:join/2` demonstrates the trap: `join("/a", "/b") = "/b"`, an
-- | absolute right-hand side silently replacing the base. Build paths in a path
-- | library and hand the result here.
-- |
-- | It lives in its own module rather than in `Erl.Kernel.File` because
-- | `Erl.Kernel.Tcp` and `Erl.Kernel.Udp` need it for `netns` and have no
-- | business importing a filesystem API, and rather than in `Erl.Types` because
-- | the smart constructor is the whole point and PureScript's export control is
-- | per-module: in a grab-bag anything in the module can forge one.
module Erl.Kernel.Filename
  ( Filename
  , filename
  , rawFilename
  , filenameToString
  , filenameToBinary
  ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Erl.Data.Binary (Binary)
import Erl.Types (class ToErl)
import Foreign (unsafeToForeign)

-- | Backed by `Binary`, not `String`, because a POSIX filename is bytes: names
-- | that are not valid UTF-8 exist on disk and have to be representable rather
-- | than dropped. The constructor is not exported and there is no `Newtype`
-- | instance, so `filename` and `rawFilename` are the only ways in.
newtype Filename = Filename Binary

derive newtype instance eqFilename :: Eq Filename

-- | Erl.Data.Binary has no Ord, and a directory listing is a thing people sort,
-- | so borrow Erlang's own total order over binaries.
instance ordFilename :: Ord Filename where
  compare (Filename a) (Filename b) = compareBinaryImpl LT EQ GT a b

instance showFilename :: Show Filename where
  show f = maybe "(rawFilename <non-utf8>)" (\s -> "(filename " <> show s <> ")") (filenameToString f)

-- | The class is declared in `Erl.Types`, so putting the instance here keeps it
-- | in the same module as the type and avoids both an orphan and a cycle.
instance toErlFilename :: ToErl Filename where
  toErl (Filename b) = unsafeToForeign b

foreign import compareBinaryImpl :: Ordering -> Ordering -> Ordering -> Binary -> Binary -> Ordering
foreign import stringToBinary :: String -> Binary
foreign import binaryToStringImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Binary -> Maybe String

-- | The POSIX rule in full: a name is any non-empty byte sequence without NUL.
-- | `/` is deliberately allowed — a `Filename` is a whole path, not one segment.
filename :: String -> Maybe Filename
filename s
  | s == "" = Nothing
  | String.contains (String.Pattern "\x0000") s = Nothing
  | otherwise = Just $ Filename $ stringToBinary s

-- | The raw channel, for names that came off a filesystem rather than out of a
-- | program. Total by construction: these bytes are already on disk.
rawFilename :: Binary -> Filename
rawFilename = Filename

-- | `Nothing` when the name is not valid UTF-8, which is a real state for a name
-- | read from disk and the reason this returns a `Maybe` rather than a `String`.
filenameToString :: Filename -> Maybe String
filenameToString (Filename b) = binaryToStringImpl Just Nothing b

filenameToBinary :: Filename -> Binary
filenameToBinary (Filename b) = b
