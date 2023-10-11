module Erl.Kernel.File
  ( Directory(..)
  , PosixError(..)
  , FileError(..)
  , FileHandle
  , FileName(..)
  , FileOpenMode(..)
  , FileOutputType(..)
  , FilePositioning(..)
  , FileDelayedWrite(..)
  , FileReadAhead(..)
  , Encoding(..)
  , open
  , read
  , readFile
  , write
  , writeFile
  , sync
  , seek
  , length
  , close
  , copy
  , delete
  , cwd
  , posixErrorToPurs
  , isDir
  ) where

import Prelude hiding (join)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Types (class ToErl)
import Foreign (Foreign, unsafeToForeign)
import Prim.Row as Row

data PosixError
  = EAcces
  | EAgain
  | EBadf
  | EBadmsg
  | EBusy
  | EDeadlk
  | EDeadlock
  | EDquot
  | EExist
  | EFault
  | EFbig
  | EFtype
  | EIntr
  | EInval
  | EIo
  | EIsdir
  | ELoop
  | EMfile
  | EMlink
  | EMultihop
  | ENametoolong
  | ENfile
  | ENobufs
  | ENodev
  | ENolck
  | ENolink
  | ENoent
  | ENomem
  | ENospc
  | ENosr
  | ENostr
  | ENosys
  | ENotblk
  | ENotdir
  | ENotsup
  | ENxio
  | EOpnotsupp
  | EOverflow
  | EPerm
  | EPipe
  | ERange
  | ERofs
  | ESpipe
  | ESrch
  | EStale
  | ETxtbsy
  | EXdev

derive instance eq_PosixError :: Eq PosixError

--derive instance posixError_generic :: Generic PosixError _
instance posixError_show :: Show PosixError where
  show _ = "file posix" --genericShow

foreign import posixErrorToPurs :: Foreign -> Maybe PosixError

data FileError
  = Eof
  | BadArg
  | SystemLimit
  | Terminated
  | NoTranslation
  | Posix PosixError
  | Other Foreign

instance fileError_show :: Show FileError where
  show Eof = "eof"
  show BadArg = "bad arg"
  show SystemLimit = "system limit"
  show Terminated = "terminated"
  show NoTranslation = "no translation"
  show (Posix posixError) = "posix:" <> show posixError
  show (Other _other) = "other"

foreign import data FileHandle :: Type

foreign import openImpl
  :: forall options
   . (FileError -> Either FileError FileHandle)
  -> (FileHandle -> Either FileError FileHandle)
  -> Record (FileOpenOptions)
  -> FileName
  -> Record (modes :: List FileOpenMode | options)
  -> Effect (Either FileError FileHandle)

foreign import readImpl
  :: FileHandle
  -> Int
  -> Effect (Either FileError Binary)

foreign import readFileImpl
  :: (FileError -> Either FileError Binary)
  -> (Binary -> Either FileError Binary)
  -> FileName
  -> Effect (Either FileError Binary)

foreign import writeImpl
  :: (FileError -> Either FileError IOData)
  -> (Either FileError Unit)
  -> FileHandle
  -> IOData
  -> Effect (Either FileError Unit)

foreign import writeFileImpl
  :: (FileError -> Either FileError IOData)
  -> (Either FileError Unit)
  -> FileName
  -> IOData
  -> Effect (Either FileError Unit)

foreign import join :: FileName -> FileName -> FileName

foreign import closeImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> FileHandle
  -> Effect (Either FileError Unit)

foreign import deleteImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> FileName
  -> Effect (Either FileError Unit)

foreign import isDir
  :: Directory
  -> Effect Boolean

foreign import syncImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> FileHandle
  -> Effect (Either FileError Unit)

foreign import seekImpl
  :: (FileError -> Either FileError Int)
  -> (Int -> Either FileError Int)
  -> FileHandle
  -> FilePositioning
  -> Int
  -> Effect (Either FileError Int)

foreign import copyImpl
  :: (FileError -> Either FileError Int)
  -> (Int -> Either FileError Int)
  -> FileHandle
  -> FileHandle
  -> Maybe Int
  -> Effect (Either FileError Int)

foreign import cwdImpl
  :: (FileError -> Either FileError FileName)
  -> (FileName -> Either FileError FileName)
  -> Effect (Either FileError FileName)

data FileOpenMode
  = Read
  | Write
  | Append
  | Exclusive

data FileOutputType
  = List
  | Binary

data FileDelayedWrite
  = DelayedWriteDefault
  | DelayedWrite Int Int

data FileReadAhead
  = ReadAheadDefault
  | ReadAhead Int

data Encoding
  = Latin1
  | Utf8
  | Utf16Big
  | Utf16Little
  | Utf32Big
  | Utf32Little

data FilePositioning
  = FromBeginning
  | FromCurrent
  | FromEnd

type FileOpenOptions =
  ( modes :: List FileOpenMode
  , raw :: Boolean
  , output :: FileOutputType
  , delayedWrite :: Maybe FileDelayedWrite
  , readAhead :: Maybe FileReadAhead
  , compressed :: Boolean
  , encoding :: Maybe Encoding
  , ram :: Boolean
  , sync :: Boolean
  , directory :: Boolean
  )

defaultFileOpenOptions :: Record (FileOpenOptions)
defaultFileOpenOptions =
  { modes: List.singleton Read
  , raw: true
  , output: Binary
  , delayedWrite: Nothing
  , readAhead: Nothing
  , compressed: false
  , encoding: Nothing
  , ram: false
  , sync: false
  , directory: false
  }

-- type Fetch
--    = forall options trash
--    . Union options trash Options
--   => URL
--   -> Record (method :: Method | options)
--   -> Aff Response
open
  :: forall options trash
   . Row.Union options trash FileOpenOptions
  => FileName
  -> Record (modes :: List FileOpenMode | options)
  -> Effect (Either FileError FileHandle)
open = openImpl Left Right defaultFileOpenOptions

read :: FileHandle -> Int -> Effect (Either FileError Binary)
read = readImpl

close :: FileHandle -> Effect (Either FileError Unit)
close = closeImpl Left (Right unit)

delete :: FileName -> Effect (Either FileError Unit)
delete = deleteImpl Left (Right unit)

sync :: FileHandle -> Effect (Either FileError Unit)
sync = syncImpl Left (Right unit)

write :: FileHandle -> IOData -> Effect (Either FileError Unit)
write = writeImpl Left (Right unit)

writeFile :: FileName -> IOData -> Effect (Either FileError Unit)
writeFile = writeFileImpl Left (Right unit)

readFile :: FileName -> Effect (Either FileError Binary)
readFile = readFileImpl Left Right

seek :: FileHandle -> FilePositioning -> Int -> Effect (Either FileError Int)
seek = seekImpl Left Right

length :: FileHandle -> Effect (Either FileError Int)
length file =
  seek file FromCurrent 0 >>= case _ of
    Left e -> pure $ Left e
    Right current ->
      seek file FromEnd 0 >>= case _ of
        Left e -> pure $ Left e
        Right theLength ->
          map (const theLength) <$> seek file FromBeginning current

copy :: FileHandle -> FileHandle -> Maybe Int -> Effect (Either FileError Int)
copy = copyImpl Left Right

cwd :: Effect (Either FileError FileName)
cwd = cwdImpl Left Right

newtype FileName = FileName String

derive instance Newtype FileName _
derive newtype instance Eq FileName
derive newtype instance Ord FileName

instance toErl_Filename :: ToErl FileName where
  toErl = unsafeToForeign

instance semiFileName :: Semigroup FileName where
  append = join

newtype Directory = Directory String

derive instance Newtype Directory _
derive newtype instance Eq Directory
derive newtype instance Ord Directory
derive newtype instance Show Directory
