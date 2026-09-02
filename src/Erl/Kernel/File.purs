module Erl.Kernel.File
  ( Encoding(..)
  , FileDelayedWrite(..)
  , FileError(..)
  , FileHandle
  , FileOpenMode(..)
  , FileOutputType(..)
  , FilePositioning(..)
  , FileReadAhead(..)
  , Location(..)
  , PosixError(..)
  , close
  , copy
  , cwd
  , delDir
  , delDirR
  , delete
  , fileErrorToPurs
  , length
  , listDir
  , makeDir
  , open
  , posixErrorToPurs
  , pread
  , pwrite
  , read
  , readFile
  , rename
  , seek
  , sync
  , truncate
  , write
  , writeFile
  -- so an ordinary call site needs only this module
  , module Erl.Kernel.Filename
  )
  where

import Prelude hiding (join)

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Tuple (tuple2)
import Erl.Kernel.Filename (Filename, filename, filenameToBinary, filenameToString, rawFilename)
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
derive instance generic_PosixError :: Generic PosixError _

instance posixError_show :: Show PosixError where
  show = genericShow

foreign import posixErrorToPurs :: Foreign -> Maybe PosixError

foreign import fileErrorToPurs :: Foreign -> FileError

data Location
  = LocationDirect Int
  | LocationBof Int
  | LocationCur Int
  | LocationEof Int

locationToFfi :: Location -> Foreign
locationToFfi (LocationDirect number) = unsafeToForeign number
locationToFfi (LocationBof number) = unsafeToForeign $ tuple2 (atom "bof") number
locationToFfi (LocationCur number) = unsafeToForeign $ tuple2 (atom "cur") number
locationToFfi (LocationEof number) = unsafeToForeign $ tuple2 (atom "eof") number

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

foreign import delDirImpl
  :: (FileError -> Either FileError IOData)
  -> (Either FileError Unit)
  -> Filename
  -> Effect (Either FileError Unit)

foreign import delDirRImpl
  :: (FileError -> Either FileError IOData)
  -> (Either FileError Unit)
  -> Filename
  -> Effect (Either FileError Unit)

foreign import makeDirImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> Filename
  -> Effect (Either FileError Unit)

foreign import openImpl
  :: forall options
   . (FileError -> Either FileError FileHandle)
  -> (FileHandle -> Either FileError FileHandle)
  -> Record (FileOpenOptions)
  -> Filename
  -> Record (modes :: List FileOpenMode | options)
  -> Effect (Either FileError FileHandle)

foreign import readImpl
  :: FileHandle
  -> Int
  -> Effect (Either FileError Binary)

foreign import preadImpl
  :: FileHandle
  -> Foreign
  -> Int
  -> Effect (Either FileError Binary)

foreign import readFileImpl
  :: (FileError -> Either FileError Binary)
  -> (Binary -> Either FileError Binary)
  -> Filename
  -> Effect (Either FileError Binary)

foreign import writeImpl
  :: (FileError -> Either FileError IOData)
  -> (Either FileError Unit)
  -> FileHandle
  -> IOData
  -> Effect (Either FileError Unit)

foreign import pwriteImpl
  :: (FileError -> Either FileError IOData)
  -> (Either FileError Unit)
  -> FileHandle
  -> Foreign
  -> IOData
  -> Effect (Either FileError Unit)

foreign import writeFileImpl
  :: (FileError -> Either FileError IOData)
  -> (Either FileError Unit)
  -> Filename
  -> IOData
  -> Effect (Either FileError Unit)

foreign import renameImpl
  :: (FileError -> Either FileError Unit)
  -> Either FileError Unit
  -> Filename
  -> Filename
  -> Effect (Either FileError Unit)

foreign import closeImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> FileHandle
  -> Effect (Either FileError Unit)

foreign import deleteImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> Filename
  -> Effect (Either FileError Unit)

foreign import listDirImpl
  :: (FileError -> Either FileError (List Filename))
  -> (List Filename -> Either FileError (List Filename))
  -> Filename
  -> Effect (Either FileError (List Filename))

-- | The entry names as they are on disk — no classification, and no trailing
-- | separator on directories.
-- |
-- | It used to return `Either RelDir RelFile`, deciding with
-- | `filelib:is_dir/1` on the bare entry name, which resolves against the
-- | *process cwd* rather than the directory being listed: unless the two
-- | happened to coincide, every subdirectory came back typed as a file. Classify
-- | here and you also buy a stat per entry and a TOCTOU window between the list
-- | and the stat. Callers that want the distinction should join the entry onto
-- | the directory and ask.
listDir :: Filename -> Effect (Either FileError (List Filename))
listDir = listDirImpl Left Right

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


foreign import truncateImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> FileHandle
  -> Effect (Either FileError Unit)

foreign import copyImpl
  :: (FileError -> Either FileError Int)
  -> (Int -> Either FileError Int)
  -> FileHandle
  -> FileHandle
  -> Maybe Int
  -> Effect (Either FileError Int)

foreign import cwdImpl
  :: (FileError -> Either FileError Filename)
  -> (Filename -> Either FileError Filename)
  -> Effect (Either FileError Filename)

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
  => Filename
  -> Record (modes :: List FileOpenMode | options)
  -> Effect (Either FileError FileHandle)
open file opts =
  openImpl Left Right defaultFileOpenOptions file opts

delDir :: Filename -> Effect (Either FileError Unit)
delDir = delDirImpl Left (Right unit)

delDirR :: Filename -> Effect (Either FileError Unit)
delDirR = delDirRImpl Left (Right unit)

makeDir :: Filename -> Effect (Either FileError Unit)
makeDir = makeDirImpl Left (Right unit)

read :: FileHandle -> Int -> Effect (Either FileError Binary)
read = readImpl

pread :: FileHandle -> Location -> Int -> Effect (Either FileError Binary)
pread handle location amount = preadImpl handle (locationToFfi location) amount

close :: FileHandle -> Effect (Either FileError Unit)
close = closeImpl Left (Right unit)

delete :: Filename -> Effect (Either FileError Unit)
delete = deleteImpl Left (Right unit)

sync :: FileHandle -> Effect (Either FileError Unit)
sync = syncImpl Left (Right unit)

write :: FileHandle -> IOData -> Effect (Either FileError Unit)
write = writeImpl Left (Right unit)

pwrite :: FileHandle -> Location -> IOData -> Effect (Either FileError Unit)
pwrite handle location iodata = pwriteImpl Left (Right unit) handle (locationToFfi location) iodata

writeFile :: Filename -> IOData -> Effect (Either FileError Unit)
writeFile = writeFileImpl Left (Right unit)

readFile :: Filename -> Effect (Either FileError Binary)
readFile = readFileImpl Left Right

rename :: Filename -> Filename -> Effect (Either FileError Unit)
rename = renameImpl Left (Right unit)

seek :: FileHandle -> FilePositioning -> Int -> Effect (Either FileError Int)
seek = seekImpl Left Right

truncate :: FileHandle -> Effect (Either FileError Unit)
truncate = truncateImpl Left (Right unit)

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

-- | Exactly what `file:get_cwd/0` returns — no trailing separator appended.
cwd :: Effect (Either FileError Filename)
cwd = cwdImpl Left Right
