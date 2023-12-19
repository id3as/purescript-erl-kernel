module Erl.Kernel.File
  ( PosixError(..)
  , FileError(..)
  , FileHandle
  , FileOpenMode(..)
  , FileOutputType(..)
  , FilePositioning(..)
  , FileDelayedWrite(..)
  , FileReadAhead(..)
  , Encoding(..)
  , open
  , read
  , readFile
  , rename
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
  , fileErrorToPurs
  , listDir
  , fileToString
  , dirToString
  , pathToString
  , fileExtension
  ) where

import Prelude hiding (join)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Types (SandboxedDir, SandboxedFile)
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, Parser, Path, RelDir, RelFile, SandboxedPath, extension, fileName, parseAbsDir, parseRelDir, parseRelFile, posixParser, posixPrinter, printPath, unsandbox)
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

foreign import fileErrorToPurs :: Foreign -> FileError

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
  -> String
  -> Record (modes :: List FileOpenMode | options)
  -> Effect (Either FileError FileHandle)

foreign import readImpl
  :: FileHandle
  -> Int
  -> Effect (Either FileError Binary)

foreign import readFileImpl
  :: (FileError -> Either FileError Binary)
  -> (Binary -> Either FileError Binary)
  -> String
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
  -> String
  -> IOData
  -> Effect (Either FileError Unit)

foreign import renameImpl
  :: (FileError -> Either FileError Unit)
  -> Either FileError Unit
  -> String
  -> String
  -> Effect (Either FileError Unit)

foreign import closeImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> FileHandle
  -> Effect (Either FileError Unit)

foreign import deleteImpl
  :: (FileError -> Either FileError Unit)
  -> (Either FileError Unit)
  -> String
  -> Effect (Either FileError Unit)

foreign import listDirImpl
  :: (FileError -> Either FileError (List String))
  -> (List String -> Either FileError (List String))
  -> String
  -> Effect (Either FileError (List (Either String String)))

-- todo - not a string output
listDir
  :: SandboxedDir
  -> Effect (Either FileError (List (Either RelDir RelFile)))
listDir dir = do
  res <- listDirImpl Left Right $ dirToString dir
  case res of
    Left err -> pure $ Left err
    Right entries -> pure $ Right $ toPath <$> entries

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
  :: (FileError -> Either FileError String)
  -> (String -> Either FileError String)
  -> Effect (Either FileError String)

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
  => SandboxedFile
  -> Record (modes :: List FileOpenMode | options)
  -> Effect (Either FileError FileHandle)
open file opts =
  openImpl Left Right defaultFileOpenOptions (fileToString file) opts

read :: FileHandle -> Int -> Effect (Either FileError Binary)
read = readImpl

close :: FileHandle -> Effect (Either FileError Unit)
close = closeImpl Left (Right unit)

delete :: SandboxedFile -> Effect (Either FileError Unit)
delete = deleteImpl Left (Right unit) <<< fileToString

sync :: FileHandle -> Effect (Either FileError Unit)
sync = syncImpl Left (Right unit)

write :: FileHandle -> IOData -> Effect (Either FileError Unit)
write = writeImpl Left (Right unit)

writeFile :: SandboxedFile -> IOData -> Effect (Either FileError Unit)
writeFile = writeFileImpl Left (Right unit) <<< fileToString

readFile :: SandboxedFile -> Effect (Either FileError Binary)
readFile = readFileImpl Left Right <<< fileToString

rename :: SandboxedFile -> SandboxedFile -> Effect (Either FileError Unit)
rename source dest = renameImpl Left (Right unit) (fileToString source) (fileToString dest)

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

cwd :: Effect (Either FileError (Path Abs Dir))
cwd = do
  res <- cwdImpl Left Right
  case res of
    Left err -> pure $ Left err
    Right dir ->
      pure $ Right $ unsafeFromString parseAbsDir dir

pathToString :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath a b -> String
pathToString = printPath posixPrinter

fileToString :: SandboxedFile -> String
fileToString (Left abs) = pathToString abs
fileToString (Right rel) = pathToString rel

dirToString :: SandboxedDir -> String
dirToString (Left abs) = pathToString abs
dirToString (Right rel) = pathToString rel

fileExtension :: SandboxedFile -> Maybe NonEmptyString
fileExtension file = do
  extension $ case file of
    Left abs -> fileName $ unsandbox abs
    Right rel -> fileName $ unsandbox rel

toPath :: Either String String -> Either RelDir RelFile
toPath (Left dir) =
  Left $ unsafeFromString parseRelDir dir
toPath (Right file) =
  Right $ unsafeFromString parseRelFile file

unsafeFromString :: forall a b. (Parser -> String -> Maybe (Path a b)) -> String -> Path a b
unsafeFromString parser str =
  case parser posixParser str of
    Just val -> val
    Nothing -> unsafeCrashWith "impossible, cannot get invalid paths from ffi call"
