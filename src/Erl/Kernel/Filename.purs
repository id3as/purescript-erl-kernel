-- | Two types, one module: `Filename`, the currency `file` and `filelib`
-- | actually deal in, and `Path`, the thing you compose to build one.
-- |
-- | They are different things, which is why they are different types.
-- |
-- | * `Filename` is **what the runtime hands back and takes**. `Binary`-backed,
-- |   so a non-UTF-8 name on disk is representable rather than dropped --
-- |   `file:list_dir_all/1` returns them. It composes nothing: there is no
-- |   join, no parent, no extension. OTP's own `filename:join/2` demonstrates
-- |   why a bindings library should not offer one: `join("/a", "/b") = "/b"`,
-- |   an absolute right-hand side silently replacing the base, which is the
-- |   canonical traversal primitive.
-- | * `Path` is **what you compose**. `String`-backed and normalised on
-- |   construction, with `Abs`/`Rel` and `Dir`/`File` in phantoms so
-- |   `absDir </> relFile` is provably inside `absDir`.
-- |
-- | The bridge is one function, `toFilename :: Path Abs b -> Filename`, and it
-- | deliberately has **no `Rel` overload**. Handing the operating system a
-- | relative name is a read whose target depends on the emulator's cwd, which
-- | is per-node rather than per-process and can be moved by any process in the
-- | system. If you want to give the filesystem a relative path, supply the
-- | base.
-- |
-- | Both constructors are unexported and neither type has a `Newtype`
-- | instance, so the smart constructors are the only way in. That is the whole
-- | point of the module, and it is why these live here rather than in
-- | `Erl.Types`: PureScript's export control is per-module, so in a grab-bag
-- | anything in the module can forge one.
-- |
-- | ### What `Path` rejects, and why it matters
-- |
-- | `..` is rejected at parse time. This is Haskell `path`'s central trick:
-- | narrow the representation and containment stops needing a runtime witness.
-- | It is also the security-relevant part. A structural path type that
-- | *resolves* `..` while parsing silently launders a traversal string into a
-- | legal absolute path -- `"/data/../../../etc/passwd"` parses, successfully,
-- | as `/etc/passwd`. Rejecting the input is the only thing that closes that.
-- |
-- | Note what this is not: containment is **not decidable from a string**, and
-- | nothing here claims otherwise. A `..`-free relative path still escapes
-- | through a symlink. OTP re-sorted itself along the same line when
-- | `filename:safe_relative_path/1` was removed in favour of
-- | `filelib:safe_relative_path/2`, which takes a `Cwd` because it has to
-- | resolve links. The security boundary is the OS and the container; this is
-- | lexical normalisation, and worth having on those terms.
module Erl.Kernel.Filename
  ( Filename
  , filename
  , rawFilename
  , filenameToString
  , filenameToBinary
  -- Phantoms
  , RelOrAbs
  , Rel
  , Abs
  , DirOrFile
  , Dir
  , File
  , class IsRelOrAbs
  , isAbs
  , class IsDirOrFile
  , dirSep
  -- Names
  , Name
  , name
  , nameToString
  , class IsName
  , reflectName
  , class NoSlash
  , splitName
  , joinName
  , extension
  , coerceName
  -- Paths
  , Path
  , AbsDir
  , AbsFile
  , RelDir
  , RelFile
  , AnyPath
  , AbsPath
  , RelPath
  , AnyDir
  , AnyFile
  , rootDir
  , currentDir
  , dir
  , dir'
  , file
  , file'
  , extendPath
  , appendPath
  , (</>)
  , printPath
  , parseAbsDir
  , parseAbsFile
  , parseRelDir
  , parseRelFile
  , peel
  , peelFile
  , pathName
  , fileName
  , rename
  , setExtension
  , (<.>)
  , toFilename
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Symbol (reflectSymbol) as Symbol
import Data.Tuple (Tuple(..), snd)
import Erl.Data.Binary (Binary)
import Erl.Types (class ToErl)
import Foreign (unsafeToForeign)
import Prim.Symbol (class Cons)
import Type.Data.Boolean (False) as Symbol
import Type.Data.Symbol (class Equals) as Symbol
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Filename
--------------------------------------------------------------------------------

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
-- | `/` is deliberately allowed -- a `Filename` is a whole path, not one
-- | segment.
-- |
-- | Relative and absolute are both accepted, and neither is privileged here.
-- | `listDir` returns bare entry names, so a relative `Filename` is the normal
-- | output of this module rather than an edge case. The runtime resolves one
-- | against the emulator's cwd, which is process-global and mutable, so whether
-- | that is acceptable is a question about the caller and not about the name.
-- | `toFilename` is where this module states its answer: supply a base.
filename :: String -> Maybe Filename
filename s
  | s == "" = Nothing
  | String.contains (String.Pattern "\x0000") s = Nothing
  | otherwise = Just $ Filename $ stringToBinary s

-- | The raw channel, for names that came off a filesystem rather than out of a
-- | program. Total by construction: these bytes are already on disk.
-- |
-- | A caution this has already cost once: when a type has a validating
-- | constructor and a raw one, the raw one is where the bugs go. `os:cmd`
-- | output is neither on disk nor validated -- `mktemp -q` is silent on failure
-- | and `os:cmd` returns `""`, so a `rawFilename` of that holds no name at all.
-- | Audit the call sites of this, not just the parses.
rawFilename :: Binary -> Filename
rawFilename = Filename

-- | `Nothing` when the name is not valid UTF-8, which is a real state for a
-- | name read from disk and the reason this returns a `Maybe` rather than a
-- | `String`.
filenameToString :: Filename -> Maybe String
filenameToString (Filename b) = binaryToStringImpl Just Nothing b

filenameToBinary :: Filename -> Binary
filenameToBinary (Filename b) = b

--------------------------------------------------------------------------------
-- Phantoms
--------------------------------------------------------------------------------

-- | The kind for the relative/absolute phantom type.
data RelOrAbs

-- | The phantom type of relative paths.
foreign import data Rel :: RelOrAbs

-- | The phantom type of absolute paths.
foreign import data Abs :: RelOrAbs

-- | Lets a signature abstract over `RelOrAbs` while still being able to ask
-- | which it got. `isAbs` is the whole of what the representation needs to
-- | know: an `Abs` path's string starts with `/` and a `Rel` path's does not.
class IsRelOrAbs :: RelOrAbs -> Constraint
class IsRelOrAbs a where
  isAbs :: forall proxy. proxy a -> Boolean

instance relIsRelOrAbs :: IsRelOrAbs Rel where
  isAbs _ = false

instance absIsRelOrAbs :: IsRelOrAbs Abs where
  isAbs _ = true

-- | The kind for the directory/file phantom type.
data DirOrFile

-- | The phantom type of directories.
foreign import data Dir :: DirOrFile

-- | The phantom type of files.
foreign import data File :: DirOrFile

-- | As `IsRelOrAbs`, and again the member is the one thing the representation
-- | turns on: a `Dir`'s string ends with a separator and a `File`'s does not.
-- |
-- | That trailing separator is not cosmetic. `filelib:ensure_dir/1` creates the
-- | directory that *contains* the name it is given, so `"/a/b/c"` creates
-- | `/a/b` and `"/a/b/c/"` creates `/a/b/c`; the separator is how a caller says
-- | which it meant. Keeping it in the stored string is also what makes an `Abs`
-- | path print byte-identically to the structural representation this replaced.
class IsDirOrFile :: DirOrFile -> Constraint
class IsDirOrFile b where
  dirSep :: forall proxy. proxy b -> String

instance isDirOrFileDir :: IsDirOrFile Dir where
  dirSep _ = "/"

instance isDirOrFileFile :: IsDirOrFile File where
  dirSep _ = ""

--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

-- | One path segment, indexed by `DirOrFile`. The phantom says what the segment
-- | is going to be used as; it is not a claim about the filesystem, which is
-- | the only entity that knows.
-- |
-- | The constructor is unexported and there is no `Newtype` instance, so the
-- | four clauses below are real rather than decorative.
newtype Name :: DirOrFile -> Type
newtype Name b = Name String

derive newtype instance eqName :: Eq (Name b)
derive newtype instance ordName :: Ord (Name b)
derive newtype instance showName :: Show (Name b)

-- | Lawful, and it is what keeps the callers that build a name by prefixing
-- | total. Two valid names concatenated are non-empty, contain no `/` and no
-- | NUL, and cannot be `.` or `..` -- that would need both operands to be `.`,
-- | which is not a valid `Name`. Append is `String` append, so associativity is
-- | free.
instance semigroupName :: Semigroup (Name b) where
  append (Name a) (Name b) = Name (a <> b)

-- | The entire POSIX rule for a single segment: non-empty, no `/`, no NUL, and
-- | not `.` or `..`.
-- |
-- | The last two clauses are the ones that matter. They are why there is no
-- | escaper here: the structural representation this replaced ran a
-- | `posixEscaper` implicitly inside every print, silently rewriting `/` to `-`
-- | and `..` to `$dot$dot`. Silent corruption became a wrong-but-valid
-- | filename. Rejecting is the right answer, and it is a behaviour change at
-- | any site that was relying on the mangling.
name :: forall b. String -> Maybe (Name b)
name s
  | s == "" = Nothing
  | s == "." || s == ".." = Nothing
  | String.contains (String.Pattern "/") s = Nothing
  | String.contains (String.Pattern "\x0000") s = Nothing
  | otherwise = Just (Name s)

nameToString :: forall b. Name b -> String
nameToString (Name s) = s

-- | The `DirOrFile` phantom is decorative on a `Name` -- the string is the same
-- | either way -- so recasting one is total. Used where a name that was read as
-- | a file is going to be appended as a directory, which is a statement about
-- | the caller's intent and not about the bytes.
coerceName :: forall b b'. Name b -> Name b'
coerceName (Name s) = Name s

-- | Splits a name into its stem and extension.
-- |
-- | ```purescript
-- | splitName (name ".foo")    == { name: ".foo", ext: Nothing }
-- | splitName (name "foo.")    == { name: "foo.", ext: Nothing }
-- | splitName (name "foo")     == { name: "foo",  ext: Nothing }
-- | splitName (name "foo.baz") == { name: "foo",  ext: Just "baz" }
-- | ```
-- |
-- | The stem and the extension both come back as `Name`s rather than as raw
-- | strings, which is what makes `joinName` total -- see there.
splitName :: forall b. Name b -> { name :: Name b, ext :: Maybe (Name b) }
splitName n@(Name s) =
  case String.lastIndexOf (String.Pattern ".") s of
    Nothing -> { name: n, ext: Nothing }
    Just idx ->
      let
        stem = String.take idx s
        ext = String.drop (idx + 1) s
      in
        if stem == "" || ext == "" then { name: n, ext: Nothing }
        else { name: Name stem, ext: Just (Name ext) }

-- | The inverse of `splitName`, and **total**: both halves are already valid
-- | `Name`s, so the join is non-empty, has no `/` and no NUL, and contains a
-- | `.` with something either side of it and so cannot be `.` or `..`.
-- |
-- | Taking a `Name` for the extension rather than a raw string is the whole
-- | reason this needs no `Maybe`. It is the same `Semigroup` argument.
joinName :: forall b. { name :: Name b, ext :: Maybe (Name b) } -> Name b
joinName { name: Name n, ext } = case ext of
  Nothing -> Name n
  Just (Name e) -> Name (n <> "." <> e)

-- | The extension of a name, if it has one. See `splitName` for the edge cases.
extension :: forall b. Name b -> Maybe (Name b)
extension = splitName >>> _.ext

--------------------------------------------------------------------------------
-- Type-level names
--------------------------------------------------------------------------------

-- | Creates a `Name` from a type-level string, so a literal segment is checked
-- | at compile time and costs nothing at runtime.
-- |
-- | The instance below enforces the same four clauses `name` does, at the type
-- | level, which is what lets it hand back a `Name` without a `Maybe` and
-- | without forging one. The structural representation this replaced could not:
-- | its `reflectName` was an `unsafeCoerce` over a reflected `Symbol` that
-- | checked only non-emptiness, so `dir @"a/b"` produced an invalid `Name` and
-- | the printer quietly rewrote it.
class IsName :: Symbol -> Constraint
class IsName sym where
  reflectName :: forall proxy b. proxy sym -> Name b

instance isNameSymbol ::
  ( IsSymbol s
  , Symbol.Equals s "" Symbol.False
  , Symbol.Equals s "." Symbol.False
  , Symbol.Equals s ".." Symbol.False
  , NoSlash s
  ) =>
  IsName s where
  reflectName _ = Name (Symbol.reflectSymbol (Proxy :: Proxy s))

-- | Holds for symbols containing no `/`. Structural recursion over
-- | `Prim.Symbol.Cons`, so a literal that does contain one fails to resolve
-- | rather than silently becoming two segments.
class NoSlash :: Symbol -> Constraint
class NoSlash s

instance noSlashEmpty :: NoSlash ""
else instance noSlashCons ::
  ( Cons h t s
  , Symbol.Equals h "/" Symbol.False
  , NoSlash t
  ) =>
  NoSlash s

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

-- | A path, indexed by whether it is relative or absolute and whether it names
-- | a directory or a file.
-- |
-- | The representation is the rendered form, normalised on construction, so
-- | `printPath` is `unwrap` and `Eq`/`Ord`/`Show` are the underlying `String`
-- | instances. The invariants every constructor in this module maintains:
-- |
-- | * an `Abs` string starts with `/`; a `Rel` string does not
-- | * a `Dir` string ends with `/`; a `File` string does not
-- | * no segment is empty, `.`, or `..`, and none contains a NUL
-- |
-- | Those together are what make `appendPath` a plain string append, and what
-- | make `absDir </> relFile` provably inside `absDir`.
-- |
-- | Note `Ord` is therefore the string order, which is not the structural order
-- | it replaces: `/a-b` and `/a/b` swap. Nothing here sorts paths, but it is an
-- | observable change for anyone who does.
newtype Path :: RelOrAbs -> DirOrFile -> Type
newtype Path a b = Path String

type role Path nominal nominal

derive newtype instance eqPath :: Eq (Path a b)
derive newtype instance ordPath :: Ord (Path a b)
derive newtype instance showPath :: Show (Path a b)

-- | A directory whose location is given relative to some other, unspecified
-- | directory.
type RelDir = Path Rel Dir

-- | A directory whose location is absolutely specified.
type AbsDir = Path Abs Dir

-- | A file whose location is given relative to some other, unspecified
-- | directory.
type RelFile = Path Rel File

-- | A file whose location is absolutely specified.
type AbsFile = Path Abs File

-- | A file or directory path at a known relative-or-absolute.
type AnyPath a = Either (Path a Dir) (Path a File)

type RelPath = AnyPath Rel

type AbsPath = AnyPath Abs

-- | An absolute or relative directory path.
type AnyDir = Either AbsDir RelDir

-- | An absolute or relative file path.
type AnyFile = Either AbsFile RelFile

-- | The root directory.
rootDir :: Path Abs Dir
rootDir = Path "/"

-- | The "current directory" -- the empty relative path, so that
-- | `currentDir </> p == p`.
currentDir :: Path Rel Dir
currentDir = Path ""

-- | A relative directory of the given literal name.
dir :: forall s proxy. IsName s => proxy s -> Path Rel Dir
dir = dir' <<< reflectName

-- | A relative directory of the given name.
dir' :: Name Dir -> Path Rel Dir
dir' (Name n) = Path (n <> "/")

-- | A relative file of the given literal name.
file :: forall s proxy. IsName s => proxy s -> Path Rel File
file = file' <<< reflectName

-- | A relative file of the given name.
file' :: Name File -> Path Rel File
file' (Name n) = Path n

-- | Extends a directory with one further segment.
extendPath :: forall a b. IsDirOrFile b => Path a Dir -> Name b -> Path a b
extendPath (Path p) n@(Name s) = Path (p <> s <> dirSep (proxyOfName n))

proxyOfName :: forall b. Name b -> Proxy b
proxyOfName _ = Proxy

proxyOfPath :: forall a b. Path a b -> Proxy b
proxyOfPath _ = Proxy

-- | Appends a relative path to a directory.
-- |
-- | This is where the narrowed representation pays for itself. The right-hand
-- | side cannot be absolute and cannot ascend, so the result is provably inside
-- | the left -- no `Maybe`, no runtime containment witness. Contrast
-- | `filename:join/2`, where an absolute right-hand side silently replaces the
-- | base.
appendPath :: forall a b. Path a Dir -> Path Rel b -> Path a b
appendPath (Path p) (Path q) = Path (p <> q)

infixl 6 appendPath as </>

-- | The path as a string. The representation is the rendered form, so this is
-- | `unwrap`: there is no printer, no escaper, and nothing is decided here that
-- | was not already decided at construction.
printPath :: forall a b. Path a b -> String
printPath (Path s) = s

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Splits a path string into segments, rejecting the input outright if any
-- | segment is `..` or contains a NUL. Empty segments and `.` segments are
-- | dropped, so `/foo/././//bar` normalises to `/foo/bar`.
-- |
-- | `..` is a rejection rather than a resolution. Resolving it is what turns
-- | `"/data/../../../etc/passwd"` into a perfectly good `/etc/passwd`, which is
-- | the laundering this type exists to stop.
segmentsOf :: String -> Maybe (Array String)
segmentsOf s =
  let
    raw = String.split (String.Pattern "/") s
    kept = Array.filter (\seg -> seg /= "" && seg /= ".") raw
  in
    if Array.any (\seg -> seg == ".." || String.contains (String.Pattern "\x0000") seg) kept then Nothing
    else Just kept

startsWithSlash :: String -> Boolean
startsWithSlash s = String.take 1 s == "/"

endsWithSlash :: String -> Boolean
endsWithSlash s = String.length s > 0 && String.drop (String.length s - 1) s == "/"

-- | Parses a directory.
-- |
-- | A trailing slash positively asserts `Dir`; its absence asserts nothing, so
-- | `parseAbsDir "/a/b"` succeeds. That is a relaxation of the structural
-- | parser, which read a missing separator as "this names a file" and so
-- | rejected most directory strings anyone would actually write. No string that
-- | parsed before changes its value; strictly more strings parse as `Dir`.
parseAbsDir :: String -> Maybe (Path Abs Dir)
parseAbsDir s
  | not (startsWithSlash s) = Nothing
  | otherwise = (\segs -> Path ("/" <> foldSegs segs)) <$> segmentsOf s

parseRelDir :: String -> Maybe (Path Rel Dir)
parseRelDir s
  | s == "" = Nothing
  | startsWithSlash s = Nothing
  | otherwise = (\segs -> Path (foldSegs segs)) <$> segmentsOf s

-- | Parses a file. A trailing slash asserts `Dir`, so a string carrying one is
-- | rejected here, and so is a string with no segments at all.
parseAbsFile :: String -> Maybe (Path Abs File)
parseAbsFile s
  | not (startsWithSlash s) = Nothing
  | endsWithSlash s = Nothing
  | otherwise = do
      segs <- segmentsOf s
      if Array.null segs then Nothing
      else Just (Path ("/" <> String.joinWith "/" segs))

parseRelFile :: String -> Maybe (Path Rel File)
parseRelFile s
  | s == "" = Nothing
  | startsWithSlash s = Nothing
  | endsWithSlash s = Nothing
  | otherwise = do
      segs <- segmentsOf s
      if Array.null segs then Nothing
      else Just (Path (String.joinWith "/" segs))

foldSegs :: Array String -> String
foldSegs = Array.foldMap (_ <> "/")

--------------------------------------------------------------------------------
-- Taking paths apart
--------------------------------------------------------------------------------

-- | Peels off the terminal segment and the directory containing it. `Nothing`
-- | for `rootDir` and `currentDir`, which have no terminal segment.
peel :: forall a b. IsDirOrFile b => Path a b -> Maybe (Tuple (Path a Dir) (Name b))
peel p@(Path s) =
  let
    body =
      if dirSep (proxyOfPath p) == "/" then String.take (max 0 (String.length s - 1)) s
      else s
  in
    if body == "" then Nothing
    else case String.lastIndexOf (String.Pattern "/") body of
      Nothing -> Just (Tuple (Path "") (Name body))
      Just idx -> Just (Tuple (Path (String.take (idx + 1) body)) (Name (String.drop (idx + 1) body)))

-- | `peel` for files, which is total: a `File` path always has a terminal
-- | segment.
peelFile :: forall a. Path a File -> Tuple (Path a Dir) (Name File)
peelFile (Path s) =
  case String.lastIndexOf (String.Pattern "/") s of
    Nothing -> Tuple (Path "") (Name s)
    Just idx -> Tuple (Path (String.take (idx + 1) s)) (Name (String.drop (idx + 1) s))

-- | The name of the terminal segment, if there is one.
pathName :: forall a b. IsDirOrFile b => Path a b -> Maybe (Name b)
pathName = peel >>> map snd

-- | The name of a file path, which always exists.
fileName :: forall a. Path a File -> Name File
fileName = snd <<< peelFile

-- | Renames the terminal segment. A path with no terminal segment -- `rootDir`
-- | or `currentDir` -- is returned unchanged.
rename :: forall a b. IsDirOrFile b => (Name b -> Name b) -> Path a b -> Path a b
rename f p = case peel p of
  Nothing -> p
  Just (Tuple parent n) -> extendPath parent (f n)

-- | Sets the extension on the terminal segment.
-- |
-- | ```purescript
-- | file @"image" <.> nm "png"
-- | ```
setExtension :: forall a b. IsDirOrFile b => Path a b -> Name b -> Path a b
setExtension p ext = rename (\n -> joinName (splitName n) { ext = Just ext }) p

infixl 6 setExtension as <.>

--------------------------------------------------------------------------------
-- The boundary
--------------------------------------------------------------------------------

-- | The one bridge from a composed path to something the runtime will take.
-- |
-- | There is deliberately no `Rel` overload and no escape hatch beside it. A
-- | relative name handed to the emulator resolves against a cwd that is
-- | per-node rather than per-process, so its meaning is mutable by code that
-- | has nothing to do with the call -- and both `file:` and the raw `prim_file`
-- | paths move together, so `raw` does not escape it. If you want to give the
-- | filesystem a relative path, supply the base:
-- |
-- | ```purescript
-- | toFilename (base </> rel)
-- | ```
-- |
-- | `Effect` then appears exactly where process state is genuinely consulted,
-- | instead of infecting every path render.
toFilename :: forall b. Path Abs b -> Filename
toFilename (Path s) = Filename (stringToBinary s)
