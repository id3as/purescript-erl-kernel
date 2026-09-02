module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..), fromRight', hush, isRight)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Data.Traversable (traverse)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.Binary.IOData (fromBinary)
import Erl.Data.Binary.UTF8 (toBinary)
import Erl.Data.Tuple (tuple4, tuple8)
import Erl.Kernel.Exceptions (ErrorType(..), error, exit, throw, try, tryError, tryExit, tryNamedError, tryThrown)
import Erl.Kernel.File (FileAccess(..), FileError(..), FileType(..), Filename, PosixError(..), filename, filenameToString, listDir, makeDir, readFile, readFileInfo, readLinkInfo, writeFile)
import Erl.Kernel.Filename (Abs, Dir, File, Name, Path, Rel, currentDir, dir, extension, file, file', joinName, name, nameToString, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, peel, peelFile, printPath, rename, rootDir, splitName, toFilename, (</>), (<.>))
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Kernel.Inet (ActiveError(..), ConnectAddress(..), ConnectError(..), HostAddress(..), Ip4Address(..), Ip6Address(..), IpAddress(..), Port(..), SocketActive(..), connectIp4Loopback, ip4, ip4Any, ip4Loopback, ip6, ip6Any, ip6Loopback, ntoa, ntoa4, ntoa6, parseIp4Address, parseIp6Address, parseIpAddress)
import Erl.Kernel.Tcp (TcpMessage(..), setopts)
import Erl.Kernel.Tcp as Tcp
import Erl.Kernel.Udp (UdpMessage(..))
import Erl.Kernel.Udp as Udp
import Erl.Process (Process, ProcessM, receive, self, spawnLink, unsafeRunProcessM, (!))
import Erl.Test.EUnit (TestF, runTests, suite, test)
import Erl.Types (Hextet(..), Octet(..), Timeout(..))
import Erl.Untagged.Union (class RuntimeType, type (|$|), type (|+|), Nil, RTLiteralAtom, RTOption, RTTuple1, Union, inj, prj)
import Foreign (unsafeToForeign)
import Partial.Unsafe (unsafeCrashWith)
import Erl.Data.Binary.IOData (fromBinary) as IOData
import Test.Assert (assert', assertEqual, assertTrue)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main =
  void
    $ runTests do
        tcpTests
        udpTests
        ipTests
        exceptionTests
        fileTests
        pathTests

data Msg = Ready

derive instance eqMsg :: Eq Msg
derive instance genericMsg :: Generic Msg _
instance showMsg :: Show Msg where
  show = genericShow

instance runtimeTypeMsg :: RuntimeType Msg (RTOption (RTTuple1 (RTLiteralAtom "ready")) (RTTuple1 (RTLiteralAtom "accepted")))

type ClientUnion = Union |$| Msg |+| TcpMessage |+| Nil

type ServerUnion = Union |$| TcpMessage |+| Nil

fileTests :: Free TestF Unit
fileTests = do
  suite "filename tests" do
    test "filename rejects the two names POSIX does not allow" $ liftEffect do
      assertEqual { expected: true, actual: isNothing $ filename "" }
      assertEqual { expected: true, actual: isNothing $ filename "with\x0000nul" }

    test "filename accepts a separator, because a Filename is a whole path" $ liftEffect do
      assertEqual { expected: Just "/tmp/a/b", actual: filenameToString =<< filename "/tmp/a/b" }

  suite "file tests" do
    test "can list tmp" $ liftEffect do
      res <- listDir $ fn "/tmp"
      assertTrue $ isRight res

    test "PosixError distinguishes its constructors" $ liftEffect do
      assertEqual { expected: "ENoent", actual: show ENoent }
      assertEqual { expected: "EAcces", actual: show EAcces }
      assertTrue $ show ENoent /= show EAcces

    -- listDir used to decide dir-vs-file with filelib:is_dir/1 on the bare entry
    -- name, which resolves against the process cwd rather than the directory
    -- being listed, and appended "/" to whatever that mistook for a directory.
    -- The names are now returned as they are on disk.
    test "listDir returns entry names verbatim" $ liftEffect do
      fixture <- makeFixture
      entries <- listFixture fixture
      assertEqual
        { expected: List.fromFoldable [ Just "afile.txt", Just "subdir" ]
        , actual: List.sort $ filenameToString <$> entries
        }

    -- Linux only. APFS refuses to create the fixture at all (eilseq), so on a
    -- macOS dev box this asserts nothing -- which is to say the bug reproduces
    -- in production and not on the machine you would debug it from. The
    -- underlying difference is file:list_dir/1, which silently drops undecodable
    -- names, versus file:list_dir_all/1, which returns them as raw binaries.
    test "listDir returns entries whose names are not valid UTF-8" $ liftEffect do
      fixture <- makeFixture
      created <- makeRawEntry fixture
      if created then do
        entries <- listFixture fixture
        assertEqual { expected: 3, actual: List.length entries }
        assertEqual
          { expected: 1
          , actual: List.length $ List.filter (isNothing <<< filenameToString) entries
          }
      else
        -- Not a silent pass: say why there was nothing to check.
        assert' "fixture creation failed for a reason other than a raw-hostile filesystem"
          =<< rawNamesUnsupported

    test "makeDir then write, read and list it back" $ liftEffect do
      fixture <- makeFixture
      let nested = fn $ fixture <> "nested"
      made <- makeDir nested
      assertTrue $ isRight made
      let target = fn $ fixture <> "nested/hello.txt"
      wrote <- writeFile target $ IOData.fromBinary $ toBinary "hello"
      assertTrue $ isRight wrote
      readBack <- readFile target
      assertEqual
        { expected: toBinary "hello"
        , actual: unsafeFromRight "readFile must succeed" readBack
        }
      entries <- listFixture $ fixture <> "nested/"
      assertEqual
        { expected: List.singleton (Just "hello.txt")
        , actual: filenameToString <$> entries
        }

  suite "file info tests" do
    test "reports size, access and type, and tells a directory from a file" $ liftEffect do
      fixture <- makeFixture
      wrote <- writeFile (fn $ fixture <> "afile.txt") $ IOData.fromBinary $ toBinary "hello"
      assertTrue $ isRight wrote
      fileInfo <- unsafeFromRight "readFileInfo must succeed" <$> readFileInfo (fn $ fixture <> "afile.txt")
      assertEqual { expected: Regular, actual: fileInfo.fileType }
      assertEqual { expected: 5, actual: fileInfo.size }
      assertEqual { expected: AccessReadWrite, actual: fileInfo.access }
      dirInfo <- unsafeFromRight "readFileInfo must succeed" <$> readFileInfo (fn fixture)
      assertEqual { expected: Directory, actual: dirInfo.fileType }

    -- The reason it is here at all: listDir no longer classifies entries, so the
    -- answer has to come from asking about the joined path. Asking about the
    -- bare entry name asks about the process cwd instead, which is precisely the
    -- bug the old classifying listDir shipped.
    test "classifies a listDir entry once it is joined onto the directory" $ liftEffect do
      fixture <- makeFixture
      entries <- List.sort <$> listFixture fixture
      types <- traverse (map (map _.fileType <<< hush) <<< readFileInfo <<< joinOnto fixture) entries
      assertEqual
        { expected: List.fromFoldable [ Just Regular, Just Directory ]
        , actual: types
        }

    test "a missing name is ENoent rather than a crash" $ liftEffect do
      fixture <- makeFixture
      res <- readFileInfo $ fn $ fixture <> "no-such-thing"
      assertEqual { expected: Just ENoent, actual: posixOf res }

    -- readFileInfo follows the link and readLinkInfo does not, so Symlink is
    -- only ever reachable through the latter -- and a link to nothing is ENoent
    -- through the former, which is how a caller tells "absent" from "dangling".
    test "readLinkInfo sees the symlink that readFileInfo follows through" $ liftEffect do
      fixture <- makeFixture
      created <- makeSymlinks fixture
      when created do
        throughLink <- readFileInfo $ fn $ fixture <> "alink"
        assertEqual { expected: Just Regular, actual: _.fileType <$> hush throughLink }
        atLink <- readLinkInfo $ fn $ fixture <> "alink"
        assertEqual { expected: Just Symlink, actual: _.fileType <$> hush atLink }
        broken <- readFileInfo $ fn $ fixture <> "broken"
        assertEqual { expected: Just ENoent, actual: posixOf broken }
        brokenLink <- readLinkInfo $ fn $ fixture <> "broken"
        assertEqual { expected: Just Symlink, actual: _.fileType <$> hush brokenLink }

foreign import makeFixtureImpl :: Effect String
foreign import makeSymlinksImpl :: String -> Effect Boolean
foreign import makeRawEntryImpl :: String -> Effect Boolean
foreign import rawNamesUnsupported :: Effect Boolean

makeFixture :: Effect String
makeFixture = makeFixtureImpl

makeRawEntry :: String -> Effect Boolean
makeRawEntry = makeRawEntryImpl

-- | Reports whether the filesystem allowed the links, so a platform without
-- | them skips visibly instead of failing.
makeSymlinks :: String -> Effect Boolean
makeSymlinks = makeSymlinksImpl

-- | FileError has no Eq -- its Other carries a Foreign -- so assert on the
-- | POSIX errno, which is the part being claimed.
posixOf :: forall a. Either FileError a -> Maybe PosixError
posixOf (Left (Posix e)) = Just e
posixOf _ = Nothing

-- | A directory listing entry is a bare name; it means nothing until it is put
-- | back onto the directory it came from.
joinOnto :: String -> Filename -> Filename
joinOnto directory entry =
  fn $ directory <> fromMaybe' (\_ -> unsafeCrashWith "fixture names are utf8") (filenameToString entry)

listFixture :: String -> Effect (List Filename)
listFixture fixture =
  unsafeFromRight "listDir must succeed" <$> listDir (fn fixture)

fn :: String -> Filename
fn = unsafeFromJust "must be a valid filename" <<< filename

tcpTests :: Free TestF Unit
tcpTests = do
  suite "tcp tests" do
    test "active listen-connect-accept-message-close test" do
      unsafeRunProcessM
        $ do
            self <- self
            _server <- liftEffect $ spawnLink $ server self
            ready <- receive
            liftEffect $ assertEqual { actual: prj ready, expected: Just Ready }
            client <- unsafeFromRight "connect failed" <$> Tcp.connect connectIp4Loopback (Port 8080) {} (Timeout $ Milliseconds 1000.0)
            _ <- liftEffect $ Tcp.send client $ fromBinary $ toBinary "hello"
            msg <- receive
            _ <- liftEffect $ assertEqual { expected: Just $ Tcp client (toBinary "world"), actual: prj msg }
            close <- receive
            liftEffect $ assertEqual { expected: Just $ Tcp_closed client, actual: prj close }
    test "passive listen-connect-accept-message-close test" do
      unsafeRunProcessM
        $ do
            self <- self
            _server <- liftEffect $ spawnLink $ server self
            ready <- receive
            liftEffect $ assertEqual { actual: prj ready, expected: Just Ready }
            client <- unsafeFromRight "connect failed" <$> Tcp.connect connectIp4Loopback (Port 8080) { active: Passive } (Timeout $ Milliseconds 1000.0)
            liftEffect
              $ do
                  _ <- Tcp.send client $ fromBinary $ toBinary "hello"
                  msg <- unsafeFromRight "recv failed" <$> Tcp.recv client 5 InfiniteTimeout
                  _ <- assertTrue $ msg == toBinary "world"
                  closed <- Tcp.recv client 0 InfiniteTimeout
                  assertTrue $ closed == Left ActiveClosed
    test "passive listen-connect-accept-message-close test via setopts" do
      unsafeRunProcessM
        $ do
            self <- self
            _server <- liftEffect $ spawnLink $ server self
            ready <- receive
            liftEffect $ assertEqual { actual: prj ready, expected: Just Ready }
            client <- unsafeFromRight "connect failed" <$> Tcp.connect connectIp4Loopback (Port 8080) {} (Timeout $ Milliseconds 1000.0)
            liftEffect
              $ do
                  _ <- unsafeFromRight "setopts failed" <$> Tcp.setopts client { active: Passive }
                  _ <- Tcp.send client $ fromBinary $ toBinary "hello"
                  msg <- unsafeFromRight "recv failed" <$> Tcp.recv client 5 InfiniteTimeout
                  _ <- assertTrue $ msg == toBinary "world"
                  closed <- Tcp.recv client 0 InfiniteTimeout
                  assertTrue $ closed == Left ActiveClosed
    test "can do partial receives" do
      unsafeRunProcessM
        $ do
            self <- self
            _server <- liftEffect $ spawnLink $ server self
            ready <- receive
            liftEffect $ assertEqual { actual: prj ready, expected: Just Ready }
            client <- unsafeFromRight "connect failed" <$> Tcp.connect connectIp4Loopback (Port 8080) { active: Passive } (Timeout $ Milliseconds 1000.0)
            _ <- liftEffect $ setopts client { active: Passive } -- this is a noop since it's already an active socket, but it is proving that the compiler allows us to change the option
            liftEffect
              $ do
                  _ <- Tcp.send client $ fromBinary $ toBinary "hello"
                  msg1 <- unsafeFromRight "recv failed" <$> Tcp.recv client 3 InfiniteTimeout
                  _ <- assertTrue $ msg1 == toBinary "wor"
                  msg2 <- unsafeFromRight "recv failed" <$> Tcp.recv client 2 InfiniteTimeout
                  _ <- assertTrue $ msg2 == toBinary "ld"
                  closed <- Tcp.recv client 0 InfiniteTimeout
                  assertTrue $ closed == Left ActiveClosed
    test "can create passive sockets" do
      unsafeRunProcessM
        $ do
            self <- self
            _server <- liftEffect $ spawnLink $ server self
            ready <- receive
            liftEffect $ assertEqual { actual: prj ready, expected: Just Ready }
            client <- liftEffect $ unsafeFromRight "connect failed" <$> Tcp.connectPassive connectIp4Loopback (Port 8080) {} (Timeout $ Milliseconds 1000.0)
            _ <- liftEffect $ setopts client { reuseaddr: true } -- this is pointless since the socket is connected, but it is proving that the compiler allows us to change some options
            --_ <- liftEffect $ setopts client { active: Active } -- this is not valid, the compiler enforces that you cannot set 'active' on a connectPassive socket
            liftEffect
              $ do
                  _ <- Tcp.send client $ fromBinary $ toBinary "hello"
                  msg1 <- unsafeFromRight "recv failed" <$> Tcp.recv client 3 InfiniteTimeout
                  _ <- assertTrue $ msg1 == toBinary "wor"
                  msg2 <- unsafeFromRight "recv failed" <$> Tcp.recv client 2 InfiniteTimeout
                  _ <- assertTrue $ msg2 == toBinary "ld"
                  closed <- Tcp.recv client 0 InfiniteTimeout
                  assertTrue $ closed == Left ActiveClosed
    -- Regression: a connect failure atom outside the POSIX table (`.invalid` is
    -- reserved by RFC 6761, so it always resolves to `nxdomain`) used to unwind
    -- `connectImpl` via `unsafeCrashWith "invalidError"`, killing the caller.
    -- Now it must surface as `Left (ConnectOther _)` so the caller can retry.
    test "connect to an unresolvable host yields Left, not a crash" do
      result <- liftEffect $ Tcp.connectPassive (HostAddr (Host "no-such-host.invalid")) (Port 80) {} (Timeout $ Milliseconds 5000.0)
      liftEffect $ case result of
        Left (ConnectOther _) -> pure unit
        Left ConnectTimeout -> assert' "resolved to ConnectTimeout, expected ConnectOther nxdomain" false
        Left (ConnectPosix _) -> assert' "resolved to ConnectPosix, expected ConnectOther nxdomain" false
        Right _ -> assert' "unexpectedly connected to an invalid host" false

  where
  server :: Process ClientUnion -> ProcessM ServerUnion Unit
  server parent = do
    listenSocket <- liftEffect $ unsafeFromRight "listen failed" <$> Tcp.listen (Port 8080) { reuseaddr: true }
    _ <- liftEffect $ parent ! inj Ready
    clientSocket <- unsafeFromRight "accept failed" <$> Tcp.accept listenSocket InfiniteTimeout
    _ <- liftEffect $ Tcp.close listenSocket
    msg <- receive
    liftEffect
      $ do
          _ <- assertEqual { expected: Just $ Tcp clientSocket (toBinary "hello"), actual: prj msg }
          _ <- Tcp.send clientSocket $ fromBinary $ toBinary "world"
          _ <- Tcp.close clientSocket
          pure unit

udpTests :: Free TestF Unit
udpTests = do
  suite "udp tests" do
    test "active message test" do
      unsafeRunProcessM
        ( ( do
              socket1 <- unsafeFromRight "open failed" <$> Udp.open (Port 8888) { reuseaddr: true }
              socket2 <- unsafeFromRight "open failed" <$> Udp.open (Port 0) {}
              port2 <- liftEffect $ unsafeFromJust "port failed" <$> Udp.port socket2
              _ <- liftEffect $ Udp.send socket2 (Host "localhost") (Port 8888) (fromBinary (toBinary "hello"))
              message <- receive
              liftEffect
                $ assertEqual
                    { actual: message
                    , expected: inj $ Udp socket1 (inj ip4Loopback) port2 (toBinary "hello")
                    }
          )
            :: ProcessM (Union |$| UdpMessage |+| Nil) Unit
        )
    test "Active socket in passive mode test" do
      unsafeRunProcessM
        ( ( do
              socket1 <- unsafeFromRight "open failed" <$> Udp.open (Port 8888) { reuseaddr: true, active: Passive }
              socket2 <- unsafeFromRight "open failed" <$> Udp.open (Port 0) {}
              liftEffect
                $ do
                    _ <- Udp.send socket2 (Host "localhost") (Port 8888) (fromBinary (toBinary "hello"))
                    recvData <- unsafeFromRight "recv failed" <$> Udp.recv socket1 InfiniteTimeout
                    let
                      payload = case recvData of
                        Udp.Data _ _ p -> Just p
                        Udp.DataAnc _ _ _ _ -> Nothing
                    assertTrue $ payload == (Just $ toBinary "hello")
          )
            :: ProcessM (Union |$| UdpMessage |+| Nil) Unit
        )
    test "passive socket test" do
      socket1 <- unsafeFromRight "open failed" <$> Udp.openPassive (Port 8888) { reuseaddr: true }
      socket2 <- unsafeFromRight "open failed" <$> Udp.openPassive (Port 0) {}
      liftEffect do
        _ <- Udp.send socket2 (Host "localhost") (Port 8888) (fromBinary (toBinary "hello"))
        recvData <- unsafeFromRight "recv failed" <$> Udp.recv socket1 InfiniteTimeout
        let
          payload = case recvData of
            Udp.Data _ _ p -> Just p
            Udp.DataAnc _ _ _ _ -> Nothing
        assertTrue $ payload == (Just $ toBinary "hello")
    test "passive message test via setopts" do
      unsafeRunProcessM
        ( ( do
              socket1 <- unsafeFromRight "open failed" <$> Udp.open (Port 8888) { reuseaddr: true }
              socket2 <- unsafeFromRight "open failed" <$> Udp.open (Port 0) {}
              liftEffect
                $ do
                    _ <- unsafeFromRight "setopts failed" <$> Udp.setopts socket1 { active: Passive }
                    _ <- Udp.send socket2 (Host "localhost") (Port 8888) (fromBinary (toBinary "hello"))
                    recvData <- unsafeFromRight "recv failed" <$> Udp.recv socket1 InfiniteTimeout
                    let
                      payload = case recvData of
                        Udp.Data _ _ p -> Just p
                        Udp.DataAnc _ _ _ _ -> Nothing
                    assertTrue $ payload == (Just $ toBinary "hello")
          )
            :: ProcessM (Union |$| UdpMessage |+| Nil) Unit
        )
  test "show binary short string" do
    let
      bin = toBinary "12345"
      expected = "<<31 32 33 34 35>>"
      actual = show bin
    assertEqual { actual, expected }

ipTests :: Free TestF Unit
ipTests = do
  suite "ip tests" do
    test "Can convert valid IPv4 address" do
      let
        expected = Just $ Ip4 $ Ip4Address ip4Addr
        actual = parseIpAddress validIp4Str
      assertEqual { actual, expected }
    test "Can convert valid IPv4 address II" do
      let
        expected = Just $ Ip4Address ip4Addr
        actual = parseIp4Address validIp4Str
      assertEqual { actual, expected }
    test "Can convert valid IPv6 address" do
      let
        expected = Just $ Ip6 $ Ip6Address ip6Addr
        actual = parseIpAddress validIp6Str
      assertEqual { actual, expected }
    test "Can convert valid IPv6 address II" do
      let
        expected = Just $ Ip6Address ip6Addr
        actual = parseIp6Address validIp6Str
      assertEqual { actual, expected }
    test "Can convert IPv4 address to IPv6 format" do
      let
        expected = Just $ Ip6Address $ tuple8 (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 65535) (Hextet 31709) (Hextet 255)
        actual = parseIp6Address validIp4Str
      assertEqual { actual, expected }
    test "Fails on invalid IPv4 address" do
      let
        ipStr = "123.221.0.256"
        expected = Nothing
        actual = parseIpAddress ipStr
      assertEqual { actual, expected }
    test "Fails on invalid IPv4 address II" do
      let
        ipStr = "123.221.0.256"
        expected = Nothing
        actual = parseIp4Address ipStr
      assertEqual { actual, expected }
    test "Fails on invalid IPv4 address III" do
      let
        expected = Nothing
        actual = parseIp4Address validIp6Str
      assertEqual { actual, expected }
    test "Fails on invalid IPv6 address" do
      let
        ipStr = "z001:db8:3333:4444:5555:6666:7777:8888"
        expected = Nothing
        actual = parseIpAddress ipStr
      assertEqual { actual, expected }
    test "Fails on invalid IPv6 address II" do
      let
        ipStr = "123.221.0.256"
        expected = Nothing
        actual = parseIp6Address ipStr
      assertEqual { actual, expected }
    test "Can build string from valid Ip4 address" do
      let
        expected = validIp4Str
        actual = ntoa $ Ip4 $ Ip4Address ip4Addr
      assertEqual { actual, expected }
    test "Can build string from valid Ip6 address" do
      let
        expected = validIp6Str
        actual = ntoa $ Ip6 $ Ip6Address ip6Addr
      assertEqual { actual, expected }
    test "Can build string from Ip4 tuple" do
      let
        expected = validIp4Str
        actual = ntoa4 $ Ip4Address ip4Addr
      assertEqual { actual, expected }
    test "Can build string from Ip6 tuple" do
      let
        expected = validIp6Str
        actual = ntoa6 $ Ip6Address ip6Addr
      assertEqual { actual, expected }
    test "ip4Loopback helper is correct" do
      let
        expected = parseIp4Address "127.0.0.1"
        actual = Just ip4Loopback
      assertEqual { actual, expected }
    test "ip6Loopback helper is correct" do
      let
        expected = parseIp6Address "::1"
        actual = Just ip6Loopback
      assertEqual { actual, expected }
    test "ip4Any helper is correct" do
      let
        expected = parseIp4Address "0.0.0.0"
        actual = Just ip4Any
      assertEqual { actual, expected }
    test "ip6Any helper is correct" do
      let
        expected = parseIp6Address "::"
        actual = Just ip6Any
      assertEqual { actual, expected }
    test "Can construct ip4 addresses" do
      let
        expected = Just $ Ip4Address ip4Addr
        actual = ip4 123 221 0 255
      assertEqual { actual, expected }
    test "Can construct ip6 addresses" do
      let
        expected = Just $ Ip6Address ip6Addr
        actual = ip6 8193 3512 13107 17476 21845 26214 30583 34952
      assertEqual { actual, expected }

  where
  validIp4Str = "123.221.0.255"
  ip4Addr = tuple4 (Octet 123) (Octet 221) (Octet 0) (Octet 255)
  validIp6Str = "2001:db8:3333:4444:5555:6666:7777:8888"
  ip6Addr = tuple8 (Hextet 8193) (Hextet 3512) (Hextet 13107) (Hextet 17476) (Hextet 21845) (Hextet 26214) (Hextet 30583) (Hextet 34952)

exceptionTests :: Free TestF Unit
exceptionTests = do
  suite "exception tests" do
    test "try/throw" do
      try (throw testValue) >>=
        case _ of
          Left { class: Throw, reason } | isMyError reason -> pure unit
          _ -> assert' "not my error" false

    test "try/error" do
      try (error testValue) >>=
        case _ of
          Left { class: Error, reason } | isMyError reason -> pure unit
          _ -> assert' "not my error" false

    test "try/exit" do
      try (exit testValue) >>=
        case _ of
          Left { class: Exit, reason } | isMyError reason -> pure unit
          _ -> assert' "not my error" false

    test "tryThrown" do
      tryThrown (throw testValue) >>=
        case _ of
          Left reason | isMyError reason -> pure unit
          _ -> assert' "not my error" false

    test "tryError" do
      tryError (error testValue) >>=
        case _ of
          Left reason | isMyError reason -> pure unit
          _ -> assert' "not my error" false

    test "tryExit" do
      tryExit (exit testValue) >>=
        case _ of
          Left reason | isMyError reason -> pure unit
          _ -> assert' "not my error" false

    test "tryNamedError" do
      tryNamedError (atom "some_error") (error $ unsafeToForeign $ atom "some_error") >>=
        assertTrue <<< isNothing

  where
  testValue = unsafeToForeign "my error"
  isMyError reason = unsafeCoerce reason == "my error"

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust s = fromMaybe' (\_ -> unsafeCrashWith s)

unsafeFromRight :: forall a b. String -> Either a b -> b
unsafeFromRight s = fromRight' (\_ -> unsafeCrashWith s)

pathTests :: Free TestF Unit
pathTests = do
  suite "path: what it refuses" do
    -- The point of the whole exercise. The structural representation this
    -- replaced *resolved* `..` while parsing, so a traversal string arrived as
    -- a perfectly good absolute path and nothing downstream could tell.
    test "`..` is rejected, not resolved" $ liftEffect do
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsFile "/data/../../../etc/passwd" }
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsFile "/../x" }
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsFile "/a/b/../c" }
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsDir "/a/../b/" }
      assertEqual { expected: Nothing, actual: printPath <$> parseRelFile "../x" }
      assertEqual { expected: Nothing, actual: printPath <$> parseRelDir "a/../../b/" }

    test "a NUL anywhere in a path is rejected" $ liftEffect do
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsFile ("/a/b" <> nul <> "c") }

    test "a name is one segment, and not a navigational one" $ liftEffect do
      assertEqual { expected: Nothing, actual: nameToString <$> (name "" :: Maybe (Name File)) }
      assertEqual { expected: Nothing, actual: nameToString <$> (name "." :: Maybe (Name File)) }
      assertEqual { expected: Nothing, actual: nameToString <$> (name ".." :: Maybe (Name File)) }
      assertEqual { expected: Nothing, actual: nameToString <$> (name "a/b" :: Maybe (Name File)) }
      assertEqual { expected: Nothing, actual: nameToString <$> (name ("a" <> nul <> "b") :: Maybe (Name File)) }
      assertEqual { expected: Just "a.b", actual: nameToString <$> (name "a.b" :: Maybe (Name File)) }

  suite "path: printing" do
    -- Byte-compatibility with the structural printer is a live constraint, not
    -- a nicety: absolute paths are printed into the generated engine config and
    -- into outbound HTTP request paths, and a running deployment compares them.
    test "an absolute path prints exactly as the structural printer did" $ liftEffect do
      assertEqual { expected: "/", actual: printPath rootDir }
      assertEqual { expected: "/a/", actual: printPath (rootDir </> dir (Proxy :: _ "a")) }
      assertEqual { expected: "/a/b/", actual: printPath (rootDir </> dir (Proxy :: _ "a") </> dir (Proxy :: _ "b")) }
      assertEqual { expected: "/a/b", actual: printPath (rootDir </> dir (Proxy :: _ "a") </> file (Proxy :: _ "b")) }
      assertEqual { expected: "/b", actual: printPath (rootDir </> file (Proxy :: _ "b")) }

    test "a directory keeps its trailing separator, because ensure_dir reads it" $ liftEffect do
      assertEqual { expected: Just "/a/b/", actual: printPath <$> parseAbsDir "/a/b" }
      assertEqual { expected: Just "/a/b/", actual: printPath <$> parseAbsDir "/a/b/" }

    test "a relative path prints bare -- no `./`, and nothing absolutized" $ liftEffect do
      assertEqual { expected: "", actual: printPath currentDir }
      assertEqual { expected: "a/", actual: printPath (dir (Proxy :: _ "a")) }
      assertEqual { expected: "a", actual: printPath (file (Proxy :: _ "a")) }

  suite "path: composing" do
    test "appending is containment: the result is under the base, by construction" $ liftEffect do
      assertEqual { expected: "/srv/app/cache/x.log", actual: printPath (absDir "/srv/app/" </> dir (Proxy :: _ "cache") </> file (Proxy :: _ "x.log")) }

    test "currentDir is the identity for appending" $ liftEffect do
      assertEqual { expected: "a/b", actual: printPath (currentDir </> relFile "a/b") }

    test "no double separator, whatever the base" $ liftEffect do
      assertEqual { expected: "/a", actual: printPath (rootDir </> relFile "a") }
      assertEqual { expected: "/a/b/", actual: printPath (absDir "/a" </> relDir "b") }

  suite "path: parsing" do
    -- A trailing slash positively asserts Dir; its absence asserts nothing.
    -- The structural parser read a missing separator as "this names a file",
    -- so `parseAbsDir "/a/b"` used to fail -- which is to say it rejected most
    -- directory strings anyone would actually write in a config file.
    test "a trailing slash asserts Dir; its absence asserts nothing" $ liftEffect do
      assertEqual { expected: Just "/a/b/", actual: printPath <$> parseAbsDir "/a/b" }
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsFile "/a/b/" }
      assertEqual { expected: Just "/a/b", actual: printPath <$> parseAbsFile "/a/b" }

    test "empty and `.` segments collapse, as they did before" $ liftEffect do
      assertEqual { expected: Just "/foo/bar/", actual: printPath <$> parseAbsDir "/foo/././//bar/" }
      assertEqual { expected: Just "/foo/bar", actual: printPath <$> parseAbsFile "//foo///bar" }

    test "abs and rel are decided by the leading slash alone" $ liftEffect do
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsFile "a/b" }
      assertEqual { expected: Nothing, actual: printPath <$> parseRelFile "/a/b" }
      assertEqual { expected: Just "/", actual: printPath <$> parseAbsDir "/" }
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsFile "/" }

    test "the empty string is not a path" $ liftEffect do
      assertEqual { expected: Nothing, actual: printPath <$> parseAbsDir "" }
      assertEqual { expected: Nothing, actual: printPath <$> parseRelDir "" }
      assertEqual { expected: Nothing, actual: printPath <$> parseRelFile "" }

    test "`.` on its own is the current directory" $ liftEffect do
      assertEqual { expected: Just "", actual: printPath <$> parseRelDir "." }
      assertEqual { expected: Nothing, actual: printPath <$> parseRelFile "." }

  suite "path: taking apart" do
    test "peel splits off the terminal segment" $ liftEffect do
      assertEqual { expected: Just { parent: "/a/", entry: "b" }, actual: peeled (absFile "/a/b") }
      assertEqual { expected: Just { parent: "/", entry: "b" }, actual: peeled (absFile "/b") }
      assertEqual { expected: Just { parent: "/a/", entry: "b" }, actual: peeledDir (absDir "/a/b/") }
      assertEqual { expected: Nothing, actual: peeledDir rootDir }
      assertEqual { expected: Nothing, actual: peeledRelDir currentDir }

    test "peelFile is total, and round-trips" $ liftEffect do
      let Tuple parent entry = peelFile (absFile "/a/b/c")
      assertEqual { expected: "/a/b/c", actual: printPath (parent </> file' entry) }

    test "renaming touches only the terminal segment" $ liftEffect do
      assertEqual { expected: "/a/xb", actual: printPath (rename (\n -> nm "x" <> n) (absFile "/a/b")) }
      assertEqual { expected: "/a/xb/", actual: printPath (rename (\n -> nm "x" <> n) (absDir "/a/b/")) }
      assertEqual { expected: "/", actual: printPath (rename (\n -> nm "x" <> n) rootDir) }

  suite "path: names and extensions" do
    test "splitName and joinName round-trip" $ liftEffect do
      assertEqual { expected: "foo.baz", actual: nameToString (joinName (splitName (nm "foo.baz"))) }
      assertEqual { expected: "foo", actual: nameToString (joinName (splitName (nm "foo"))) }
      assertEqual { expected: ".foo", actual: nameToString (joinName (splitName (nm ".foo"))) }
      assertEqual { expected: "foo.", actual: nameToString (joinName (splitName (nm "foo."))) }

    test "an extension is the part after the last dot, when there is one either side" $ liftEffect do
      assertEqual { expected: Just "baz", actual: nameToString <$> extension (nm "foo.baz") }
      assertEqual { expected: Just "baz", actual: nameToString <$> extension (nm "foo.bar.baz") }
      assertEqual { expected: Nothing, actual: nameToString <$> extension (nm ".foo") }
      assertEqual { expected: Nothing, actual: nameToString <$> extension (nm "foo.") }
      assertEqual { expected: Nothing, actual: nameToString <$> extension (nm "foo") }

    test "setting an extension replaces rather than appends" $ liftEffect do
      assertEqual { expected: "/a/image.png", actual: printPath (absFile "/a/image.jpg" <.> nm "png") }
      assertEqual { expected: "/a/image.png", actual: printPath (absFile "/a/image" <.> nm "png") }

    -- The prefix sites in norsk build a name by concatenating one onto
    -- another; the Semigroup is what keeps them total once the constructor
    -- closes. Note what is NOT expressible this way: `nm "."` is not a valid
    -- Name, so a dotted join goes through joinName, which is the point.
    test "appending two names is a name, so prefixing stays total" $ liftEffect do
      assertEqual { expected: "worker-daemon-17", actual: nameToString (nm "worker-daemon-" <> nm "17") }
      assertEqual { expected: Nothing, actual: nameToString <$> (name "." :: Maybe (Name File)) }
      assertEqual { expected: "a.b", actual: nameToString (joinName { name: nm "a", ext: Just (nm "b") }) }

  suite "path: the runtime boundary" do
    test "toFilename hands over exactly what printPath shows" $ liftEffect do
      assertEqual { expected: Just "/a/b", actual: filenameToString (toFilename (absFile "/a/b")) }
      assertEqual { expected: Just "/a/b/", actual: filenameToString (toFilename (absDir "/a/b/")) }

    -- There is no Rel overload for toFilename and this cannot be tested for at
    -- runtime: the whole point is that `toFilename (file (Proxy :: _ "x"))`
    -- does not typecheck. Recorded here so the property is not silently lost.
    test "there is no relative overload -- see the comment" $ liftEffect do
      assertEqual { expected: unit, actual: unit }

-- | A PureScript `\\x` escape is greedy, so `"a\\x0000b"` is one codepoint 0xB
-- | rather than a NUL followed by `b`. Spelling it separately is the only way
-- | to get a NUL next to anything.
nul :: String
nul = "\x0000"

-- | A `Name` from a literal, for tests. The four clauses are checked; a test
-- | that trips one is a broken test.
nm :: forall b. String -> Name b
nm = unsafeFromJust "test names must be valid" <<< name

absDir :: String -> Path Abs Dir
absDir = unsafeFromJust "test abs dir" <<< parseAbsDir

absFile :: String -> Path Abs File
absFile = unsafeFromJust "test abs file" <<< parseAbsFile

relDir :: String -> Path Rel Dir
relDir = unsafeFromJust "test rel dir" <<< parseRelDir

relFile :: String -> Path Rel File
relFile = unsafeFromJust "test rel file" <<< parseRelFile

peeled :: Path Abs File -> Maybe { parent :: String, entry :: String }
peeled p = (\(Tuple parent entry) -> { parent: printPath parent, entry: nameToString entry }) <$> peel p

peeledDir :: Path Abs Dir -> Maybe { parent :: String, entry :: String }
peeledDir p = (\(Tuple parent entry) -> { parent: printPath parent, entry: nameToString entry }) <$> peel p

peeledRelDir :: Path Rel Dir -> Maybe { parent :: String, entry :: String }
peeledRelDir p = (\(Tuple parent entry) -> { parent: printPath parent, entry: nameToString entry }) <$> peel p
