module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..), fromRight', isRight)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.Binary.IOData (fromBinary)
import Erl.Data.Binary.UTF8 (toBinary)
import Erl.Data.Tuple (tuple4, tuple8)
import Erl.Kernel.Exceptions (ErrorType(..), error, exit, throw, try, tryError, tryExit, tryNamedError, tryThrown)
import Erl.Kernel.File (Filename, PosixError(..), filename, filenameToString, listDir, makeDir, readFile, writeFile)
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

foreign import makeFixtureImpl :: Effect String
foreign import makeRawEntryImpl :: String -> Effect Boolean
foreign import rawNamesUnsupported :: Effect Boolean

makeFixture :: Effect String
makeFixture = makeFixtureImpl

makeRawEntry :: String -> Effect Boolean
makeRawEntry = makeRawEntryImpl

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
