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
import Erl.Kernel.File (listDir)
import Erl.Kernel.Inet (ActiveError(..), HostAddress(..), Ip4Address(..), Ip6Address(..), IpAddress(..), Port(..), SocketActive(..), connectIp4Loopback, ip4, ip4Any, ip4Loopback, ip6, ip6Any, ip6Loopback, ntoa, ntoa4, ntoa6, parseIp4Address, parseIp6Address, parseIpAddress)
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
import Pathy (dir, rootDir, sandbox, (</>))
import Test.Assert (assert', assertEqual, assertTrue)
import Type.Prelude (Proxy(..))
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
  suite "file tests" do
    test "can list tmp" $ liftEffect do
      res <- listDir $ Left $ unsafeFromJust "impossible" $ sandbox rootDir $ rootDir </> dir (Proxy :: _ "tmp")
      assertTrue $ isRight res

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
