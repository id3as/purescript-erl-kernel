module Erl.Kernel.Tcp
  ( Options
  , ConnectListenOptions
  , ConnectOptions
  , ListenOptions
  , SocketPacket(..)
  , OptionToMaybe(..)
  , TcpSocket
  , TcpMessage(..)
  , Linger(..)
  , ShutdownHow
  , AcceptError(..)
  , ListenError(..)
  , defaultConnectListenOptions
  , defaultConnectOptions
  , defaultListenOptions
  , connectOptions
  , listenOptions
  , accept
  , acceptPassive
  , close
  , connect
  , connectPassive
  , listen
  , listenPassive
  , recv
  , send
  , shutdown
  , setopts
  , convertPassiveToActive
  ) where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List)
import Erl.Data.Tuple (tuple2)
import Erl.Kernel.Inet (class OptionsValid, class Socket, ActiveError, ActiveSocket, AddressFamily, CommonOptions, ConnectAddress, ConnectError, ConnectedSocket, ListenSocket, PassiveSocket, Port, PosixError, SendError, SocketActive(..), SocketAddress, SocketMessageBehaviour, SocketMode(..), SocketType, activeErrorToPurs, connectErrorToPurs, defaultCommonOptions, optionsToErl, posixErrorToPurs, sendErrorToPurs)
import Erl.Types (class ToErl, NonNegInt, Timeout, toErl)
import Erl.Untagged.Union (class CanReceiveMessage, class RuntimeType, RTBinary, RTLiteralAtom, RTOption, RTTuple2, RTTuple3, RTWildcard)
import Foreign (Foreign, unsafeToForeign)
import Partial.Unsafe (unsafeCrashWith)
import Pathy (Abs, File, SandboxedPath)
import Prim.Row as Row
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import data TcpSocket :: SocketMessageBehaviour -> SocketType -> Type

instance Eq (TcpSocket ActiveSocket ConnectedSocket) where
  eq = eqSocketImpl

instance Eq (TcpSocket PassiveSocket ConnectedSocket) where
  eq = eqSocketImpl

instance Eq (TcpSocket ActiveSocket ListenSocket) where
  eq = eqSocketImpl

instance Eq (TcpSocket PassiveSocket ListenSocket) where
  eq = eqSocketImpl

foreign import eqSocketImpl :: forall socketMessageBehaviour socketType. TcpSocket socketMessageBehaviour socketType -> TcpSocket socketMessageBehaviour socketType -> Boolean

instance Show (TcpSocket ActiveSocket ConnectedSocket) where
  show = showSocketImpl

instance Show (TcpSocket PassiveSocket ConnectedSocket) where
  show = showSocketImpl

instance Show (TcpSocket ActiveSocket ListenSocket) where
  show = showSocketImpl

instance Show (TcpSocket PassiveSocket ListenSocket) where
  show = showSocketImpl

foreign import showSocketImpl :: forall socketMessageBehaviour socketType. TcpSocket socketMessageBehaviour socketType -> String

instance Socket (TcpSocket ActiveSocket ConnectedSocket) where
  send = send
  recv = recv
  close = close

instance Socket (TcpSocket PassiveSocket ConnectedSocket) where
  send = send
  recv = recv
  close = close

data AcceptError
  = AcceptClosed
  | AcceptTimeout
  | AcceptSystemLimit
  | AcceptPosix PosixError

derive instance eq_AcceptError :: Eq AcceptError

derive instance generic_AcceptError :: Generic AcceptError _

instance show_AcceptError :: Show AcceptError where
  show = genericShow

acceptErrorToPurs :: Foreign -> Maybe AcceptError
acceptErrorToPurs f = acceptErrorToPursImpl ((map AcceptPosix) <<< posixErrorToPurs) f

data ListenError
  = ListenSystemLimit
  | ListenPosix PosixError

derive instance eq_ListenError :: Eq ListenError

derive instance generic_ListenError :: Generic ListenError _

instance show_ListenError :: Show ListenError where
  show = genericShow

listenErrorToPurs :: Foreign -> Maybe ListenError
listenErrorToPurs f = listenErrorToPursImpl ((map ListenPosix) <<< posixErrorToPurs) f

data SocketPacket
  = Raw
  | Header_1
  | Header_2
  | Header_4
  | Asn1
  | Cdr
  | Sunrm
  | Fcgi
  | Tpkt
  | Line
  | Http
  | Http_bin
  | Httph
  | Httph_bin

derive instance eqSocketPacket :: Eq SocketPacket

data ShutdownHow
  = Read
  | Write
  | ReadWrite
  | I

derive instance eqShutdonwHow :: Eq ShutdownHow

instance toErl_SocketPacket :: ToErl SocketPacket where
  toErl Raw = unsafeToForeign $ atom "raw"
  toErl Header_1 = unsafeToForeign $ 1
  toErl Header_2 = unsafeToForeign $ 2
  toErl Header_4 = unsafeToForeign $ 4
  toErl Asn1 = unsafeToForeign $ atom "asn1"
  toErl Cdr = unsafeToForeign $ atom "cdr"
  toErl Sunrm = unsafeToForeign $ atom "sunrm"
  toErl Fcgi = unsafeToForeign $ atom "fcgi"
  toErl Tpkt = unsafeToForeign $ atom "tpkt"
  toErl Line = unsafeToForeign $ atom "line"
  toErl Http = unsafeToForeign $ atom "http"
  toErl Http_bin = unsafeToForeign $ atom "http_bin"
  toErl Httph = unsafeToForeign $ atom "httph"
  toErl Httph_bin = unsafeToForeign $ atom "httph_bin"

newtype Linger = Linger NonNegInt

derive newtype instance eqLinger :: Eq Linger

instance toErl :: ToErl Linger where
  toErl (Linger linger) = unsafeToForeign $ tuple2 true linger

type Options r = CommonOptions
  ( delay_send :: Maybe Boolean
  , exit_on_close :: Maybe Boolean
  , high_watermark :: Maybe NonNegInt
  , keepalive :: Maybe Boolean
  , linger :: Maybe Linger
  , low_watermark :: Maybe NonNegInt
  , nodelay :: Maybe Boolean
  , packet :: Maybe SocketPacket
  , packet_size :: Maybe NonNegInt
  , send_timeout :: Maybe Milliseconds
  , send_timeout_close :: Maybe Boolean
  , show_econnreset :: Maybe Boolean
  | r
  )

type ConnectListenOptions r = Options
  ( ip :: Maybe SocketAddress
  , fd :: Maybe NonNegInt -- todo
  , ifaddr :: Maybe SocketAddress
  , family :: Maybe AddressFamily
  , port :: Maybe Port
  , netns :: Maybe (SandboxedPath Abs File)
  , bind_to_device :: Maybe Binary
  | r
  )

type ConnectOptions = ConnectListenOptions ()

connectOptions
  :: forall options
   . ConvertOptionsWithDefaults OptionToMaybe (Record ConnectOptions) options (Record ConnectOptions)
  => options
  -> Record ConnectOptions
connectOptions options = convertOptionsWithDefaults OptionToMaybe defaultConnectOptions options

type ListenOptions = ConnectListenOptions
  ( backlog :: Maybe (NonNegInt)
  )

listenOptions
  :: forall options
   . ConvertOptionsWithDefaults OptionToMaybe (Record ListenOptions) options (Record ListenOptions)
  => options
  -> Record ListenOptions
listenOptions options = convertOptionsWithDefaults OptionToMaybe defaultListenOptions options

data TcpMessage
  = Tcp (TcpSocket ActiveSocket ConnectedSocket) Binary
  | Tcp_error (TcpSocket ActiveSocket ConnectedSocket) ActiveError
  | Tcp_closed (TcpSocket ActiveSocket ConnectedSocket)
  | Tcp_passive (TcpSocket ActiveSocket ConnectedSocket)

derive instance eq_TcpMessage :: Eq TcpMessage

instance show_TcpMessage :: Show TcpMessage where
  show (Tcp socket _bin) = "tcp-data: " <> show socket <> " / binary"
  show (Tcp_error socket err) = "tcp-error: " <> show socket <> " / " <> show err
  show (Tcp_closed socket) = "tcp-closed: " <> show socket
  show (Tcp_passive socket) = "tcp-passive: " <> show socket

instance runtimeTypeTcpMessage ::
  RuntimeType
    TcpMessage
    ( RTOption (RTTuple3 (RTLiteralAtom "tcp") RTWildcard RTBinary)
        ( RTOption (RTTuple3 (RTLiteralAtom "tcp_error") RTWildcard RTWildcard)
            ( RTOption (RTTuple2 (RTLiteralAtom "tcp_closed") RTWildcard)
                (RTTuple2 (RTLiteralAtom "tcp_passive") RTWildcard)
            )
        )
    )

defaultOptions
  :: forall r
   . Row.Union r (Options ()) (Options r)
  => Record r
  -> Record (Options r)
defaultOptions r =
  Record.union r
    $ defaultCommonOptions
        { delay_send: Nothing
        , exit_on_close: Nothing
        , high_watermark: Nothing
        , keepalive: Nothing
        , linger: Nothing
        , low_watermark: Nothing
        , nodelay: Nothing
        , packet: Nothing
        , packet_size: Nothing
        , send_timeout: Nothing
        , send_timeout_close: Nothing
        , show_econnreset: Nothing
        }

defaultConnectListenOptions
  :: forall r
   . Row.Union r (ConnectListenOptions ()) (ConnectListenOptions r)
  => Record r
  -> Record (ConnectListenOptions r)
defaultConnectListenOptions r =
  Record.union r
    $ defaultOptions
        { ip: Nothing
        , fd: Nothing
        , ifaddr: Nothing
        , family: Nothing
        , port: Nothing
        , netns: Nothing
        , bind_to_device: Nothing
        }

defaultConnectOptions :: Record ConnectOptions
defaultConnectOptions = defaultConnectListenOptions {}

defaultListenOptions :: Record ListenOptions
defaultListenOptions =
  defaultConnectListenOptions
    { backlog: Nothing
    }

type ForcedOptions r =
  ( mode :: SocketMode
  | r
  )

forcedOptions :: Record (ForcedOptions ())
forcedOptions =
  { mode: BinaryData
  }

data OptionToMaybe = OptionToMaybe

instance convertOption_OptionToMaybeMode :: ConvertOption OptionToMaybe "mode" a a where
  convertOption _ _ val = val
else instance convertOption_OptionToMaybe :: ConvertOption OptionToMaybe sym (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance convertOption_OptionToMaybe2 :: ConvertOption OptionToMaybe sym a (Maybe a) where
  convertOption _ _ val = Just val

convertPassiveToActive
  :: forall m
   . MonadEffect m
  => CanReceiveMessage TcpMessage m
  => TcpSocket PassiveSocket ConnectedSocket
  -> TcpSocket ActiveSocket ConnectedSocket
convertPassiveToActive = unsafeCoerce

accept
  :: forall m
   . MonadEffect m
  => CanReceiveMessage TcpMessage m
  => TcpSocket ActiveSocket ListenSocket
  -> Timeout
  -> m (Either AcceptError (TcpSocket ActiveSocket ConnectedSocket))
accept socket timeout = liftEffect $ acceptImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< acceptErrorToPurs) Right socket (toErl timeout)

acceptPassive
  :: TcpSocket PassiveSocket ListenSocket -> Timeout -> Effect (Either AcceptError (TcpSocket PassiveSocket ConnectedSocket))
acceptPassive socket timeout = liftEffect $ acceptImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< acceptErrorToPurs) Right socket (toErl timeout)

close :: forall socketMessageBehaviour socketType. TcpSocket socketMessageBehaviour socketType -> Effect Unit
close = closeImpl

connect
  :: forall options m
   . MonadEffect m
  => CanReceiveMessage TcpMessage m
  => Row.Union (ForcedOptions ()) options (ForcedOptions options)
  => Row.Nub (ForcedOptions options) (ForcedOptions options)
  => ConvertOptionsWithDefaults OptionToMaybe (Record ConnectOptions) (Record (ForcedOptions options)) (Record (ForcedOptions ConnectOptions))
  => ConnectAddress
  -> Port
  -> Record options
  -> Timeout
  -> m (Either ConnectError (TcpSocket ActiveSocket ConnectedSocket))
connect address port options timeout = do
  let
    addressErl = toErl address
    forced = Record.disjointUnion forcedOptions options
    optionsErl = optionsToErl $ convertOptionsWithDefaults OptionToMaybe defaultConnectOptions forced
  liftEffect $ connectImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< connectErrorToPurs) Right addressErl port optionsErl (toErl timeout)

connectPassive
  :: forall options
   . Row.Lacks "active" options
  => Row.Union (ForcedOptions ()) options (ForcedOptions options)
  => Row.Nub (ForcedOptions options) (ForcedOptions options)
  => ConvertOptionsWithDefaults OptionToMaybe (Record ConnectOptions) (Record (ForcedOptions options)) (Record (ForcedOptions ConnectOptions))
  => ConnectAddress
  -> Port
  -> Record options
  -> Timeout
  -> Effect (Either ConnectError (TcpSocket PassiveSocket ConnectedSocket))
connectPassive address port options timeout = do
  let
    addressErl = toErl address
    forced = Record.disjointUnion forcedOptions options
    merged = convertOptionsWithDefaults OptionToMaybe defaultConnectOptions forced
    optionsErl = optionsToErl merged { active = Just Passive }
  liftEffect $ connectImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< connectErrorToPurs) Right addressErl port optionsErl (toErl timeout)

listen
  :: forall options
   . Row.Union (ForcedOptions ()) options (ForcedOptions options)
  => Row.Nub (ForcedOptions options) (ForcedOptions options)
  => ConvertOptionsWithDefaults OptionToMaybe (Record ListenOptions) (Record (ForcedOptions options)) (Record (ForcedOptions ListenOptions))
  => Port
  -> Record options
  -> Effect (Either ListenError (TcpSocket ActiveSocket ListenSocket))
listen port options = do
  let
    forced = Record.disjointUnion forcedOptions options
    optionsErl = optionsToErl $ convertOptionsWithDefaults OptionToMaybe defaultListenOptions forced
  listenImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< listenErrorToPurs) Right port optionsErl

listenPassive
  :: forall options
   . Row.Lacks "active" options
  => Row.Union (ForcedOptions ()) options (ForcedOptions options)
  => Row.Nub (ForcedOptions options) (ForcedOptions options)
  => ConvertOptionsWithDefaults OptionToMaybe (Record ListenOptions) (Record (ForcedOptions options)) (Record (ForcedOptions ListenOptions))
  => Port
  -> Record options
  -> Effect (Either ListenError (TcpSocket PassiveSocket ListenSocket))
listenPassive port options = do
  let
    forced = Record.disjointUnion forcedOptions options
    merged = convertOptionsWithDefaults OptionToMaybe defaultListenOptions forced
    optionsErl = optionsToErl $ merged { active = Just Passive }
  listenImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< listenErrorToPurs) Right port optionsErl

recv :: forall socketMessageBehaviour. TcpSocket socketMessageBehaviour ConnectedSocket -> NonNegInt -> Timeout -> Effect (Either ActiveError Binary)
recv socket length timeout = recvImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< activeErrorToPurs) Right socket length (toErl timeout)

send :: forall socketMessageBehaviour. TcpSocket socketMessageBehaviour ConnectedSocket -> IOData -> Effect (Either SendError Unit)
send = sendImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< sendErrorToPurs) Right

shutdown :: forall socketMessageBehaviour socketType. TcpSocket socketMessageBehaviour socketType -> ShutdownHow -> Effect (Either PosixError Unit)
shutdown = shutdownImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< posixErrorToPurs) Right

setopts
  :: forall options socketMessageBehaviour socketType
   . Row.Union (ForcedOptions ()) options (ForcedOptions options)
  => Row.Nub (ForcedOptions options) (ForcedOptions options)
  => ConvertOptionsWithDefaults OptionToMaybe (Record (Options ())) (Record (ForcedOptions options)) (Record (ForcedOptions (Options ())))
  => OptionsValid socketMessageBehaviour options
  => TcpSocket socketMessageBehaviour socketType
  -> Record options
  -> Effect (Either PosixError Unit)
setopts socket options = do
  let
    forced = Record.disjointUnion forcedOptions options

    optionsErl = optionsToErl $ convertOptionsWithDefaults OptionToMaybe (defaultOptions {}) forced
  liftEffect $ setoptsImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< posixErrorToPurs) Right socket optionsErl

foreign import acceptImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either AcceptError (TcpSocket socketMessageBehaviour ConnectedSocket))
  -> ((TcpSocket socketMessageBehaviour ConnectedSocket) -> Either AcceptError (TcpSocket socketMessageBehaviour ConnectedSocket))
  -> TcpSocket socketMessageBehaviour ListenSocket
  -> Foreign
  -> Effect (Either AcceptError (TcpSocket socketMessageBehaviour ConnectedSocket))

foreign import closeImpl
  :: forall socketMessageBehaviour socketType
   . TcpSocket socketMessageBehaviour socketType
  -> Effect Unit

foreign import connectImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either ConnectError (TcpSocket socketMessageBehaviour ConnectedSocket))
  -> ((TcpSocket socketMessageBehaviour ConnectedSocket) -> Either ConnectError (TcpSocket socketMessageBehaviour ConnectedSocket))
  -> Foreign
  -> Port
  -> List Foreign
  -> Foreign
  -> Effect (Either ConnectError (TcpSocket socketMessageBehaviour ConnectedSocket))

foreign import listenImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either ListenError (TcpSocket socketMessageBehaviour ListenSocket))
  -> ((TcpSocket socketMessageBehaviour ListenSocket) -> Either ListenError (TcpSocket socketMessageBehaviour ListenSocket))
  -> Port
  -> List Foreign
  -> Effect (Either ListenError (TcpSocket socketMessageBehaviour ListenSocket))

foreign import recvImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either ActiveError Binary)
  -> (Binary -> Either ActiveError Binary)
  -> TcpSocket socketMessageBehaviour ConnectedSocket
  -> NonNegInt
  -> Foreign
  -> Effect (Either ActiveError Binary)

foreign import sendImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either SendError Unit)
  -> (Unit -> Either SendError Unit)
  -> TcpSocket socketMessageBehaviour ConnectedSocket
  -> IOData
  -> Effect (Either SendError Unit)

foreign import shutdownImpl
  :: forall socketMessageBehaviour socketType
   . (Foreign -> Either PosixError Unit)
  -> (Unit -> Either PosixError Unit)
  -> TcpSocket socketMessageBehaviour socketType
  -> ShutdownHow
  -> Effect (Either PosixError Unit)

foreign import setoptsImpl
  :: forall socketMessageBehaviour socketType
   . (Foreign -> Either PosixError Unit)
  -> (Unit -> Either PosixError Unit)
  -> TcpSocket socketMessageBehaviour socketType
  -> List Foreign
  -> Effect (Either PosixError Unit)

foreign import acceptErrorToPursImpl :: (Foreign -> Maybe AcceptError) -> Foreign -> Maybe AcceptError

foreign import listenErrorToPursImpl :: (Foreign -> Maybe ListenError) -> Foreign -> Maybe ListenError
