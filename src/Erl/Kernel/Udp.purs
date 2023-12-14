module Erl.Kernel.Udp
  ( Options
  , OpenOptions
  , UdpSocket
  , UdpMessage(..)
  , UdpRecvData(..)
  , UdpAncillary(..)
  , OpenError(..)
  , OptionToMaybe
  , SendError(..)
  , ReceiveError(..)
  , open
  , openPassive
  , send
  , recv
  , close
  , port
  , setopts
  , convertPassiveToActive
  ) where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, Fn4, mkFn3, mkFn4)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Erl.Kernel.Inet (class OptionsValid, AddressFamily, ActiveSocket, PassiveSocket, CommonOptions, HostAddress, IpAddress, IpAddressUnion, Port, PosixError, SocketActive(..), SocketAddress, SocketMessageBehaviour, SocketMode(..), defaultCommonOptions, optionsToErl, posixErrorToPurs)
import Erl.Types (NonNegInt, Timeout, toErl)
import Erl.Untagged.Union (class CanReceiveMessage, class RuntimeType, RTBinary, RTInt, RTLiteralAtom, RTLiteralAtomConvert, RTOption, RTTuple2, RTTuple5, RTTuple6, RTWildcard)
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Pathy (Abs, File, SandboxedPath)
import Prim.Row as Row
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import data UdpSocket :: SocketMessageBehaviour -> Type

instance Eq (UdpSocket ActiveSocket) where
  eq = eqSocketImpl

instance Eq (UdpSocket PassiveSocket) where
  eq = eqSocketImpl

foreign import eqSocketImpl :: forall socketMessageBehaviour. UdpSocket socketMessageBehaviour -> UdpSocket socketMessageBehaviour -> Boolean

instance Show (UdpSocket ActiveSocket) where
  show = showSocketImpl

instance Show (UdpSocket PassiveSocket) where
  show = showSocketImpl

foreign import showSocketImpl :: forall socketMessageBehaviour. UdpSocket socketMessageBehaviour -> String

data UdpAncillary
  = Tos Int
  | Tclass Int
  | Ttl Int

derive instance eq_UdpAncillary :: Eq UdpAncillary
derive instance Generic UdpAncillary _
instance Show UdpAncillary where
  show = genericShow

data UdpMessage
  = Udp (UdpSocket ActiveSocket) IpAddressUnion Port Binary
  | UdpAnc (UdpSocket ActiveSocket) IpAddressUnion Port (List UdpAncillary) Binary
  | Udp_passive (UdpSocket ActiveSocket)

derive instance eq_UdpMessage :: Eq UdpMessage
derive instance Generic UdpMessage _
instance Show UdpMessage where
  show = genericShow

instance runtimeTypeUdpMessage ::
  RuntimeType
    UdpMessage
    ( RTOption (RTTuple5 (RTLiteralAtom "udp") RTWildcard RTWildcard RTInt RTBinary)
        ( RTOption (RTTuple6 (RTLiteralAtomConvert "udp" "udpAnc") RTWildcard RTWildcard RTInt RTWildcard RTBinary)
            (RTTuple2 (RTLiteralAtom "udp_passive") RTWildcard)
        )
    )

data UdpRecvData
  = Data IpAddressUnion Port Binary
  | DataAnc IpAddressUnion Port (List UdpAncillary) Binary

derive instance eq_UdpRecvData :: Eq UdpRecvData
instance show_UdpRecvData :: Show UdpRecvData where
  show (Data ip port' _bin) = "udp-data: " <> show ip <> ", " <> show port' <> " / binary"
  show (DataAnc ip port' _anc _bin) = "udp-anc-data: " <> show ip <> ", " <> show port' <> " / binary"

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

data OpenError
  = OpenSystemLimit
  | OpenPosix PosixError

derive instance eq_OpenError :: Eq OpenError
derive instance generic_OpenError :: Generic OpenError _
instance show_OpenError :: Show OpenError where
  show = genericShow

openErrorToPurs :: Foreign -> Maybe OpenError
openErrorToPurs f = openErrorToPursImpl ((map OpenPosix) <<< posixErrorToPurs) f

data SendError
  = SendNotOwner
  | SendPosix PosixError

derive instance eq_SendError :: Eq SendError
derive instance generic_SendError :: Generic SendError _
instance show_SendError :: Show SendError where
  show = genericShow

sendErrorToPurs :: Foreign -> Maybe SendError
sendErrorToPurs f = sendErrorToPursImpl ((map SendPosix) <<< posixErrorToPurs) f

data ReceiveError
  = ReceiveNotOwner
  | ReceiveTimeout
  | ReceivePosix PosixError

derive instance eq_ReceiveError :: Eq ReceiveError
derive instance generic_ReceiveError :: Generic ReceiveError _
instance show_ReceiveError :: Show ReceiveError where
  show = genericShow

receiveErrorToPurs :: Foreign -> Maybe ReceiveError
receiveErrorToPurs f = receiveErrorToPursImpl ((map ReceivePosix) <<< posixErrorToPurs) f

type Options r = CommonOptions
  ( add_membership :: Maybe (Tuple2 IpAddress IpAddress)
  , broadcast :: Maybe Boolean
  , drop_membership :: Maybe (Tuple2 IpAddress IpAddress)
  , multicast_if :: Maybe IpAddress
  , multicast_loop :: Maybe Boolean
  , multicast_ttl :: Maybe NonNegInt
  , read_packets :: Maybe NonNegInt
  | r
  )

defaultOptions
  :: forall r
   . Row.Union r (Options ()) (Options r)
  => Record r
  -> Record (Options r)
defaultOptions
  r =
  Record.union r
    $ defaultCommonOptions
        { add_membership: Nothing
        , broadcast: Nothing
        , drop_membership: Nothing
        , multicast_if: Nothing
        , multicast_loop: Nothing
        , multicast_ttl: Nothing
        , read_packets: Nothing
        }

type OpenOptions = Options
  ( ip :: Maybe SocketAddress
  , fd :: Maybe NonNegInt -- todo
  , ifaddr :: Maybe SocketAddress
  , family :: Maybe AddressFamily
  , port :: Maybe Port
  , netns :: Maybe (SandboxedPath Abs File)
  , bind_to_device :: Maybe Binary
  )

defaultOpenOptions :: Record OpenOptions
defaultOpenOptions =
  defaultOptions
    { ip: Nothing
    , fd: Nothing
    , ifaddr: Nothing
    , family: Nothing
    , port: Nothing
    , netns: Nothing
    , bind_to_device: Nothing
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

instance ConvertOption OptionToMaybe "mode" a a where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe sym (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe sym a (Maybe a) where
  convertOption _ _ val = Just val

convertPassiveToActive
  :: forall m
   . MonadEffect m
  => CanReceiveMessage UdpMessage m
  => UdpSocket PassiveSocket
  -> UdpSocket ActiveSocket
convertPassiveToActive = unsafeCoerce

open
  :: forall options m
   . MonadEffect m
  => CanReceiveMessage UdpMessage m
  => Row.Union (ForcedOptions ()) options (ForcedOptions options)
  => Row.Nub (ForcedOptions options) (ForcedOptions options)
  => ConvertOptionsWithDefaults OptionToMaybe (Record OpenOptions) (Record (ForcedOptions options)) (Record (ForcedOptions OpenOptions))
  => Port
  -> Record options
  -> m (Either OpenError (UdpSocket ActiveSocket))
open port' options = do
  let
    forced = Record.disjointUnion forcedOptions options
    optionsErl = optionsToErl $ convertOptionsWithDefaults OptionToMaybe defaultOpenOptions forced
  liftEffect $ openImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< openErrorToPurs) Right port' optionsErl

openPassive
  :: forall options
   . Row.Lacks "active" options
  => Row.Union (ForcedOptions ()) options (ForcedOptions options)
  => Row.Nub (ForcedOptions options) (ForcedOptions options)
  => ConvertOptionsWithDefaults OptionToMaybe (Record OpenOptions) (Record (ForcedOptions options)) (Record (ForcedOptions OpenOptions))
  => Port
  -> Record options
  -> Effect (Either OpenError (UdpSocket PassiveSocket))
openPassive port' options = do
  let
    forced = Record.disjointUnion forcedOptions options
    merged = convertOptionsWithDefaults OptionToMaybe defaultOpenOptions forced
    optionsErl = optionsToErl $ merged { active = Just Passive }
  liftEffect $ openImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< openErrorToPurs) Right port' optionsErl

send :: forall socketMessageBehaviour. UdpSocket socketMessageBehaviour -> HostAddress -> Port -> IOData -> Effect (Either SendError Unit)
send socket host port' packet = do
  let
    hostErl = toErl host
  liftEffect $ sendImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< sendErrorToPurs) Right socket hostErl port' packet

recv :: forall socketMessageBehaviour. UdpSocket socketMessageBehaviour -> Timeout -> Effect (Either ReceiveError UdpRecvData)
recv socket timeout = do
  recvImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< receiveErrorToPurs) (mkFn3 (\a b c -> Right $ Data a b c)) (mkFn4 (\a b c d -> Right $ DataAnc a b c d)) socket (toErl timeout)

close :: forall socketMessageBehaviour. UdpSocket socketMessageBehaviour -> Effect Unit
close = closeImpl

port :: forall socketMessageBehaviour. UdpSocket socketMessageBehaviour -> Effect (Maybe Port)
port = portImpl

setopts
  :: forall options socketMessageBehaviour
   . Row.Union (ForcedOptions ()) options (ForcedOptions options)
  => Row.Nub (ForcedOptions options) (ForcedOptions options)
  => ConvertOptionsWithDefaults OptionToMaybe (Record (Options ())) (Record (ForcedOptions options)) (Record (ForcedOptions (Options ())))
  => OptionsValid socketMessageBehaviour options
  => UdpSocket socketMessageBehaviour
  -> Record options
  -> Effect (Either PosixError Unit)
setopts socket options = do
  let
    forced = Record.disjointUnion forcedOptions options
    optionsErl = optionsToErl $ convertOptionsWithDefaults OptionToMaybe (defaultOptions {}) forced
  liftEffect $ setoptsImpl (Left <<< fromMaybe' (\_ -> unsafeCrashWith "invalidError") <<< posixErrorToPurs) Right socket optionsErl

foreign import openImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either OpenError (UdpSocket socketMessageBehaviour))
  -> (UdpSocket socketMessageBehaviour -> Either OpenError (UdpSocket socketMessageBehaviour))
  -> Port
  -> List Foreign
  -> Effect (Either OpenError (UdpSocket socketMessageBehaviour))

foreign import sendImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either SendError Unit)
  -> (Unit -> Either SendError Unit)
  -> UdpSocket socketMessageBehaviour
  -> Foreign
  -> Port
  -> IOData
  -> Effect (Either SendError Unit)

foreign import recvImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either ReceiveError UdpRecvData)
  -> Fn3 IpAddressUnion Port Binary (Either ReceiveError UdpRecvData)
  -> Fn4 IpAddressUnion Port (List UdpAncillary) Binary (Either ReceiveError UdpRecvData)
  -> UdpSocket socketMessageBehaviour
  -> Foreign
  -> Effect (Either ReceiveError UdpRecvData)

foreign import closeImpl :: forall socketMessageBehaviour. UdpSocket socketMessageBehaviour -> Effect Unit

foreign import portImpl :: forall socketMessageBehaviour. UdpSocket socketMessageBehaviour -> Effect (Maybe Port)

foreign import setoptsImpl
  :: forall socketMessageBehaviour
   . (Foreign -> Either PosixError Unit)
  -> (Unit -> Either PosixError Unit)
  -> UdpSocket socketMessageBehaviour
  -> List Foreign
  -> Effect (Either PosixError Unit)

foreign import openErrorToPursImpl :: (Foreign -> Maybe OpenError) -> Foreign -> Maybe OpenError
foreign import sendErrorToPursImpl :: (Foreign -> Maybe SendError) -> Foreign -> Maybe SendError
foreign import receiveErrorToPursImpl :: (Foreign -> Maybe ReceiveError) -> Foreign -> Maybe ReceiveError
