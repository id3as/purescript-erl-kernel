module Erl.Kernel.Inet
  ( ActiveError(..)
  , ActiveSocket
  , AddressFamily(..)
  , CommonOptions
  , ConnectAddress(..)
  , ConnectError(..)
  , ConnectedSocket
  , HostAddress(..)
  , Hostname
  , InterfaceAddressRecord
  , InterfaceAddresses(..)
  , InterfaceFlags(..)
  , InterfaceName
  , InterfaceOptions(..)
  , Ip4Address(..)
  , Ip6Address(..)
  , IpAddress(..)
  , IpAddressUnion
  , ListenSocket
  , LocalAddress
  , PassiveSocket
  , Port(..)
  , PosixError(..)
  , Raw(..)
  , SendError(..)
  , SocketActive(..)
  , SocketAddress(..)
  , SocketMessageBehaviour
  , SocketMode(..)
  , SocketType
  , activeErrorToPurs
  , class OptionToErl
  , class OptionsToErl
  , class OptionsValid
  , class Socket
  , close
  , connectErrorToPurs
  , connectIp4Any
  , connectIp4Loopback
  , connectIp6Any
  , connectIp6Loopback
  , defaultCommonOptions
  , getIfAddresses
  , getIp4IfAddresses
  , ip4
  , ip4Any
  , ip4Loopback
  , ip6
  , ip6Any
  , ip6Loopback
  , ipMulticastAll
  , isMulticast
  , macOsSoReusePort
  , makeTerms
  , ntoa
  , ntoa4
  , ntoa6
  , optionToErl
  , optionsToErl
  , parseIp4Address
  , parseIp6Address
  , parseIpAddress
  , printIpAddress
  , printIpv4
  , printIpv6
  , getHostByName
  , hostAddressToIp
  , posixErrorToPurs
  , recv
  , send
  , sendErrorToPurs
  , windowsSoReuseAddr
  ) where

import Prelude
import Control.Bind (bindFlipped)
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Atom.Symbol (toAtom)
import Erl.Atom.Symbol as AtomSymbol
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List, filterMap, nil, (:))
import Erl.Data.Map (Map, lookup)
import Erl.Data.Tuple (Tuple4, Tuple8, tuple2, tuple4, tuple8, uncurry4, uncurry8)
import Erl.Kernel.File as File
import Data.Int (decimal, hexadecimal, toStringAs)
import Erl.Types (class ToErl, NonNegInt, Timeout, toErl, Octet(..), Hextet(..), hextet, octet)
import Erl.Untagged.Union (class RuntimeType, type (|+|), Nil, RTInt, RTTuple4, RTTuple8, Union)
import Foreign (Foreign, unsafeToForeign)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (Proxy(..))

type Hostname
  = String

newtype Port
  = Port Int

derive instance Eq Port
derive instance Generic Port _
instance Show Port where
  show = genericShow
derive instance Newtype Port _
instance RuntimeType Port RTInt

data SocketType

foreign import data ListenSocket :: SocketType
foreign import data ConnectedSocket :: SocketType

data SocketMessageBehaviour

foreign import data ActiveSocket :: SocketMessageBehaviour
foreign import data PassiveSocket :: SocketMessageBehaviour

class OptionsValid :: SocketMessageBehaviour -> Row Type -> Constraint
class OptionsValid socketMessageBehaviour options

instance OptionsValid ActiveSocket options

instance (Row.Lacks "active" options) => OptionsValid PassiveSocket options

class Socket socket where
  send :: socket -> IOData -> Effect (Either SendError Unit)
  
  recv :: socket -> NonNegInt -> Timeout -> Effect (Either ActiveError Binary)
  
  close :: socket -> Effect Unit

data PosixError
  = EAddrinuse
  | EAddrnotavail
  | EAfnosupport
  | EAlready
  | EConnaborted
  | EConnrefused
  | EConnreset
  | EDestaddrreq
  | EHostdown
  | EHostunreach
  | EInprogress
  | EIsconn
  | EMsgsize
  | ENetdown
  | ENetunreach
  | ENopkg
  | ENoprotoopt
  | ENotconn
  | ENotty
  | ENotsock
  | EProto
  | EProtonosupport
  | EPrototype
  | ESocktnosupport
  | ETimedout
  | EWouldblock
  | EXbadport
  | EXbadseq
  | File File.PosixError

derive instance Eq PosixError

--derive instance Generic PosixError _
instance Show PosixError where
  show _ = "inet posix"

posixErrorToPurs :: Foreign -> Maybe PosixError
posixErrorToPurs f = case posixErrorToPursImpl f of
  Nothing -> File <$> File.posixErrorToPurs f
  posix@(Just _) -> posix

foreign import posixErrorToPursImpl :: Foreign -> Maybe PosixError

newtype Ip4Address
  = Ip4Address (Tuple4 Octet Octet Octet Octet)

derive instance Eq Ip4Address
derive instance Generic Ip4Address _
instance Show Ip4Address where
  show = genericShow
derive instance Newtype Ip4Address _
instance RuntimeType Ip4Address (RTTuple4 RTInt RTInt RTInt RTInt)

newtype Ip6Address
  = Ip6Address (Tuple8 Hextet Hextet Hextet Hextet Hextet Hextet Hextet Hextet)

derive instance Eq Ip6Address
derive instance Generic Ip6Address _
instance Show Ip6Address where
  show = genericShow
derive instance Newtype Ip6Address _
instance RuntimeType Ip6Address (RTTuple8 RTInt RTInt RTInt RTInt RTInt RTInt RTInt RTInt)

data IpAddress
  = Ip4 Ip4Address
  | Ip6 Ip6Address

type IpAddressUnion
  = Union (Ip4Address |+| Ip6Address |+| Nil)

derive instance Eq IpAddress
derive instance Generic IpAddress _
instance Show IpAddress where
  show = genericShow

isMulticast :: IpAddress -> Boolean
isMulticast (Ip4 (Ip4Address addr)) = uncurry4 (\(Octet a) _b _c _d -> a .&. 0xf0 == 0xe0) addr
isMulticast (Ip6 (Ip6Address addr)) = uncurry8 (\(Hextet a) _b _c _d _e _f _g _h -> a .&. 0xff00 == 0xff00) addr

type LocalAddress
  = String

data SocketAddress
  = IpAddress IpAddress
  | Any
  | Loopback
  | LocalAddress LocalAddress

derive instance Eq SocketAddress

type InterfaceName
  = Atom

data InterfaceFlags
  = IfUp
  | IfBroadcast
  | IfLoopback
  | IfPointToPoint
  | IfRunning
  | IfMulticast

type InterfaceAddressRecord ipType
  = { address :: ipType
    , netmask :: ipType
    , broadcastAddress :: Maybe Ip4Address
    , dstAddress :: Maybe Ip4Address
    }

data InterfaceAddresses
  = Ip4InterfaceAddresses (InterfaceAddressRecord Ip4Address)
  | Ip6InterfaceAddresses (InterfaceAddressRecord Ip6Address)

newtype InterfaceOptions
  = InterfaceOptions
  { flags :: List InterfaceFlags
  , addresses :: Maybe InterfaceAddresses
  , hwAddress :: Binary
  }

getIfAddresses :: Effect (Either PosixError (Map InterfaceName (List InterfaceOptions)))
getIfAddresses = getIfAddressesImpl Left Right

foreign import getIfAddressesImpl ::
  (PosixError -> Either PosixError (Map InterfaceName (List InterfaceOptions))) ->
  (Map InterfaceName (List InterfaceOptions) -> Either PosixError (Map InterfaceName (List InterfaceOptions))) ->
  Effect (Either PosixError (Map InterfaceName (List InterfaceOptions)))

getIp4IfAddresses :: InterfaceName -> Effect (List Ip4Address)
getIp4IfAddresses name =
  getIfAddresses <#> hush <#> (bindFlipped (lookup name)) <#> fromMaybe nil <#> filterMap ip4Addr
  where
  ip4Addr (InterfaceOptions { addresses: Just (Ip4InterfaceAddresses { address }) }) = Just $ address
  ip4Addr (InterfaceOptions _) = Nothing

data HostAddress
  = Host Hostname
  | Ip IpAddress

derive instance Eq HostAddress
derive instance Generic HostAddress _

instance Show HostAddress where
  show = genericShow

data ConnectAddress
  = SocketAddr SocketAddress
  | HostAddr HostAddress

instance ToErl ConnectAddress where
  toErl (SocketAddr socketAddress) = toErl socketAddress
  toErl (HostAddr hostAddress) = toErl hostAddress

data ConnectError
  = ConnectTimeout
  | ConnectPosix PosixError

derive instance Eq ConnectError

derive instance Generic ConnectError _

instance Show ConnectError where
  show = genericShow

connectErrorToPurs :: Foreign -> Maybe ConnectError
connectErrorToPurs f = connectErrorToPursImpl ((map ConnectPosix) <<< posixErrorToPurs) f

data AddressFamily
  = Inet4
  | Inet6
  | Local

derive instance Eq AddressFamily

data Raw
  = Raw NonNegInt NonNegInt Binary

derive instance Eq Raw

ipProtoRaw :: Int
ipProtoRaw = 0xffff

ipMulticastAll :: Boolean -> Raw
ipMulticastAll true = Raw 0 49 $ trueSocketOptVal
ipMulticastAll false = Raw 0 49 $ falseSocketOptVal

macOsSoReusePort :: Boolean -> Raw
macOsSoReusePort true = Raw ipProtoRaw 0x200 trueSocketOptVal
macOsSoReusePort false = Raw ipProtoRaw 0x200 falseSocketOptVal

-- Useful on windows since inet_drv.c ignore the reuseaddr option when on windows
windowsSoReuseAddr :: Boolean -> Raw
windowsSoReuseAddr true = Raw ipProtoRaw 0x4 trueSocketOptVal
windowsSoReuseAddr false = Raw ipProtoRaw 0x4 falseSocketOptVal

foreign import trueSocketOptVal :: Binary
foreign import falseSocketOptVal :: Binary

data SocketActive
  = Active
  | Passive
  | Once
  | N Int

derive instance Eq SocketActive

data SocketMode
  = ListData
  | BinaryData

derive instance Eq SocketMode

type CommonOptions r
  = ( active :: Maybe SocketActive
    , buffer :: Maybe NonNegInt
    , dontroute :: Maybe Boolean
    , header :: Maybe NonNegInt
    , high_msgq_watermark :: Maybe NonNegInt
    , low_msgq_watermark :: Maybe NonNegInt
    , priority :: Maybe Int
    , raw :: Maybe (List Raw)
    , recbuf :: Maybe NonNegInt
    , reuseaddr :: Maybe Boolean
    , sndbuf :: Maybe NonNegInt
    , tos :: Maybe Int
    , tclass :: Maybe NonNegInt
    , ttl :: Maybe NonNegInt
    , recvtos :: Maybe Boolean
    , recvtclass :: Maybe Boolean
    , recvttl :: Maybe Boolean
    , ipv6_v6only :: Maybe Boolean
    | r
    )

defaultCommonOptions ::
  forall r.
  Row.Union r (CommonOptions ()) (CommonOptions r) =>
  Record r -> Record (CommonOptions r)
defaultCommonOptions r =
  Record.union r
    { active: Nothing
    , buffer: Nothing
    , dontroute: Nothing
    , header: Nothing
    , high_msgq_watermark: Nothing
    , low_msgq_watermark: Nothing
    , priority: Nothing
    , raw: Nothing
    , recbuf: Nothing
    , reuseaddr: Nothing
    , sndbuf: Nothing
    , tos: Nothing
    , tclass: Nothing
    , ttl: Nothing
    , recvtos: Nothing
    , recvtclass: Nothing
    , recvttl: Nothing
    , ipv6_v6only: Nothing
    }

data SendError
  = SendClosed
  | SendPosix PosixError

derive instance Eq SendError

derive instance Generic SendError _

instance Show SendError where
  show = genericShow

sendErrorToPurs :: Foreign -> Maybe SendError
sendErrorToPurs f = sendErrorToPursImpl ((map SendPosix) <<< posixErrorToPurs) f

data ActiveError
  = ActiveClosed
  | ActiveTimeout
  | ActivePosix PosixError

derive instance Eq ActiveError

derive instance Generic ActiveError _

instance Show ActiveError where
  show = genericShow

activeErrorToPurs :: Foreign -> Maybe ActiveError
activeErrorToPurs f = activeErrorToPursImpl ((map ActivePosix) <<< posixErrorToPurs) f

optionsToErl ::
  forall r rl.
  RL.RowToList r rl =>
  OptionsToErl r rl =>
  Record r -> List Foreign
optionsToErl = makeTerms (Proxy :: _ rl)

instance ToErl Raw where
  toErl (Raw protocol optionNum val) = unsafeToForeign $ tuple4 (atom "raw") protocol optionNum val

instance ToErl HostAddress where
  toErl (Host hostname) = unsafeToForeign $ hostname
  toErl (Ip ipAddress) = toErl ipAddress

instance ToErl AddressFamily where
  toErl Inet4 = unsafeToForeign $ atom "inet4"
  toErl Inet6 = unsafeToForeign $ atom "inet6"
  toErl Local = unsafeToForeign $ atom "local"

instance ToErl SocketMode where
  toErl ListData = unsafeToForeign $ atom "list"
  toErl BinaryData = unsafeToForeign $ atom "binary"

instance ToErl Port where
  toErl (Port port) = toErl port

instance ToErl Ip4Address where
  toErl (Ip4Address ipAddress) = toErl ipAddress

instance ToErl Ip6Address where
  toErl (Ip6Address ipAddress) = toErl ipAddress

instance ToErl IpAddress where
  toErl (Ip4 ipAddress) = toErl ipAddress
  toErl (Ip6 ipAddress) = toErl ipAddress

instance ToErl SocketAddress where
  toErl (IpAddress ipAddress) = toErl ipAddress
  toErl (LocalAddress localAddress) = toErl localAddress
  toErl Any = unsafeToForeign $ atom "any"
  toErl Loopback = unsafeToForeign $ atom "loopback"

instance ToErl SocketActive where
  toErl Active = unsafeToForeign $ atom "true"
  toErl Passive = unsafeToForeign $ atom "false"
  toErl Once = unsafeToForeign $ atom "once"
  toErl (N n) = unsafeToForeign $ n

class OptionToErl :: Symbol -> Type -> Constraint
class OptionToErl sym option where
  optionToErl :: List Foreign -> AtomSymbol.Atom sym -> option -> List Foreign

instance OptionToErl "raw" (List Raw) where
  optionToErl acc _name raws = foldl (\acc' raw -> toErl raw : acc') acc raws
else instance (IsSymbol name, ToErl a) => OptionToErl name (List a) where
  optionToErl acc name val = (unsafeToForeign $ tuple2 (toAtom name) (toErl <$> val)) : acc
else instance (IsSymbol name, ToErl a) => OptionToErl name a where
  optionToErl acc name val = (unsafeToForeign $ tuple2 (toAtom name) (toErl val)) : acc

class OptionsToErl :: Row Type -> RL.RowList Type -> Constraint
class OptionsToErl r rl where
  makeTerms :: Proxy rl -> Record r -> List Foreign

instance OptionsToErl r RL.Nil where
  makeTerms _ _r = nil

instance
  ( IsSymbol sym
  , Row.Cons sym (Maybe a) t1 r
  , OptionsToErl r tail
  , OptionToErl sym a
  ) =>
  OptionsToErl r (RL.Cons sym (Maybe a) tail) where
  makeTerms _ r = do
    let
      tail = makeTerms (Proxy :: _ tail) r
    maybe tail (optionToErl tail (AtomSymbol.atom :: AtomSymbol.Atom sym)) $ Record.get (Proxy :: _ sym) r
else instance
  ( IsSymbol sym
  , Row.Cons sym a t1 r
  , OptionsToErl r tail
  , OptionToErl sym a
  ) =>
  OptionsToErl r (RL.Cons sym a tail) where
  makeTerms _ r = do
    let
      tail = makeTerms (Proxy :: _ tail) r
    optionToErl tail (AtomSymbol.atom :: AtomSymbol.Atom sym) $ Record.get (Proxy :: _ sym) r

------------------------------------------------------------------------------
-- FFI
foreign import sendErrorToPursImpl :: (Foreign -> Maybe SendError) -> Foreign -> Maybe SendError

foreign import activeErrorToPursImpl :: (Foreign -> Maybe ActiveError) -> Foreign -> Maybe ActiveError

foreign import connectErrorToPursImpl :: (Foreign -> Maybe ConnectError) -> Foreign -> Maybe ConnectError

foreign import getHostByName :: Hostname -> Effect (Either Foreign IpAddress)

hostAddressToIp :: HostAddress -> Effect (Either Foreign IpAddress)
hostAddressToIp (Host name) = getHostByName name
hostAddressToIp (Ip ip) = pure $ pure ip

printIpv4 :: Ip4Address -> String
printIpv4 (Ip4Address tuple4) =
  uncurry4
    ( \(Octet o1) (Octet o2) (Octet o3) (Octet o4) ->
        (toStringAs decimal o1)
          <> "."
          <> (toStringAs decimal o2)
          <> "."
          <> (toStringAs decimal o3)
          <> "."
          <> (toStringAs decimal o4)
    )
    tuple4

printIpv6 :: Ip6Address -> String
printIpv6 (Ip6Address tuple8) =
  uncurry8
    ( \(Hextet h1) (Hextet h2) (Hextet h3) (Hextet h4) (Hextet h5) (Hextet h6) (Hextet h7) (Hextet h8) ->
        (toStringAs hexadecimal h1)
          <> ":"
          <> (toStringAs hexadecimal h2)
          <> ":"
          <> (toStringAs hexadecimal h3)
          <> ":"
          <> (toStringAs hexadecimal h4)
          <> ":"
          <> (toStringAs hexadecimal h5)
          <> ":"
          <> (toStringAs hexadecimal h6)
          <> ":"
          <> (toStringAs hexadecimal h7)
          <> ":"
          <> (toStringAs hexadecimal h8)
    )
    tuple8

printIpAddress :: IpAddress -> String
printIpAddress (Ip4 v4) =
  printIpv4 v4

printIpAddress (Ip6 v6) =
  printIpv6 v6
------------------------------------------------------------------------------
-- IP Address helpers
------------------------------------------------------------------------------
-- FFI
foreign import parseIpAddress :: String -> Maybe IpAddress
foreign import parseIp4Address :: String -> Maybe Ip4Address
foreign import parseIp6Address :: String -> Maybe Ip6Address
foreign import ntoa4 :: Ip4Address -> String
foreign import ntoa6 :: Ip6Address -> String
------------------------------------------------------------------------------
ntoa :: IpAddress -> String
ntoa (Ip4 addr) = ntoa4 addr
ntoa (Ip6 addr) = ntoa6 addr

ip4 :: Int -> Int -> Int -> Int -> Maybe Ip4Address
ip4 i1 i2 i3 i4 = Ip4Address <$> (tuple4 <$> (octet i1) <*> (octet i2) <*> (octet i3) <*> (octet i4))

ip6 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Maybe Ip6Address
ip6 i1 i2 i3 i4 i5 i6 i7 i8 = Ip6Address <$> (tuple8 <$> (hextet i1) <*> (hextet i2) <*> (hextet i3) <*> (hextet i4) <*> (hextet i5) <*> (hextet i6) <*> (hextet i7) <*> (hextet i8))

ip4Loopback :: Ip4Address
ip4Loopback = Ip4Address (tuple4 (Octet 127) (Octet 0) (Octet 0) (Octet 1))

ip4Any :: Ip4Address
ip4Any = Ip4Address (tuple4 (Octet 0) (Octet 0) (Octet 0) (Octet 0))

ip6Loopback :: Ip6Address
ip6Loopback = Ip6Address (tuple8 (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 1))

ip6Any :: Ip6Address
ip6Any = Ip6Address (tuple8 (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0) (Hextet 0))

connectIp4Loopback :: ConnectAddress
connectIp4Loopback = SocketAddr $ IpAddress $ Ip4 ip4Loopback

connectIp4Any :: ConnectAddress
connectIp4Any = SocketAddr $ IpAddress $ Ip4 ip4Any

connectIp6Loopback :: ConnectAddress
connectIp6Loopback = SocketAddr $ IpAddress $ Ip6 ip6Loopback

connectIp6Any :: ConnectAddress
connectIp6Any = SocketAddr $ IpAddress $ Ip6 ip6Any
