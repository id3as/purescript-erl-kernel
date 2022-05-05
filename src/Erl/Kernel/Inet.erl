-module(erl_kernel_inet@foreign).

-export([ posixErrorToPursImpl/1
        , sendErrorToPursImpl/2
        , activeErrorToPursImpl/2
        , connectErrorToPursImpl/2
        , parseIpAddress/1
        , parseIp4Address/1
        , parseIp6Address/1
        , ntoa4/1
        , ntoa6/1
        , trueSocketOptVal/0
        , falseSocketOptVal/0
        , getIfAddressesImpl/2
        , getHostByName/1
        ]).

%% FFI Helpers
-define(just(A), {just, A}).
-define(nothing, {nothing}).
-define(ip4(A), {ip4, A}).
-define(ip6(A), {ip6, A}).

-include_lib("kernel/include/inet.hrl").


posixErrorToPursImpl(eaddrinuse) -> ?just({eaddrinuse});
posixErrorToPursImpl(eaddrnotavail) -> ?just({eaddrnotavail});
posixErrorToPursImpl(eafnosupport) -> ?just({eafnosupport});
posixErrorToPursImpl(ealready) -> ?just({ealready});
posixErrorToPursImpl(econnaborted) -> ?just({econnaborted});
posixErrorToPursImpl(econnrefused) -> ?just({econnrefused});
posixErrorToPursImpl(econnreset) -> ?just({econnreset});
posixErrorToPursImpl(edestaddrreq) -> ?just({edestaddrreq});
posixErrorToPursImpl(ehostdown) -> ?just({ehostdown});
posixErrorToPursImpl(ehostunreach) -> ?just({ehostunreach});
posixErrorToPursImpl(einprogress) -> ?just({einprogress});
posixErrorToPursImpl(eisconn) -> ?just({eisconn});
posixErrorToPursImpl(emsgsize) -> ?just({emsgsize});
posixErrorToPursImpl(enetdown) -> ?just({enetdown});
posixErrorToPursImpl(enetunreach) -> ?just({enetunreach});
posixErrorToPursImpl(enopkg) -> ?just({enopkg});
posixErrorToPursImpl(enoprotoopt) -> ?just({enoprotoopt});
posixErrorToPursImpl(enotconn) -> ?just({enotconn});
posixErrorToPursImpl(enotty) -> ?just({enotty});
posixErrorToPursImpl(enotsock) -> ?just({enotsock});
posixErrorToPursImpl(eproto) -> ?just({eproto});
posixErrorToPursImpl(eprotonosupport) -> ?just({eprotonosupport});
posixErrorToPursImpl(eprototype) -> ?just({eprototype});
posixErrorToPursImpl(esocktnosupport) -> ?just({esocktnosupport});
posixErrorToPursImpl(etimedout) -> ?just({etimedout});
posixErrorToPursImpl(ewouldblock) -> ?just({ewouldblock});
posixErrorToPursImpl(exbadport) -> ?just({exbadport});
posixErrorToPursImpl(exbadseq) -> ?just({exbadseq});
posixErrorToPursImpl(_) -> ?nothing.

sendErrorToPursImpl(_PosixErr, closed) -> ?just({sendClosed});
sendErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

activeErrorToPursImpl(_PosixErr, closed) -> ?just({activeClosed});
activeErrorToPursImpl(_PosixErr, timeout) -> ?just({activeTimeout});
activeErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

connectErrorToPursImpl(_PosixErr, timeout) -> ?just({connectTimeout});
connectErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

parseIpAddress(Str) ->
  case inet:parse_address(binary_to_list(Str)) of
      {ok, Ip} ->
          ?just(ipTupleToPurs(Ip));
      _ ->
          ?nothing
  end.

parseIp4Address(Str) ->
  case inet:parse_ipv4_address(binary_to_list(Str)) of
      {ok, Ip} ->
          ?just(Ip);
      _ ->
          ?nothing
  end.

parseIp6Address(Str) ->
  case inet:parse_ipv6_address(binary_to_list(Str)) of
      {ok, Ip} ->
          ?just(Ip);
      _ ->
          ?nothing
  end.


ntoa4(Addr) ->
  case inet:ntoa(Addr) of
    Str when is_list(Str) -> list_to_binary(Str)
  end.

ntoa6(Addr) ->
  ntoa4(Addr).

trueSocketOptVal() -> <<1:32/native>>.

falseSocketOptVal() -> <<0:32/native>>.

getIfAddressesImpl(Left, Right) ->
  fun() ->
      case inet:getifaddrs() of
        {ok, Addrs} ->
          Right(lists:foldl(fun({InterfaceName, Options}, Map) ->
                              {Current, Acc} =
                                  lists:foldl(fun({flags, Flags}, undefined) ->
                                                  {newIfOptsFromFlags(Flags), []};
                                                 ({flags, Flags}, {Current, Acc}) ->
                                                  {newIfOptsFromFlags(Flags), [Current | Acc]};
                                                 ({addr, Addr}, undefined) ->
                                                  {newIfOptsFromAddr(Addr), []};
                                                 ({addr, Addr}, {Current = #{address := ?nothing}, Acc}) ->
                                                  {Current#{address => ?just(Addr)}, Acc};
                                                 ({addr, Addr}, {Current = #{flags := Flags}, Acc}) ->
                                                  {newIfOptsFromAddrAndFlags(Addr, Flags), [Current | Acc]};
                                                 ({netmask, Mask}, {Current, Acc}) ->
                                                  {Current#{netmask => ?just(Mask)}, Acc};
                                                 ({broadaddr, Addr}, {Current, Acc}) ->
                                                  {Current#{broadcastAddress => ?just(Addr)}, Acc};
                                                 ({dstaddr, Addr}, {Current, Acc}) ->
                                                  {Current#{dstAddress => ?just(Addr)}, Acc};
                                                 ({hwaddr, Addr}, {Current, Acc}) ->
                                                  {Current#{hwAddress => ?just(list_to_binary(Addr))}, Acc}
                                              end,
                                              undefined,
                                              Options),
                              maps:put(list_to_atom(InterfaceName), lists:map(fun ifOptsToPurs/1, [Current | Acc]), Map)
                            end,
                            #{},
                            Addrs));
        {error, Posix} ->
          Left(Posix)
      end
  end.

newIfOptsFromAddr(Addr) ->
  newIfOpts(?just(Addr), []).

newIfOptsFromFlags(Flags) ->
  newIfOpts(?nothing, ifFlagsToPurs(Flags)).

newIfOptsFromAddrAndFlags(Addr, Flags) ->
  newIfOpts(?just(Addr), Flags).

newIfOpts(Addr, Flags) ->
  #{ flags => Flags
   , address => Addr
   , netmask => ?nothing
   , broadcastAddress => ?nothing
   , dstAddress => ?nothing
   , hwAddress => ?nothing
   }.

ifOptsToPurs(#{flags := Flags
              , address := Addr
              , netmask := Netmask
              , broadcastAddress := BroadcastAddr
              , dstAddress := DestAddr
              , hwAddress := HwAddr}) ->

  InterfaceAddressType = ifAddressTypes([Addr, Netmask]),
  #{ flags => Flags
   , addresses => case InterfaceAddressType of
                    undefined -> ?nothing;
                    IpType ->
                      ?just(Addr2) = Addr,
                      ?just(Netmask2) = Netmask,
                      InterfaceAddressType2 = case IpType of
                                                ip4 -> ip4InterfaceAddresses;
                                                ip6 -> ip6InterfaceAddresses
                                              end,
                      ?just({InterfaceAddressType2, #{ address => Addr2
                                                     , netmask => Netmask2
                                                     , broadcastAddress => BroadcastAddr
                                                     , dstAddress => DestAddr
                                                     }})
                  end
   , hwAddress => HwAddr}.

ifAddressTypes([]) ->
  undefined;
ifAddressTypes([?nothing | Tail]) ->
  ifAddressTypes(Tail);
ifAddressTypes([?just(Ip) | Tail]) when tuple_size(Ip) == 4 ->
  ifAddressTypes(Tail, ip4);
ifAddressTypes([?just(Ip) | Tail]) when tuple_size(Ip) == 8 ->
  ifAddressTypes(Tail, ip6).

ifAddressTypes([], Type) ->
  Type;
ifAddressTypes([?nothing | Tail], Type) ->
  ifAddressTypes(Tail, Type);
ifAddressTypes([?just(Ip) | Tail], ip4) when tuple_size(Ip) == 4 ->
  ifAddressTypes(Tail, ip4);
ifAddressTypes([?just(Ip) | Tail], ip6) when tuple_size(Ip) == 8 ->
  ifAddressTypes(Tail, ip6).

ifFlagsToPurs(Flags) ->
  lists:map(fun(up) -> {ifUp};
               (broadcast) -> {ifBroadcast};
               (loopback) -> {ifLoopback};
               (pointtopoint) -> {ifPointToPoint};
               (running) -> {ifRunning};
               (multicast) -> {ifMulticast}
            end, Flags).

ipTupleToPurs(Ip) when tuple_size(Ip) == 4 ->
  ?ip4(Ip);
ipTupleToPurs(Ip) when tuple_size(Ip) == 8 ->
  ?ip6(Ip).


getHostByName(Host) ->
  fun() ->
      case inet:gethostbyname(unicode:characters_to_list(Host, utf8)) of
        { ok, #hostent { h_addrtype = Type,
                         h_addr_list = [ Head | _ ] } } -> { right, case Type of
                                                                      inet -> ?ip4(Head);
                                                                      inet6 -> ?ip6(Head)
                                                                    end };
      { error, Error } -> { left, Error }
    end
  end.
