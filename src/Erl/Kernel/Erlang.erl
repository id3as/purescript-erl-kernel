-module(erl_kernel_erlang@foreign).

-export([ makeRef/0
        , sleep_/1
        , utcNowMs/0
        , vmNowMs/0
        , utcNowUs/0
        , vmNowUs/0
        , termToString/1
        , eqFfi/2
        , listToBinary/1
        , monitor/2
        , monotonicTime_/1
        , monotonicStartTime_/1
        , strictlyMonotonicInt_/1
        , currentTimeOffset_/1
        , nativeTimeToMilliseconds_/1
        , millisecondsToNativeTime_/1
        , node/0
        ]).

makeRef() ->
  fun() ->
      make_ref()
  end.

sleep_(Ms) ->
  fun() ->
      timer:sleep(Ms),
      unit
  end.

utcNowMs() ->
  fun() ->
      erlang:system_time(millisecond)
  end.

listToBinary(List) ->
  list_to_binary(List).

vmNowMs() ->
  fun() ->
      erlang:monotonic_time(millisecond)
  end.

utcNowUs() ->
  fun() ->
      erlang:system_time(microsecond)
  end.

vmNowUs() ->
  fun() ->
      erlang:monotonic_time(microsecond)
  end.

termToString(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

eqFfi(A,B) -> A == B.

monitor(Type, Item) ->
  fun() ->
    erlang:monitor(Type, Item)
  end.

monotonicTime_(Ctor) ->
  fun() ->
      Ctor(erlang:monotonic_time())
  end.

monotonicStartTime_(Ctor) ->
  Ctor(erlang:system_info(start_time)).

nativeTimeToMilliseconds_(Time) ->
  erlang:convert_time_unit(Time, native, microsecond) / 1000.

millisecondsToNativeTime_(Time) ->
  erlang:convert_time_unit(erlang:round(Time * 1000), microsecond, native).

strictlyMonotonicInt_(Ctor) ->
  fun() ->
    Ctor(erlang:unique_integer([monotonic]))
  end.

currentTimeOffset_(Ctor) ->
  fun() ->
    Ctor(erlang:time_offset())
  end.

node() -> fun() -> erlang:node() end.
