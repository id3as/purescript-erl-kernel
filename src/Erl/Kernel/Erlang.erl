-module(erl_kernel_erlang@foreign).

-export([ makeRef/0
        , sleep_/1
        , utcNowMs/0
        , vmNowMs/0
        , utcNowUs/0
        , vmNowUs/0
        , termToString/1
        , termToBinary/1
        , binaryToTerm/1
        , eqFfi/2
        , listToBinary/1
        , monitor/2
        , monotonicTime_/1
        , monotonicStartTime_/1
        , strictlyMonotonicInt_/1
        , currentTimeOffset_/1
        , nativeTimeToMilliseconds_/1
        , nativeTimeUnit/0
        , millisecondsToNativeTime_/1
        , node/0
        , uniqueInteger_/1
        , cpuTopology/0
        , totalSystemMemory/0
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

termToBinary(Term) ->
    term_to_binary(Term).

binaryToTerm(Binary) ->
    binary_to_term(Binary).

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

nativeTimeUnit() ->
  erlang:convert_time_unit(1, second, native).

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

uniqueInteger_(Options) ->
  fun() ->
    Options2 = [case Option of
                  {positiveUniqueInteger} -> positive;
                  {monotonicUniqueInteger} -> monotonic
                end || Option <- Options],

    erlang:unique_integer(Options2)
  end.

cpuTopology() ->
  fun() ->
      case erlang:system_info(cpu_topology) of
        undefined -> [];
        Topology -> nodeTopology(Topology)
      end
  end.

%% Heirarchy for our output is nodes -> processors -> cores -> threads
nodeTopology(NodesOrProcessors) ->
  %% Top level could be nodes or processors
  {Nodes, Processors} = lists:partition(fun({node, _}) ->
                                            true;
                                           (_) ->
                                            false
                                        end, NodesOrProcessors),
  Nodes2 = case Processors of
             [] ->
               Nodes;
             _ ->
               Nodes ++ [{node, Processors}]
           end,
  [processorTopology(NodeProcessors) || {node, NodeProcessors} <- Nodes2].

processorTopology(Processors) ->
  %% Processors are just processors
  [coreTopology(Cores) || {processor, Cores} <- Processors].

coreTopology(CoresOrThreads) ->
  %% Core level could be cores or threads
  {Cores, Threads} = lists:partition(fun({core, _}) ->
                                            true;
                                           (_) ->
                                            false
                                        end, CoresOrThreads),
  Cores2 = case Threads of
             [] ->
               Cores;
             _ ->
               Cores ++ [{core, Threads}]
           end,
  [threadTopology(CoreThreads) || {core, CoreThreads} <- Cores2].

threadTopology({logical, Id}) ->
  [Id];
threadTopology([]) ->
  [];
threadTopology([{thread, {logical, Id}} | T]) ->
  [Id | threadTopology(T)].

totalSystemMemory() ->
  fun() ->
      case memsup:get_system_memory_data() of
        [] ->
          {nothing};
        PropList ->
          case lists:keyfind(system_total_memory, 1, PropList) of
            {system_total_memory, Mem} -> {just, Mem};
            _ -> {nothing}
          end
     end
  end.
