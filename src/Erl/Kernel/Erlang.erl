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
        , ordFfi/2
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

ordFfi(A,B) ->
  if A < B -> {lT};
     A > B -> {gT};
     A == B -> {eQ}
  end.

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

%% Hierarchy for our output is nodes -> processors -> cores -> threads.
%%
%% erlang:system_info(cpu_topology) is looser than that fixed shape: any level
%% except the logical-cpu leaf may be absent, entries may carry an InfoList
%% ({Tag, Info, SubLevel} as well as {Tag, SubLevel}), and `processor` may sit
%% either above OR below `node`. Multi-die packages / sub-NUMA clustering (seen
%% on large cloud instances) report e.g.
%%   [{processor, [{node, [{core, [{thread, {logical, N}}, ...]}, ...]}, ...]}]
%% i.e. a processor spanning several NUMA nodes with no per-node processor
%% grouping. We normalise all of that into the fixed 4-level nesting,
%% synthesising a singleton level wherever one is omitted, so an unexpected
%% shape degrades to sensible grouping instead of crashing with function_clause.
nodeTopology(Entries) ->
  Es = as_list(Entries),
  case [E || E <- Es, level_tag(E) =:= node] of
    [] ->
      %% No node level here. If a processor level sits above the nodes (a
      %% processor spanning multiple NUMA nodes), descend through it; otherwise
      %% treat the whole system as a single node.
      case lists:append([as_list(level_sub(E)) || E <- Es, level_tag(E) =:= processor]) of
        [] ->
          [processorTopology(Es)];
        Inner ->
          nodeTopology(Inner)
      end;
    Nodes ->
      [processorTopology(level_sub(N)) || N <- Nodes]
  end.

processorTopology(Entries) ->
  Es = as_list(Entries),
  case [E || E <- Es, level_tag(E) =:= processor] of
    [] ->
      %% No processor level: synthesise a single processor holding the cores.
      [coreTopology(Es)];
    Processors ->
      [coreTopology(level_sub(P)) || P <- Processors]
  end.

coreTopology(Entries) ->
  Es = as_list(Entries),
  case [E || E <- Es, level_tag(E) =:= core] of
    [] ->
      %% No core level: synthesise a single core holding the threads.
      [threadTopology(Es)];
    Cores ->
      [threadTopology(level_sub(C)) || C <- Cores]
  end.

threadTopology({logical, Id}) ->
  %% Thread level omitted: the core's sublevel is a bare logical cpu id.
  [Id];
threadTopology(Entries) when is_list(Entries) ->
  lists:append([logicalId(E) || E <- Entries]).

logicalId({logical, Id}) -> [Id];
logicalId({thread, Sub}) -> threadTopology(Sub);
logicalId({thread, _Info, Sub}) -> threadTopology(Sub);
logicalId(_) -> [].

%% erlang:system_info(cpu_topology) entries are {Tag, SubLevel} or, with an
%% info list, {Tag, InfoList, SubLevel}. These two helpers read either form.
level_tag({Tag, _Info, _Sub}) -> Tag;
level_tag({Tag, _Sub}) -> Tag;
level_tag(_) -> undefined.

level_sub({_Tag, _Info, Sub}) -> Sub;
level_sub({_Tag, Sub}) -> Sub.

as_list(L) when is_list(L) -> L;
as_list(X) -> [X].

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
