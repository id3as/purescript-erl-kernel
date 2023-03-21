-module(erl_kernel_ets@foreign).

-export([ new/2
        , insert2/2
        , insert3/2
        , updateCounter/3
        , increment/4
        , lookupElement/3
        , updateElement/4
        , updateElements/3
        , match/2
        , select/2
        , selectOp/2
        , selectOne/2
        , delete/1
        , delete/2
        ]).

new(Name, Options) ->
  fun() ->
    ets:new(Name, Options)
  end.

insert2(Table, Tuple) ->
  fun() ->
      ets:insert(Table, Tuple)
  end.

insert3(Table, Tuple) ->
  fun() ->
      ets:insert(Table, Tuple)
  end.

updateCounter(Table, Key, UpdateOp) ->
  fun() ->
      ets:update_counter(Table, Key, UpdateOp)
  end.

lookupElement(Table, Key, Index) ->
  fun() ->
      ets:lookup_element(Table, Key, Index)
  end.

increment(Table, Key, Index, Value) ->
  fun() ->
      case Value of
        Value when is_integer(Value) ->
          ets:update_counter(Table, Key, { Index, Value });
        Value ->
          Old = ets:lookup_element(Table, Key, Index),
          ets:update_element(Table, Key, { Index, Value + Old })
      end
  end.

updateElement(Table, Key, Index, Value) ->
  fun() ->
      ets:update_element(Table, Key, { Index, Value })
  end.

updateElements(Table, Key, Specs) ->
  fun() ->
      ets:update_element(Table, Key, Specs)
  end.

match(Table, Spec) ->
  fun() ->
      lists:map(fun(I) -> list_to_tuple(I) end, ets:match(Table, Spec))
  end.


select(Table, SelectOp) ->
  fun() ->
    ets:select(Table, SelectOp)
  end.

selectOne(Table, SelectOp) ->
  fun() ->
      case ets:select(Table, SelectOp) of
        [ One ] -> {just, One};
        _ -> {nothing}
      end
  end.

selectOp(K, V) ->
  [ {K, [], V} ].


delete(Table) ->
  fun() ->
    ets:delete(Table)
  end.

delete2(Table, Key) ->
  fun() ->
    ets:delete(Table, Key)
  end.