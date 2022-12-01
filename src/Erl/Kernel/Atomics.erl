-module('erl_kernel_atomics@foreign').

-export([ new/2
        , put/3
        , get/2
        , add/3
        , addGet/3
        , sub/3
        , subGet/3
        , exchange/3
        , compareExchange/4
        , info/1
        ]).

new(Arity, Signedness) ->
    fun() ->
            atomics:new(Arity, case Signedness of
                                   {signed} -> [{signed, true}];
                                   {unsigned} -> [{signed, false}]
                               end)
    end.

put(Ref, Ix, Value) ->
    fun() ->
            ok = atomics:put(Ref, Ix, Value),
            unit
    end.

get(Ref, Ix) ->
    fun() ->
            atomics:get(Ref, Ix)
    end.

add(Ref, Ix, Incr) ->
    fun() ->
            ok = atomics:add(Ref, Ix, Incr),
            unit
    end.

addGet(Ref, Ix, Incr) ->
    fun() ->
            atomics:add_get(Ref, Ix, Incr)
    end.

sub(Ref, Ix, Incr) ->
    fun() ->
            ok = atomics:sub(Ref, Ix, Incr),
            unit
    end.

subGet(Ref, Ix, Incr) ->
    fun() ->
            atomics:sub_get(Ref, Ix, Incr)
    end.

exchange(Ref, Ix, Desired) ->
    fun() ->
            atomics:exchange(Ref, Ix, Desired)
    end.

compareExchange(Ref, Ix, Expected, Desired) ->
    fun() ->
            case atomics:compare_exchange(Ref, Ix, Expected, Desired) of
                ok ->
                    {right, Desired};
                Actual ->
                    {left, Actual}
            end
    end.

info(Ref) ->
    fun() ->
            atomics:info(Ref)
    end.
