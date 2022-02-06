-module(erl_types@foreign).

-export([ showRef/1
        , eqRef/2
        , refToString/1
        , stringToRef/1
        ]).

showRef(Ref) ->
    list_to_binary(ref_to_list(Ref)).

eqRef(Ref1, Ref2) ->
    Ref1 =:= Ref2.

refToString(Ref) ->
    list_to_binary(erlang:ref_to_list(Ref)).

stringToRef(Str) ->
    try
        {just, erlang:list_to_ref(binary_to_list(Str))}
    catch
        error:badarg:_ ->
            {nothing}
    end.
