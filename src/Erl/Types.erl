-module(erl_types@foreign).

-export([ showRef/1
        , eqRef/2
        ]).

showRef(Ref) ->
    list_to_binary(ref_to_list(Ref)).

eqRef(Ref1, Ref2) ->
    Ref1 =:= Ref2.
