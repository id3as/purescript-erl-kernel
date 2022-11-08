-module(erl_kernel_code@foreign).

-export([ loadModule_/1
        , isLoaded_/1
        ]).

loadModule_(Module) ->
    fun() ->
            code:load_file(Module)
    end.

isLoaded_(Module) ->
    fun() ->
            case code:is_loaded(Module) of
                false -> false;
                {file, _Loaded} -> true
            end
    end.
