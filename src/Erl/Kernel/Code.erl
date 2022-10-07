-module(erl_kernel_code@foreign).

-export([ loadModule_/1
        ]).

loadModule_(Module) ->
    fun() ->
            code:load_file(Module)
    end.
