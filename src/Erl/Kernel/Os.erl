-module(erl_kernel_os@foreign).

-export([ cmdImpl/1
        , osType/0
        ]).

cmdImpl(Command) ->
    fun() ->
            iolist_to_binary(os:cmd(binary_to_list(Command), #{ max_size => infinity }))
    end.

osType() ->
    fun() ->
            {OsFamily, OsName} = os:type(),
            { osType
            , case OsFamily of
                  unix -> {unix};
                 win32 -> {windows}
              end
            , case OsName of
                  darwin -> {darwin};
                  linux -> {linux};
                  nt -> {windowNt};
                  Other -> {other, Other}
              end
            }
    end.
