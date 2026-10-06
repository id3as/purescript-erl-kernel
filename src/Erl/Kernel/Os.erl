-module(erl_kernel_os@foreign).

-export([ cmdImpl/1
        , osType/0
        , getEnv/1
        , setEnv/2
        ]).

%% os:cmd/2 takes and returns lists of unicode code points (it decodes the
%% command's UTF-8 output, passing invalid bytes through as-is), so convert with
%% the unicode module: iolist_to_binary/1 raises badarg on any code point > 255.
cmdImpl(Command) ->
    fun() ->
            unicode:characters_to_binary(os:cmd(unicode:characters_to_list(Command), #{ max_size => infinity }))
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

getEnv(Variable) ->
    fun() ->
        case os:getenv(binary_to_list(Variable)) of
            false -> {nothing};
            Val -> {just, list_to_binary(Val)}
        end
    end.

setEnv(Variable, Value) ->
    fun() ->
        os:putenv(binary_to_list(Variable), binary_to_list(Value))
    end.
