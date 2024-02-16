-module(erl_kernel_tcp@foreign).

-export([ acceptImpl/4
        , closeImpl/1
        , connectImpl/6
        , listenImpl/4
        , recvImpl/5
        , sendImpl/4
        , shutdownImpl/4
        , setoptsImpl/4
        , acceptErrorToPursImpl/2
        , listenErrorToPursImpl/2
        , eqSocketImpl/2
        , showSocketImpl/1
        ]).

acceptImpl(Left, Right, ListenSocket, Timeout) ->
    fun() ->
            case gen_tcp:accept(ListenSocket, Timeout) of
                {ok, Socket} ->
                    Right(Socket);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

closeImpl(Socket) ->
    fun() ->
            _ = gen_tcp:close(Socket),
            unit
    end.

connectImpl(Left, Right, Address, Port, Options, Timeout) ->
    fun() ->
            Address2 = if is_binary(Address) -> binary_to_list(Address);
                       true -> Address
                    end,
            case gen_tcp:connect(Address2, Port, Options, Timeout) of
                {ok, Socket} ->
                    Right(Socket);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

listenImpl(Left, Right, Port, Options) ->
    fun() ->
            case gen_tcp:listen(Port, Options) of
                {ok, Socket} ->
                    Right(Socket);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

recvImpl(Left, Right, Socket, Length, Timeout) ->
    fun() ->
            case gen_tcp:recv(Socket, Length, Timeout) of
                {ok, Packet} ->
                    Right(Packet);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

sendImpl(Left, Right, Socket, Packet) ->
    fun() ->
            case gen_tcp:send(Socket, Packet) of
                ok ->
                    Right(unit);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

shutdownImpl(Left, Right, Socket, How) ->
    fun() ->
            case gen_tcp:shutdown(Socket, How) of
                ok ->
                    Right(unit);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

setoptsImpl(Left, Right, Socket, Options) ->
    fun() ->
            case inet:setopts(Socket, Options) of
                ok ->
                    Right(unit);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

acceptErrorToPursImpl(_PosixErr, closed) -> {just, {acceptClosed}};
acceptErrorToPursImpl(_PosixErr, timeout) -> {just, {acceptTimeout}};
acceptErrorToPursImpl(_PosixErr, system_limit) -> {just, {acceptSystemLimit}};
acceptErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

listenErrorToPursImpl(_PosixErr, system_limit) -> {just, {listenSystemLimit}};
listenErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).


eqSocketImpl(Socket, Socket) -> true;
eqSocketImpl(_, _) -> false.

showSocketImpl(Socket) -> list_to_binary(lists:flatten(io_lib:format("~p", [Socket]))).
