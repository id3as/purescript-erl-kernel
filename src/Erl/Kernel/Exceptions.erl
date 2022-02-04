-module(erl_kernel_exceptions@foreign).

-export(['throw'/1, error/1, exit/1, 'try'/1, tryError/1, tryExit/1, tryThrown/1, tryNamedError/2, tryNoproc/1, showStack/1]).

'throw'(E) -> fun () ->
  erlang:throw(E)
end.

error(E) -> fun () ->
  erlang:error(E)
end.

exit(E) -> fun () ->
  erlang:exit(E)
end.

'try'(E) -> fun () ->
  try E() of
    Result -> {right, Result}
  catch 
    Class:Reason:Stack ->
      {left, #{ class => {Class}, reason => Reason, stack => Stack }}
  end
end.

tryError(E) -> fun () ->
  try E() of
    Result -> {right, Result}
  catch 
    error:Reason -> {left, Reason}
  end
end.

tryExit(E) -> fun () ->
  try E() of
    Result -> {right, Result}
  catch 
    exit:Reason -> {left, Reason}
  end
end.

tryThrown(E) -> fun () ->
  try E() of
    Result -> {right, Result}
  catch 
    throw:Reason -> {left, Reason}
  end
end.

tryNamedError(Atom, E) when is_atom(Atom) ->
  fun() ->
    try E() of
      Result -> {just, Result}
    catch
      error:Atom -> {nothing}
    end
  end.

tryNoproc(E) -> fun() ->
  try E() of
    Result -> {just, Result}
  catch
    exit:{noproc, _} -> {nothing};
    exit:noproc -> {nothing}
  end
end.

showStack(S) ->
  unicode:characters_to_binary(io_lib:format("~p", [S]), utf8).
