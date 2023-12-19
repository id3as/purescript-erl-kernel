-module(erl_kernel_file@foreign).

-export([
         openImpl/3,
         readImpl/2,
         readFileImpl/3,
         writeImpl/4,
         writeFileImpl/4,
         renameImpl/4,
         closeImpl/3,
         syncImpl/3,
         seekImpl/5,
         copyImpl/5,
         deleteImpl/3,
         cwdImpl/2,
         listDirImpl/3,
         posixErrorToPurs/1,
         fileErrorToPurs/1,
         unsafeRead/2
        ]).

openImpl(Left, Right, DefaultOptions) ->
      fun(FileName) ->
          fun(Options) ->
              fun() ->
                  #{ modes := OpenModes
                   , raw := Raw
                   , output := Output
                   , delayedWrite := DelayedWrite
                   , readAhead := ReadAhead
                   , compressed := Compressed
                   , encoding := Encoding
                   , ram := Ram
                   , sync := Sync
                   , directory := Directory
                   } = maps:merge(DefaultOptions, Options),

                  Modes = [
                           case Raw of
                             true -> raw;
                             false -> undefined
                           end,
                           case Output of
                             {binary} -> binary;
                             {list} -> undefined
                           end,
                           case DelayedWrite of
                             {just, {delayedWriteDefault}} -> delayed_write;
                             {just, {delayedWrite, Size, Delay}} -> {delayed_write, Size, Delay};
                             {nothing} -> undefined
                           end,
                           case ReadAhead of
                             {just, {readAheadDefault}} -> read_ahead;
                             {just, {readAhead, Size}} -> {read_ahead, Size};
                             {nothing} -> undefined
                           end,
                           case Compressed of
                             true -> compressed;
                             false -> undefined
                           end,
                           case Encoding of
                             {just, Enc} ->
                               {encoding, case Enc of
                                            {latin1} -> latin1;
                                            {utf8} -> utf8;
                                            {utf16Big} -> {utf16, big};
                                            {utf16Little} -> {utf16, little};
                                            {utf32Big} -> {utf32, big};
                                            {utf32Little} -> {utf32, little}
                                          end
                               };
                             {nothing} ->
                               undefined
                           end,
                           case Ram of
                             true -> ram;
                             false -> undefined
                           end,
                           case Sync of
                             true -> sync;
                             false -> undefined
                           end,
                           case Directory of
                             true -> directory;
                             false -> undefined
                           end
                           |
                           [case Mode of
                              {read} -> read;
                              {write} -> write;
                              {append} -> append;
                              {exclusive} -> exclusive
                            end || Mode <- OpenModes]
                          ],

                    Modes2 = [Mode || Mode <- Modes,
                                      Mode /= undefined],
                  case file:open(FileName, Modes2) of
                    {ok, Handle} -> Right(Handle);
                    {error, Err} -> Left(fileErrorToPurs(Err))
                  end
              end
      end
  end.

readImpl(Handle, Amount) ->
  fun() ->
    unsafeRead(Handle, Amount)
  end.

%% This function exists and is exported since it is known about by the purerl optimiser to 
%% enable it to remove the thunk around `readImpl`
unsafeRead(Handle, Amount) ->
  case file:read(Handle, Amount) of
    {ok, Data} ->
       {right, Data};
    eof ->
      {left, fileErrorToPurs(eof)};
    {error, Err} ->
      {left, fileErrorToPurs(Err)}
  end.

readFileImpl(Left, Right, Filename) ->
  fun() ->
      case file:read_file(Filename) of
        {ok, Data} ->
          Right(Data);
        {error, Err} ->
          Left(fileErrorToPurs(Err))
      end
  end.

renameImpl(Left, Right, Source, Dest) ->
  fun() ->
    case file:rename(Source, Dest) of
      ok ->
        Right;
      {error, Err} ->
        Left(fileErrorToPurs(Err))
    end
  end.

writeImpl(Left, Right, Handle, Data) ->
  fun() ->
      case file:write(Handle, Data) of
        ok ->
          Right;
        {error, Err} ->
          Left(fileErrorToPurs(Err))
      end
  end.

writeFileImpl(Left, Right, FileName, Data) ->
  fun() ->
      case file:write_file(FileName, Data) of
        ok ->
          Right;
        {error, Err} ->
          Left(fileErrorToPurs(Err))
      end
  end.

closeImpl(Left, Right, Handle) ->
  fun() ->
      case file:close(Handle) of
        ok -> Right;
        {error, Err} ->
          Left(fileErrorToPurs(Err))
      end
  end.

deleteImpl(Left, Right, Handle) ->
  fun() ->
      case file:delete(Handle) of
        ok -> Right;
        {error, Err} ->
          Left(fileErrorToPurs(Err))
      end
  end.

syncImpl(Left, Right, Handle) ->
  fun() ->
      case file:sync(Handle) of
        ok -> Right;
        {error, Err} ->
          Left(fileErrorToPurs(Err))
      end
  end.

seekImpl(Left, Right, Handle, Position, Offset) ->
  Location =
    case Position of
      {fromBeginning} -> bof;
      {fromCurrent} -> cur;
      {fromEnd} -> eof
    end,
  fun() ->
      case file:position(Handle, {Location, Offset}) of
        {ok, Cur} -> Right(Cur);
        {error, Err} ->
          Left(fileErrorToPurs(Err))
      end
  end.

copyImpl(Left, Right, Handle1, Handle2, Amount) ->
  BytesToCopy =
    case Amount of
      {nothing} -> infinity;
      {just, ByteCount} -> ByteCount
    end,
  fun () ->
      case file:copy(Handle1, Handle2, BytesToCopy) of
        {ok, BytesCopied} -> Right(BytesCopied);
        {error, Err} -> Left(fileErrorToPurs(Err))
      end
  end.

cwdImpl(Left, Right) ->
  fun() ->
      case file:get_cwd() of
        {ok, Cwd} -> Right(<<(unicode:characters_to_binary(Cwd))/binary, "/">>);
        {error, Err} -> Left(fileErrorToPurs(Err))
      end
  end.

listDirImpl(Left, Right, Dir) ->
  fun() ->
    Result = file:list_dir(Dir),
    case Result of
      {ok, Names} -> 
        Right(
          lists:map(fun(Name) ->
            Bin = unicode:characters_to_binary(Name),
            case filelib:is_dir(Name) of
              true -> {left, <<Bin/binary, "/">>};
              false -> {right, Bin}
            end
          end,
          Names)
        );

      {error, Err} -> Left(fileErrorToPurs(Err))
    end
  end.

posixErrorToPurs(eacces) -> {just, {eAcces}};
posixErrorToPurs(eagain) -> {just, {eAgain}};
posixErrorToPurs(ebadf) -> {just, {eBadf}};
posixErrorToPurs(ebadmsg) -> {just, {eBadmsg}};
posixErrorToPurs(ebusy) -> {just, {eBusy}};
posixErrorToPurs(edeadlk) -> {just, {eDeadlk}};
posixErrorToPurs(edeadlock) -> {just, {eDeadlock}};
posixErrorToPurs(edquot) -> {just, {eDquot}};
posixErrorToPurs(eexist) -> {just, {eExist}};
posixErrorToPurs(efault) -> {just, {eFault}};
posixErrorToPurs(efbig) -> {just, {eFbig}};
posixErrorToPurs(eftype) -> {just, {eFtype}};
posixErrorToPurs(eintr) -> {just, {eIntr}};
posixErrorToPurs(einval) -> {just, {eInval}};
posixErrorToPurs(eio) -> {just, {eIo}};
posixErrorToPurs(eisdir) -> {just, {eIsdir}};
posixErrorToPurs(eloop) -> {just, {eLoop}};
posixErrorToPurs(emfile) -> {just, {eMfile}};
posixErrorToPurs(emlink) -> {just, {eMlink}};
posixErrorToPurs(emultihop) -> {just, {eMultihop}};
posixErrorToPurs(enametoolong) -> {just, {eNametoolong}};
posixErrorToPurs(enfile) -> {just, {eNfile}};
posixErrorToPurs(enobufs) -> {just, {eNobufs}};
posixErrorToPurs(enodev) -> {just, {eNodev}};
posixErrorToPurs(enolck) -> {just, {eNolck}};
posixErrorToPurs(enolink) -> {just, {eNolink}};
posixErrorToPurs(enoent) -> {just, {eNoent}};
posixErrorToPurs(enomem) -> {just, {eNomem}};
posixErrorToPurs(enospc) -> {just, {eNospc}};
posixErrorToPurs(enosr) -> {just, {eNosr}};
posixErrorToPurs(enostr) -> {just, {eNostr}};
posixErrorToPurs(enosys) -> {just, {eNosys}};
posixErrorToPurs(enotblk) -> {just, {eNotblk}};
posixErrorToPurs(enotdir) -> {just, {eNotdir}};
posixErrorToPurs(enotsup) -> {just, {eNotsup}};
posixErrorToPurs(enxio) -> {just, {eNxio}};
posixErrorToPurs(eopnotsupp) -> {just, {eOpnotsupp}};
posixErrorToPurs(eoverflow) -> {just, {eOverflow}};
posixErrorToPurs(eperm) -> {just, {ePerm}};
posixErrorToPurs(epipe) -> {just, {ePipe}};
posixErrorToPurs(erange) -> {just, {eRange}};
posixErrorToPurs(erofs) -> {just, {eRofs}};
posixErrorToPurs(espipe) -> {just, {eSpipe}};
posixErrorToPurs(esrch) -> {just, {eSrch}};
posixErrorToPurs(estale) -> {just, {eStale}};
posixErrorToPurs(etxtbsy) -> {just, {eTxtbsy}};
posixErrorToPurs(exdev) -> {just, {eXdev}};
posixErrorToPurs(_) -> {nothing}.

fileErrorToPurs(eof) -> {eof};
fileErrorToPurs(badarg) -> {badArg};
fileErrorToPurs(systemlimit) -> {systemLimit};
fileErrorToPurs(terminated) -> {terminated};
fileErrorToPurs({no_translation, unicode, latin1}) -> {noTranslation};
fileErrorToPurs(Other) ->
  case posixErrorToPurs(Other) of
    {nothing} -> {other, Other};
    {just, Posix} -> {posix, Posix}
  end.

%% %% Non Posix
%% errorToPurs(eof) -> {eof};
%% errorToPurs(badarg) -> {badArg};
%% errorToPurs(systemlimit) -> {systemLimit};
%% errorToPurs(terminated) -> {terminated};
%% errorToPurs({no_translation, unicode, latin1}) -> {noTranslation};

%% %% Posix
%% errorToPurs(eacces) -> {posix, {eAcces}};
%% errorToPurs(eagain) -> {posix, {eAgain}};
%% errorToPurs(ebadf) -> {posix, {eBadf}};
%% errorToPurs(ebadmsg) -> {posix, {eBadmsg}};
%% errorToPurs(ebusy) -> {posix, {eBusy}};
%% errorToPurs(edeadlk) -> {posix, {eDeadlk}};
%% errorToPurs(edeadlock) -> {posix, {eDeadlock}};
%% errorToPurs(edquot) -> {posix, {eDquot}};
%% errorToPurs(eexist) -> {posix, {eExist}};
%% errorToPurs(efault) -> {posix, {eFault}};
%% errorToPurs(efbig) -> {posix, {eFbig}};
%% errorToPurs(eftype) -> {posix, {eFtype}};
%% errorToPurs(eintr) -> {posix, {eIntr}};
%% errorToPurs(einval) -> {posix, {eInval}};
%% errorToPurs(eio) -> {posix, {eIo}};
%% errorToPurs(eisdir) -> {posix, {eIsdir}};
%% errorToPurs(eloop) -> {posix, {eLoop}};
%% errorToPurs(emfile) -> {posix, {eMfile}};
%% errorToPurs(emlink) -> {posix, {eMlink}};
%% errorToPurs(emultihop) -> {posix, {eMultihop}};
%% errorToPurs(enametoolong) -> {posix, {eNametoolong}};
%% errorToPurs(enfile) -> {posix, {eNfile}};
%% errorToPurs(enobufs) -> {posix, {eNobufs}};
%% errorToPurs(enodev) -> {posix, {eNodev}};
%% errorToPurs(enolck) -> {posix, {eNolck}};
%% errorToPurs(enolink) -> {posix, {eNolink}};
%% errorToPurs(enoent) -> {posix, {eNoent}};
%% errorToPurs(enomem) -> {posix, {eNomem}};
%% errorToPurs(enospc) -> {posix, {eNospc}};
%% errorToPurs(enosr) -> {posix, {eNosr}};
%% errorToPurs(enostr) -> {posix, {eNostr}};
%% errorToPurs(enosys) -> {posix, {eNosys}};
%% errorToPurs(enotblk) -> {posix, {eNotblk}};
%% errorToPurs(enotdir) -> {posix, {eNotdir}};
%% errorToPurs(enotsup) -> {posix, {eNotsup}};
%% errorToPurs(enxio) -> {posix, {eNxio}};
%% errorToPurs(eopnotsupp) -> {posix, {eOpnotsupp}};
%% errorToPurs(eoverflow) -> {posix, {eOverflow}};
%% errorToPurs(eperm) -> {posix, {ePerm}};
%% errorToPurs(epipe) -> {posix, {ePipe}};
%% errorToPurs(erange) -> {posix, {eRange}};
%% errorToPurs(erofs) -> {posix, {eRofs}};
%% errorToPurs(espipe) -> {posix, {eSpipe}};
%% errorToPurs(esrch) -> {posix, {eSrch}};
%% errorToPurs(estale) -> {posix, {eStale}};
%% errorToPurs(etxtbsy) -> {posix, {eTxtbsy}};
%% errorToPurs(exdev) -> {posix, {eXdev}};

%% %% Other
%% errorToPurs(Other) -> {other, Other}.
