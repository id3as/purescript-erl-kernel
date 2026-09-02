-module(test_main@foreign).

-export([ makeFixtureImpl/0
        , makeRawEntryImpl/1
        , rawNamesUnsupported/0
        , makeSymlinksImpl/1
        ]).

%% Builds a throwaway directory holding one plain file and one subdirectory,
%% and returns its path with a trailing separator. Deliberately somewhere the
%% test process's cwd is not, because listDir's classification is sensitive to
%% that and the test needs the sensitivity to show.
makeFixtureImpl() ->
  fun() ->
    Dir = "/tmp/erl-kernel-listdir-fixture",
    file:del_dir_r(Dir),
    ok = file:make_dir(Dir),
    ok = file:write_file(filename:join(Dir, "afile.txt"), <<>>),
    ok = file:make_dir(filename:join(Dir, "subdir")),
    unicode:characters_to_binary(Dir ++ "/")
  end.

%% Tries to create a file whose name is not valid UTF-8. Reports whether the
%% filesystem allowed it: APFS refuses with eilseq, so on macOS the caller has
%% nothing to assert against and skips.
makeRawEntryImpl(Dir) ->
  fun() ->
    Raw = <<Dir/binary, "raw-", 16#FF>>,
    case file:write_file(Raw, <<>>) of
      ok -> true;
      {error, _} -> false
    end
  end.

%% Distinguishes "this filesystem will not hold a non-UTF-8 name" from "the
%% fixture failed for some other reason", so the raw-name test can skip visibly
%% rather than pass by accident.
rawNamesUnsupported() ->
  fun() ->
    Probe = <<"/tmp/erl-kernel-raw-probe-", 16#FF>>,
    case file:write_file(Probe, <<>>) of
      ok -> file:delete(Probe), false;
      {error, eilseq} -> true;
      {error, _} -> true
    end
  end.

%% One link to a file that exists and one to a name that does not, so the tests
%% can tell "follows the link" from "sees the link" and "absent" from "dangling".
%% Reports whether the filesystem allowed them -- symlinks are not universal, and
%% a skip should be visible rather than a failure.
makeSymlinksImpl(Dir) ->
  fun() ->
    case { file:make_symlink("afile.txt", <<Dir/binary, "alink">>)
         , file:make_symlink("nowhere", <<Dir/binary, "broken">>) } of
      {ok, ok} -> true;
      _ -> false
    end
  end.
