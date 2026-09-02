# Changelog

## v1.0.0

The first tag since `v0.0.3` (February 2022), and the first major. Everything
below is measured against `v0.0.3`, because that is what the curated purerl
package set serves — `erl-0.15.3-20220629`, still the newest set, predates most
of this work. Consumers pinning `master` by git ref have seen the additions
already; what is new to them is the `Filename` boundary.

### Breaking

**`Erl.Kernel.File` deals in `Filename`, and `Filename` composes nothing.**
`FileName` and `Directory` are gone. The replacement lives in its own module,
`Erl.Kernel.Filename`, and differs from what it replaces in three ways that are
all deliberate:

- **The constructor is not exported and there is no `Newtype` instance**, so
  `filename :: String -> Maybe Filename` and `rawFilename :: Binary -> Filename`
  are the only ways in. `FileName(..)` derived `Newtype`, which made any
  validation next to it decorative.
- **It is backed by `Binary`, not `String`.** POSIX filenames are bytes; names
  that are not valid UTF-8 exist on disk, and `String` could not hold one.
  `filenameToString` is therefore a `Maybe`.
- **There is no `Semigroup`.** `FileName`'s `append` was `filename:join/2`, and
  `filename:join("/a", "/b")` is `"/b"` — an absolute right-hand side silently
  replacing the base, which is the canonical traversal primitive. Composition
  belongs in a path library that can type the operands; hand the result here.

`Erl.Types` loses `SandboxedDir` and `SandboxedFile` and their `ToErl`
instances. `Erl.Kernel.Tcp` and `Erl.Kernel.Udp`'s `netns` takes a `Filename`.
`Erl.Kernel.File` re-exports `Filename`, so an ordinary call site still imports
one module.

(Between `v0.0.3` and this tag, `master` briefly took `pathy`'s
`SandboxedPath` in these positions. No tag ever shipped that, and no package
set ever resolved to it. A bindings library should not impose a policy library
on every consumer: every function here was `impl <<< fileToString`, consuming
the path type at the FFI boundary and discarding it, yet any project that
wanted to open a file bought `pathy` transitively.)

**`listDir` returns entry names, and returns all of them.** It used to classify
each entry as a directory or a file with `filelib:is_dir/1` — on the *bare* entry
name, which resolves against the process cwd rather than the directory being
listed, so unless the two coincided every subdirectory came back typed as a
file. Classifying there also bought a stat per entry and a TOCTOU window between
the list and the stat. Callers wanting the distinction should join the entry onto
the directory it came from and call `readFileInfo`.

It now calls `file:list_dir_all/1` rather than `file:list_dir/1`. `list_dir/1`
silently drops entries whose names are not valid UTF-8, so a directory holding
one was under-reported with nothing in the logs. **Callers will start seeing
entries they have never seen**, and `filenameToString` on those returns
`Nothing`.

**`cwd` returns what `file:get_cwd/0` returns.** It no longer appends a trailing
separator; that existed only to satisfy a path parser downstream.

**`show` on a `PosixError` names the error.** Every constructor used to print as
`"file posix"`, which is diagnostic rot in an error path.

**Tcp connect errors no longer crash on an unrecognised atom.** An atom this
package does not map now arrives as `ConnectOther` instead of raising.

**`Erl.Kernel.Filename` also owns the path type now, and it rejects `..`.**
`Path a b`, `Name b`, the `Abs`/`Rel` and `Dir`/`File` phantoms, `</>`, the four
parsers and `toFilename` all live in that module, alongside `Filename` itself.
This is where `pathy` went: nothing in this package depends on a path library
any more, and nothing needs to.

The representation is a `String` newtype, normalised on construction, rather
than a structural tree that has to be rendered. Three consequences worth
knowing:

- **`..` is rejected by the parsers rather than resolved by them.** The
  structural parser resolved it, so `parseAbsFile "/data/../../../etc/passwd"`
  returned a perfectly good `/etc/passwd` and nothing downstream could tell it
  from a path that had always been spelled that way. That is a successful parse
  of hostile input, and rejecting it is the point of the change. It is
  wire-visible: a caller that sends `a/../b` for a path worked before and is
  refused now. Input that legitimately contains `..` — a URL reference, say —
  wants RFC 3986 resolution (`uri_string:resolve/2`), not a filesystem type.
- **`printPath` is `unwrap`**, so there is no `Printer`, no `Escaper` and no
  `SandboxedPath`. Containment falls out of `</>`: the right-hand side of an
  append cannot be absolute and cannot ascend, so the result is provably inside
  the base. Note this is *lexical* normalisation and never containment — a
  `..`-free relative path still escapes through a symlink, which is why OTP
  itself moved `safe_relative_path` out of `filename` and into `filelib`, where
  it can take a `Cwd` and resolve links.
- **Mangling became rejection.** The structural printer ran a `posixEscaper`
  implicitly on every render, silently rewriting `/` to `-` and `..` to
  `$dot$dot`. `Name`'s constructor is closed and enforces the four POSIX clauses
  instead — non-empty, no `/`, no NUL, not `.` or `..` — and a literal is checked
  at the type level, so `dir (Proxy :: _ "a/b")` no longer compiles rather than
  quietly becoming `a-b`.

Two smaller behaviour changes fall out. A trailing slash now positively asserts
`Dir` and its absence asserts nothing, so `parseAbsDir "/a/b"` succeeds where it
used to return `Nothing` — a pure relaxation, no string that parsed before
changes its value. And `Ord` is the string order rather than the constructor
order, so `/a-b` and `/a/b` swap; nothing here sorts paths, but a consumer might.

`toFilename :: Path Abs b -> Filename` is the only bridge to the runtime, and it
has no `Rel` overload. A relative name resolves against the emulator's cwd, which
is per-node rather than per-process and can be moved by any process in the
system — for `file:` and raw `prim_file` calls alike, so `raw` is not an escape.
Supply the base: `toFilename (base </> rel)`.

### Added

- `Erl.Kernel.Filename` — `Filename`, `filename`, `rawFilename`,
  `filenameToString`, `filenameToBinary`; and the path surface above: `Path`,
  `AbsDir`/`AbsFile`/`RelDir`/`RelFile`, `Name`, `name`, `rootDir`,
  `currentDir`, `dir`/`file`, `extendPath`, `</>`, `printPath`, `parseAbsDir`,
  `parseAbsFile`, `parseRelDir`, `parseRelFile`, `peel`, `peelFile`, `fileName`,
  `rename`, `splitName`, `joinName`, `extension`, `<.>`, `toFilename`.
- `Erl.Kernel.File` — `readFileInfo` and `readLinkInfo` (`FileInfo`, `FileType`,
  `FileAccess`), `makeDir`, `listDir`, `delete`, `delDir`, `delDirR`, `rename`,
  `copy`, `cwd`, `length`, `seek`, `sync`, `truncate`, `pread`, `pwrite`,
  `fileErrorToPurs`, `Location`, `FilePositioning`.
- `Erl.Kernel.Atomics` and `Erl.Kernel.Code` — new modules.
- `Erl.Kernel.Erlang` — `cpuTopology` (with `NumaNode`, `Processor`, `Core`,
  `LogicalCpuId`), `totalSystemMemory`, `uniqueInteger`, `termToBinary`,
  `binaryToTerm`, `nativeTimeUnit`, `millisecondsToNativeTime`,
  `microsecondsToMilliseconds`.
- `Erl.Kernel.Inet` — `getHostName`, `getHostByName`, `hostAddressToIp`,
  `printIpAddress`, `printIpv4`, `printIpv6`.
- `Erl.Kernel.Os` — `getEnv`, `setEnv`.
- `Erl.Kernel.Exceptions` — `tryTimeout`, and `throw`/`error`/`exit` generalised.
- `Erl.Kernel.Ets` — `lookup`, `delete`, `toList`, `matchObject`, `matchDelete`,
  batch insert, and the concurrency options.
- `Erl.Types` — `refToString`, `stringToRef`, and assorted `Eq`/`Ord`/`Show`
  instances.

### Fixed

- `erlang:system_info(cpu_topology)` is looser than a fixed four-level nesting:
  any level except the logical-cpu leaf may be absent, entries may carry an info
  list, and `processor` may sit either above or below `node`. Multi-die packages
  and sub-NUMA clustering hit all three, and used to crash with a
  `function_clause`.
