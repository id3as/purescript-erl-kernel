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

### Added

- `Erl.Kernel.Filename` — `Filename`, `filename`, `rawFilename`,
  `filenameToString`, `filenameToBinary`.
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
