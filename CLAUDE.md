# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

`findjar` is a Clojure CLI tool that searches files on disk and inside
`.jar`/`.zip` archives, with regex matching on file name/path/content,
optional grep with context lines, ANSI highlighting, content dump, and
hashing (md5/sha1/sha256/sha512/crc32). Single-shot CLI process; entry
point is `findjar.main/-main`.

## Common commands

Build, test, and run all use the Clojure CLI (`clj` / `clojure`) — there is
no `project.clj`/Leiningen config. Java 11/17/21 are supported (CI matrix).

- Build uberjar: `clj -T:build uber` → `target/findjar-<version>-standalone.jar`.
  The build also generates `gen-resources/build/version.edn` from git
  metadata; this file is read at runtime by `findjar.cli/version-string`.
- Clean: `clj -T:build clean` (removes `target/`).
- Run all tests: `clj -M:test` (Kaocha, configured by `tests.edn`).
- Run a single test: `clj -M:test --focus findjar.core-test/a-test`
  (Kaocha `--focus` takes a fully-qualified var or namespace).
- Run from source without building: `clj -M -m findjar.main <search-root> [opts]`.
- Run the built jar: `java -jar target/findjar-<version>-standalone.jar <search-root> [opts]`.
- REPL against the build script (for hacking on `build.clj` from Cursive/IntelliJ):
  `clj -A:build:nrepl -M -m nrepl.cmdline -p 34567`.

## Architecture

Seven namespaces under `src/findjar/` (+ `output/`). Side effects are pushed
to the edges via the `FindJarOutput` protocol, so the core scanning logic
is pure-ish and pluggable.

- `findjar.protocols` — defines `FindJarOutput` (sink for `warn`, `match`,
  `grep-match`, `dump-stream`, `print-hash`). All side effects flow
  through this protocol.
- `findjar.core` — the scanner. `perform-scan` walks the filesystem via
  the custom `walk-tree` (which handles `--max-depth`, `--exclude`,
  default-excluded-dirs, `--follow`/symlinks, and `.gitignore`), applies
  the file-type filter, dispatches to the registered finder for each file,
  and runs `handle-match` which checks `--name`/`--path`/`--apath`/`--grep`
  in turn and then either reports a path match, runs `grep-stream`, hashes,
  searches by hash, or hands a materialized cat string to the output sink.
  Sliding-window context lines for grep support asymmetric `-A`/`-B` via
  `effective-context`. `scan-jar` opens top-level archives via `ZipFile`;
  `scan-zip-stream` handles `--nested` recursion via `ZipInputStream` over
  cached entry bytes (so multi-pass actions like `--cat` and
  `--find-by-hash` work for nested entries too). Stream lifecycles are
  managed by `with-stream` / `with-reader` helpers taking a 0-arg
  `stream-factory` fn.
- `findjar.output.buffering` — `Buffer` deftype + `parallel-scan` driver.
  Workers scan into per-file buffers; recorded calls replay in input
  order against the real output. Worker exceptions are caught and turned
  into `:warn` calls so a single bad file doesn't kill the scan.
- `findjar.output.json` — JSON `FindJarOutput` (one object per line) for
  `--output json`. Hand-rolled escaper to avoid adding a dependency.
- `findjar.render` — pure-ish rendering: ANSI coloring, intra-line match
  highlighting, cat block formatting (two-pass streaming for exact
  line-number padding without OOM), grep line formatting.
- `findjar.main` — `-main` entry point and the default `FindJarOutput`
  (`default-output`) plus `quiet-output` for `-q`. `pick-output` chooses
  between default / json / quiet. `-main` flushes `*out*` and calls
  `shutdown-agents` on exit (the latter prevents a 60s JVM hang on the
  agent pool's idle threads after parallel scans).
- `findjar.cli` — `tools.cli` option specs, grouped `--help` summary,
  `validate-args`. Usage and examples text live in
  `resources/findjar/{usage,examples}.txt` so non-coders can edit them.
- `findjar.hash` — streaming hashing helpers (`MessageDigest` + `CRC32`).

### Two extension points (registries)

Both are plain maps; the CLI reads from them at help-build time.

- `findjar.core/hash-algorithms` — `{:md5 {:digest fn :desc "md5"} ...}`.
  `digest` takes an `InputStream` and returns a hex string.
  `findjar.core/hash-by-desc` round-trips between CLI string and keyword.
- `findjar.core/file-finders` — `{"jar" {:scan fn :desc ... :default bool
   :char \j} ...}`. Keys are file extensions (lowercased) or `:default`
  for normal disk files. `:char` is the single-char selector for `-t`.

To add a new hash or file type, `assoc` into the registry map — the CLI
help text picks it up.

### Path semantics

- For files on disk: `--path`/`-p` matches the path **relative** to
  `search-root`; `--apath`/`-a` matches the absolute canonical path
  (the two are mutually exclusive — see `validate-args`).
- For files inside an archive: paths take the form
  `<path-to-jar>@<entry-path-inside-jar>` (constructed in `find-in-jar`).
  Match patterns apply to that combined string.

## Versioning

Version is derived from git: `1.0.<git-rev-count>` (see `build.clj`).
The build writes `gen-resources/build/version.edn` (containing ref,
short ref, version, rev-count, dirty?) and the runtime reads it from
the classpath (`build/version.edn`) when packaged in the uberjar, or
from the working tree when running from source.

## Conventions

- Side effects (printing, file output, throwing on warn) belong in
  implementations of `FindJarOutput` — keep `findjar.core` free of
  `println`/IO. The codebase explicitly calls this out in protocol
  docstrings; preserve that separation when extending.
- ANSI coloring is gated by `findjar.main/*use-colors*` and the
  `--monochrome` opt; rebind the dynamic var rather than threading a flag.
- Tests drive `core/perform-scan` against a fixture tree built by
  `test/findjar/test_fixtures.clj` and assert on calls captured by a
  fake `FindJarOutput` (`test/findjar/recording_output.clj`). Add new
  end-to-end tests in `test/findjar/integration_test.clj`. Kaocha picks
  up the default `:unit` suite from `tests.edn`.
