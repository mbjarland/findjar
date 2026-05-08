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

Six namespaces under `src/findjar/` (+ `output/`). Side effects are pushed
to the edges via the `FindJarOutput` protocol, so the core scanning logic
is pure-ish and pluggable.

- `findjar.protocols` — defines `FindJarOutput` (sink for `warn`, `match`,
  `grep-match`, `dump-stream`, `print-hash`). All side effects flow
  through this protocol.
- `findjar.core` — the scanner. `perform-scan` walks `file-seq`, applies
  the file-type filter, dispatches to the registered finder for each file,
  and runs `handle-match` which checks `--name`/`--path`/`--apath`/`--grep`
  in turn and then either reports a path match, runs `grep-stream`, hashes,
  or hands a materialized cat string to the output sink. Sliding-window
  context lines for grep are built in `find-line-maps-with-context` +
  `dedupe-line-maps`. Stream lifecycles are managed by `with-stream` /
  `with-reader` helpers (taking a 0-arg `stream-factory` fn) so the same
  code path covers disk files and ZipEntry streams.
- `findjar.output.buffering` — `buffering-output` records protocol calls
  into a vector; `parallel-scan` does pmap over candidate files into
  per-file buffers and replays them against the real output in input order.
  Output is byte-identical to the serial path; throughput improves on
  IO-bound scans of many archives.
- `findjar.main` — the `-main` entry point and the default
  `FindJarOutput` implementation (`default-output`). This is the only
  place that prints to stdout / writes the `-o` out-file. It also owns
  ANSI coloring (`*use-colors*` dynamic var, `style`, `highlight-matches`)
  and cat rendering (`render-cat`), which is injected into core via a
  function argument so core stays free of presentation concerns.
  `-main` calls `shutdown-agents` on exit so the JVM doesn't pin open for
  60s on the agent pool's idle threads.
- `findjar.cli` — `tools.cli` option specs, help/usage/examples text, and
  `validate-args`. Help text is built from the registries below so adding
  a hash type or file type automatically updates `--help`.
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
