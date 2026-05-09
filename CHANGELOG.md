# Changelog

All notable changes to findjar are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project
loosely follows semantic versioning over the rev-count-derived `1.0.X`
build numbers.

## Unreleased

### Added (post-merge to master)

- **Tier 3 features.**
  - **`--manifest`** — for each matched .jar/.zip entry that is
    `META-INF/MANIFEST.MF` or any `pom.properties`, print its
    contents. Lets you `findjar app.jar --manifest --nested` to dump
    every manifest of every fat-jar dependency in one shot.
  - **`--class-info`** — parses `.class` entries via ASM and prints
    the class name, access modifiers, super, implemented interfaces,
    and method signatures. Combines with `--output json` for jq /
    editor pipelines (e.g. *every class that implements
    `Serializable`*). Adds `org.ow2.asm/asm 9.7.1` as a runtime dep.
  - **Tar / tar.gz / tgz support.** New `t` selector in `--types`.
    Hand-rolled minimal tar reader (no commons-compress); GZIPInputStream
    handles the gzip wrapper.
  - **`.gitignore` recursion**: a `.gitignore` in any descendant
    directory contributes patterns for that subtree, matching git's
    behavior.
  - **`.gitignore` negation**: `!pattern` lines re-include
    previously-excluded files; rule order matters, last match wins.
- **File search-roots accepted.** `findjar app.jar --manifest` works:
  a single-file root is treated as a one-element seq instead of being
  rejected with "non-directory search root".
- **GitHub Actions release pipeline** at `.github/workflows/release.yml`.
  Push a `v*` tag → matrix builds the native binary on `linux-x64`,
  `macos-arm64`, `windows-x64` runners; packages each with the man
  page and shell completions; publishes a GitHub Release with
  `SHASUMS256.txt` and auto-generated notes. Intel macs run the
  arm64 binary via Rosetta 2.
- **`clj -T:build package` task** that wraps `native-image` and
  produces a release-ready `target/findjar-<v>-<platform>.tar.gz`
  (or `.zip` on Windows). Layout: `findjar/`, `LICENSE`,
  `man/findjar.1`, `completions/{zsh,bash,fish}`. Honors the
  `TARGET_PLATFORM` env var so the workflow can name archives
  consistently across runners.
- **Homebrew formula** at `Formula/findjar.rb` for shipping via a
  `mbjarland/homebrew-findjar` tap. `doc/RELEASING.md` documents
  the per-release process: tag → workflow runs → bump formula sums.
- **Java 25 in CI matrix.** `.github/workflows/ci.yml` now also
  tests against the current LTS.

- **`findjar --completions <shell>`** prints a shell completion script
  (zsh, bash, or fish) to stdout. Embedded in the binary so users
  installing a single-file native binary have everything they need:
  ```
  findjar --completions zsh  > ~/.zfunc/_findjar          # zsh
  findjar --completions bash > /etc/bash_completion.d/findjar
  findjar --completions fish > ~/.config/fish/completions/findjar.fish
  ```
  Source files live in `resources/findjar/completions/`.

- **Common grep flags** users reach for from muscle memory:
  - **`-v` / `--invert-match`** — emit lines that do NOT match.
  - **`-w` / `--word-regexp`** — wrap the grep pattern in `\b…\b` for
    word-boundary match.
  - **`--count`** — emit only the count of matching lines per file
    (`<path>:<n>` in text mode, `{"kind":"count",...}` in JSON).
  - **`--max-count <n>`** — stop after N matching lines per file.
- **GraalVM native-image build.** New `native-image` task in build.clj.
  Run `clj -T:build native-image` against an Oracle GraalVM 25 install
  (or set `GRAALVM_HOME`). Produces `target/findjar` (~35MB on macOS
  arm64) with ~25x faster startup than the JVM uberjar. Reachability
  metadata captured from the agent lives at
  `resources/META-INF/native-image/findjar/findjar/`.
- **`doc/TODO.md`** — full roadmap of follow-up ideas across five
  tiers from distribution to niche.

### Changed

- **`jansi-clj.auto`** require removed; jansi installation now happens
  explicitly inside `-main` (was at namespace-load time, which
  prevented native-image builds).
- **Type hints** in `findjar.hash` and `findjar.cli/un-whitespace` to
  eliminate runtime reflection — required for native-image.

### Added

- **Default search-root is the current directory.** `findjar -g foo`
  now works without an explicit `.`.
- **Multiple search-roots** are accepted: `findjar ~/.m2 ~/.gradle -n core.clj`.
  Result paths include the root prefix so they're unambiguous between
  roots; `-a` still produces canonical absolute paths.
- **`-l` / `--files-only`** for grep: print one path per matching file
  instead of every matching line.
- **`-A <n>` / `--after <n>`** and **`-B <n>` / `--before <n>`** —
  asymmetric grep context, like `grep`. `-x` remains as the symmetric
  shorthand.
- **`-G <pattern>` / `--glob <pattern>`** — glob match against file
  name as an alternative to `-n` regex.
- **`-q` / `--quiet`** — suppresses output, exit code 0 if any match,
  1 otherwise. Shell-script friendly.
- **`-V` / `--version`** — print version and exit. (Was previously
  buried inside `--help`.)
- **`-L` / `--follow`** — opt-in to following symbolic links during
  traversal. Default is now to **not** follow.
- **`--max-depth <n>`** — limit traversal depth.
- **`--exclude <name>`** (repeatable) — additional directory names to
  skip on top of the defaults.
- **`--all`** — bypass the default directory skips (`.git`,
  `node_modules`, `target`, etc.) plus `.gitignore`.
- **`--no-gitignore`** — bypass `.gitignore` only.
- **`.gitignore` honored by default** at each search-root. Best-effort
  matcher: comments, blank lines, simple globs, directory hints; no
  negation, no recursion into subdirectory `.gitignore` files.
- **Binary files skipped when grepping.** First 8 KB sniffed for NUL
  bytes (matches `git grep`). Use `--text` to force.
- **`--nested`** — recurse into jars/zips that appear as entries inside
  other jars/zips (uberjars, fatjars). Path takes the form
  `outer.jar@inner.jar@entry`.
- **`--find-by-hash <algo>:<hex>`** (repeatable) — find every file whose
  hash equals the given hex. Useful for tracking down which library
  version contains a specific class.
- **`--output json`** — JSON output mode. One object per line; suitable
  for piping into `jq` or for editor / LLM integrations.
- **`--parallel-jobs <n>`** — cap concurrent scan workers. Useful on
  HDDs or networked filesystems where the default `cores+2` over-
  subscribes IO.
- **Hash output now labels its algorithm**: `<hex> <algo> <path>` (was
  `<hex> <path>` before; multi-hash output was unreadable).
- **`NO_COLOR` env var** is honored alongside `-m`.
- **Help is grouped** into Filtering / Action / Output / Scanning / Misc
  sections.

### Changed

- **`--no-parallel`** description corrected: parallel mode uses ~ cores+2
  workers, not "all available cores".
- **Errors go to stderr**, not stdout, so pipelines aren't polluted.
- **Error messages** now appear before the (omitted) help wall, with a
  single "Try 'findjar --help' for more information." hint.
- **`-x 0` allowed**: previously `-x` accepted `0` only by accident; now
  validated as `>= 0`.
- **Zero-byte jars** now produce a warn rather than being silently
  skipped.
- **Unknown regex flag chars** to `-f` now error at parse time rather
  than silently no-op.
- **CLI usage and examples text** moved to `resources/findjar/*.txt` so
  non-coders can edit copy without touching code.
- **Rendering split out** of `findjar.main` into `findjar.render`.
- **Architecture refactor**: `FileContent` callback protocol replaced
  with `with-stream` / `with-reader` helpers; multimethod registries
  replaced with plain map registries (`hash-algorithms`, `file-finders`).

### Fixed

- **`warn` in default-output no longer throws.** Previously the first
  IO error during a scan killed the entire jar's iteration.
- **`name-part` off-by-one** in the JVM-relative-path of jar entries.
- **`file-ext` is now case-insensitive**: `Foo.JAR` is recognized.
- **`valid-file-fn` matches by extension**, not by `String.endsWith` on
  the whole filename (a file called `notajar` was being treated as a
  jar).
- **Multiple search-roots** are no longer silently accepted (they were
  rejected by an inverted `count` comparison).
- **Build timestamp** is now actually written to `version.edn`; previously
  the `--help` output always said the build was "now".
- **`hash.clj` read-is** terminates on `(neg? n)` per the InputStream
  contract instead of looping on a `0` return.
- **Cat output** no longer disappears on uberjar runs: `*out*` is now
  flushed at exit (`(:gen-class)` leaves it as an unbuffered
  `OutputStreamWriter`).
- **Find-line-maps `concat` chain** replaced with `into` to avoid a
  stack overflow on files with many grep hits.
- **`render-cat`** now threads the output sink so stream-open errors
  surface as warnings instead of NPEs.
- **JVM 60-second exit hang** after parallel scans fixed by adding
  `(shutdown-agents)` to `-main`.

### Architecture

- New `findjar.output.buffering` — buffering FindJarOutput + parallel
  scan driver. Workers scan into per-file buffers; replay happens in
  input order against the real output, so parallel and serial output
  are byte-identical.
- New `findjar.output.json` — JSON FindJarOutput.
- New `findjar.render` — coloring, cat block formatting, grep line
  formatting (extracted from `findjar.main`).
- `FileContent` protocol removed; `findjar.protocols` now defines only
  `FindJarOutput`.
- Hash and file-type registries moved from multimethods to plain maps.

## 1.0.x (master prior to refactor)

The pre-refactor lineage. See git history.
