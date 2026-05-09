# findjar — Roadmap / TODO

Consolidated list of follow-up ideas captured across the recent reviews.
Items are tiered by ROI; pick from any tier independently.

---

## Tier 1 — Distribution (biggest UX leap)

The tool is feature-complete for its niche; the next-biggest improvement
is making it **instant to start** and **one command to install**.

- [x] **GraalVM native-image build.** Done. `clj -T:build native-image`
      against Oracle GraalVM 25 (`25.0.3-graal`) produces a 34MB
      `target/findjar` with ~22ms startup (vs ~780ms for the JVM jar,
      a 35× speedup on the same hardware). `resources/META-INF/native-image/`
      ships a small targeted reflection config for jansi + Clojure
      runtime; `--enable-all-security-services` covers MessageDigest /
      JCA. Caveats remain: jansi-clj.auto can't be required (its
      install-at-clinit traps the AnsiPrintStream in the build heap —
      we now install jansi from -main), and the native-image agent's
      full reachability metadata caused build-time deadlocks under
      analysis, so we hand-trimmed the config.
- [x] **Homebrew formula** at `Formula/findjar.rb`. Drop into a
      `homebrew-findjar` tap repo (one-time setup documented in
      `doc/RELEASING.md`). Pulls the prebuilt binary from each
      release, installs binary + man page + completions.
- [x] **GitHub Actions release pipeline** at
      `.github/workflows/release.yml`. Tag push (`v*`) triggers
      matrix build on `linux-x64` / `macos-arm64` / `macos-x64` /
      `windows-x64` runners. Each builds the native binary via
      `clj -T:build package`, archives it with completions + man
      page, attaches to a GH Release with auto-generated notes and
      a `SHASUMS256.txt` sidecar. Builds the formula bump can use.
- [x] **Bash / zsh / fish completions.** Embedded in the binary; users
      get them via `findjar --completions <shell>` and pipe to the
      right path. Source lives at `resources/findjar/completions/`.
      Cover every flag with appropriate value completion (algorithms,
      types, output formats, file paths).

## Tier 2 — Common grep flags users will reach for

High-frequency, small additions, no risk:

- [x] **`-v` / `--invert-match`** — print non-matching lines.
- [x] **`-w` / `--word-regexp`** — match only at word boundaries.
- [x] **`--count`** — number of matches per file.
- [x] **`--max-count <n>`** — stop after N matches per file.

## Tier 3 — Functional expansions

Bigger lifts but high payoff for findjar's "JVM tooling" niche:

- [x] **Tar / tar.gz / tgz support.** Entry `t` in `--types`. Hand-
      rolled minimal tar reader (USTAR + GNU long-name extension);
      GZIPInputStream for the gzip wrapper. .tar.xz / .tar.bz2 not
      yet — they'd need a real dep.
- [x] **MANIFEST.MF / pom.properties auto-extract.** `--manifest`
      cats every matched `META-INF/MANIFEST.MF` or `pom.properties`
      entry without needing the user to know the exact path.
- [x] **Class-file aware mode.** `--class-info` parses `.class`
      entries via ASM (`org.ow2.asm/asm` dep), emits class name, access
      modifiers, super, interfaces, methods. Plays nicely with
      `--output json` for jq / editor pipelines.
- [x] **`.gitignore` recursion.** Per-directory matchers stacked on
      descent in walk-tree; each `.gitignore` applies only to its
      subtree.
- [x] **`.gitignore` negation (`!pattern`).** Last-match-wins
      semantics (a `!keep.log` re-includes a file otherwise matched
      by `*.log`).

## Tier 4 — Polish

- [ ] **`--unordered` for parallel scan.** Today parallel-scan replays
      in input order so output is deterministic, but huge scans block
      until earlier files complete. `--unordered` would let workers emit
      ASAP. Useful for `findjar ~/.m2 -g foo --unordered | head`.
- [ ] **Recipe / cookbook doc.** A `doc/RECIPES.md` with worked
      solutions: "find every jar that depends on log4j", "spot duplicate
      classes across uberjars", "license audit", "compare two clojure
      versions". Adoption + SEO win.
- [ ] **Performance benchmark suite.** `criterium` against a fixed
      fixture; CI tracks regressions per commit. Cheap insurance.
- [ ] **Property tests.** Generative tests for `walk-tree`,
      `gitignore-line->regex`, `match-idxs`, `compile-glob`. Catches
      edge cases the integration suite misses.
- [ ] **Color customization** via env var
      (`FINDJAR_COLORS=match=cyan,path=blue`). Niche.

## Tier 5 — Lower priority / niche

- [ ] **`--max-time <sec>`** — bail out after N seconds.
- [ ] **`--max-results <n>`** — stop after N total matches across all files.
- [ ] **Compressed-file search** — gzipped log files (`.gz`/`.bz2`/`.xz`).
- [ ] **`-r` / `--recursive` / `--no-recursive`** — explicit depth-0 mode.
      Today always recursive (with `--max-depth 0` as the workaround).
- [ ] **`--path-style relative|absolute|root-relative`** — explicit
      replacement for the inferred `-a` / `--include-root?` logic.
- [ ] **Watch mode** (`--watch`) — re-run on filesystem changes. Niche.
- [ ] **Better Windows support** — file paths use `/` in archive entries
      but `\` on Windows disk. Probably mostly works but edge cases.
- [ ] **Memory-mapped jar reading** for huge jars. ZipFile already
      uses random access; might be a no-op.

## Tier 6 — Done (recent)

For reference of what we already shipped on `feature/cleanup-and-parallelize`
(now merged to master):

- Architecture: protocol → registry refactor, render extraction, buffering
  output, parallel-scan, byte-for-byte master parity.
- Bug fixes: warn-no-throw, name-part, file-ext, multi-search-root, build
  timestamp, hash.read-is, *out* flush, stack-overflow concat, OOM render-cat.
- CLI: default cwd, multiple roots, `-l`, `--all`, `-q`, `-V`, `-L`,
  `-A`/`-B`, `-G`, `--max-depth`, `--exclude`, `--no-gitignore`, `--text`,
  `--nested`, `--find-by-hash`, `--output json`, `--parallel-jobs`,
  `--no-parallel`, `NO_COLOR` env.
- Defaults: skip `.git`/`node_modules`/`target`/`build`/`.gradle`/`.cpcache`/
  `.idea`/`.vscode`/`.svn`/`.hg`, honor `.gitignore`, don't follow
  symlinks, skip binary files when grepping, hash output labels algorithm,
  errors to stderr.
- Help: grouped sections, accurate usage line, errors lead with the
  message not the help wall.
- Docs: README rewrite, CHANGELOG, man page, CLAUDE.md.
- Tests: 86 tests, 225 assertions, fixture-based integration suite.
