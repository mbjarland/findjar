# Building and releasing findjar

End-to-end documentation of the build pipeline and the per-release flow.
Skip to **[Cutting a release](#cutting-a-release)** if you just want the
recipe.

## Overview

```
┌────────────────────────────────────────────────────────────────────┐
│  developer                                                         │
│  ────────                                                          │
│   git tag v1.2.3                                                   │
│   git push origin v1.2.3                                           │
└──────────┬─────────────────────────────────────────────────────────┘
           │
           ▼
┌────────────────────────────────────────────────────────────────────┐
│  GitHub Actions (.github/workflows/release.yml)                    │
│  ─────────────                                                     │
│   • Matrix build on linux-x64 / macos-arm64 / windows-x64          │
│   • Each runner: setup-graalvm@v1 → setup-clojure → build pkg      │
│   • Release job: download all artifacts → SHASUMS256.txt           │
│   • Publish GitHub Release (auto notes from merged PRs since v...) │
└──────────┬─────────────────────────────────────────────────────────┘
           │
           ▼
┌────────────────────────────────────────────────────────────────────┐
│  developer                                                         │
│  ────────                                                          │
│   scripts/bump-formula.sh 1.2.3                                    │
│   cp Formula/findjar.rb $TAP_REPO/Formula/                         │
│   git -C $TAP_REPO commit -am "findjar 1.2.3" && git push          │
└──────────┬─────────────────────────────────────────────────────────┘
           │
           ▼
┌────────────────────────────────────────────────────────────────────┐
│  user                                                              │
│  ────                                                              │
│   brew install mbjarland/findjar/findjar                           │
└────────────────────────────────────────────────────────────────────┘
```

Three repos are involved:

| repo | purpose |
|---|---|
| `mbjarland/findjar` | source, build pipeline, Formula source-of-truth |
| `mbjarland/homebrew-findjar` | Homebrew tap (just `Formula/findjar.rb`) |
| GitHub Releases on `findjar` | hosts the per-platform binary archives |

---

## Local build tasks

`build.clj` exposes four tasks. All run via `clj -T:build <task>`.

### `clean`
Wipes `target/`. Standard.

### `uber`
Builds `target/findjar-<version>-standalone.jar` — the JVM-runnable
fat jar. AOT-compiles `findjar.main`, embeds resources (help text,
examples, completions), and writes `gen-resources/build/version.edn`
from git metadata. Run time: ~5–10s after first JIT warm-up.

### `native-image`
Calls `uber`, then runs GraalVM `native-image` against the jar to
produce `target/findjar` — a self-contained native binary with ~25ms
cold start (vs ~750ms for the JVM jar).

Requires Oracle GraalVM 25 (or compatible) with `native-image` on
`PATH` or one of `NATIVE_IMAGE_HOME` / `GRAALVM_HOME` / `JAVA_HOME`
pointing at a Graal install.

```bash
sdk install java 25.0.3-graal              # one-time
GRAALVM_HOME=$HOME/.sdkman/candidates/java/25.0.3-graal \
  clj -T:build native-image
```

The task hard-codes a few native-image flags; see the source for the
rationale on each.

Build time: ~30s after the first run (the GraalVM JIT warms up the
analysis phase).

### `package`
Calls `native-image`, then bundles the resulting binary with
`LICENSE`, `man/findjar.1`, and `resources/findjar/completions/{zsh,
bash,fish}` into a release-ready archive named
`target/findjar-<version>-<platform>.{tar.gz,zip}`.

Layout inside the archive:

```
findjar-1.0.135-macos-arm64/
├── findjar               (executable)
├── LICENSE
├── completions/
│   ├── bash
│   ├── fish
│   └── zsh
└── man/
    └── findjar.1
```

`zip` is used on Windows (via `clojure.tools.build.api/zip`, no
shell-out so no `zip` binary needed); `tar.gz` everywhere else.

`platform-id` autodetects the host's `os/arch`, but the env var
`TARGET_PLATFORM` overrides — that's how the GH Actions matrix
ensures consistent asset names across runners.

```bash
clj -T:build package           # macOS arm64 → findjar-X-macos-arm64.tar.gz
TARGET_PLATFORM=linux-x64 \
  clj -T:build package         # → findjar-X-linux-x64.tar.gz
```

---

## Release pipeline

`.github/workflows/release.yml` drives the cross-platform release.

**Trigger:** push of any tag matching `v*` (e.g., `v1.0.135`).
A `workflow_dispatch` event is also wired in so the workflow can be
smoke-tested manually from the Actions UI without cutting a real
release; the publish step is gated on `startsWith(ref, 'refs/tags/v')`,
so manual runs upload artifacts but skip the Release.

**Matrix jobs** (parallel):

| runner | platform asset |
|---|---|
| `ubuntu-latest`  | `findjar-<v>-linux-x64.tar.gz`   |
| `macos-latest`   | `findjar-<v>-macos-arm64.tar.gz` |
| `windows-latest` | `findjar-<v>-windows-x64.zip`    |

Each runner:
1. `actions/checkout@v4` with `fetch-depth: 0` so `b/git-count-revs`
   sees the full history (default shallow clone returns 1, breaking
   the version).
2. `graalvm/setup-graalvm@v1` with `distribution: 'graalvm'`,
   `java-version: '25'`. Sets `GRAALVM_HOME` env.
3. `DeLaGuardo/setup-clojure@13.0`.
4. `actions/cache@v4` for `~/.m2`, `~/.gitlibs`, `~/.deps.clj`.
5. `clojure -T:build package` with `TARGET_PLATFORM` set from the
   matrix entry. `find-native-image-bin` knows about
   `bin/native-image.cmd` on Windows.
6. `actions/upload-artifact@v4` uploads the archive.

**Publish job** (after all matrix entries succeed):
1. Downloads all artifacts via `actions/download-artifact@v4` with
   `merge-multiple: true`.
2. Generates `SHASUMS256.txt` (`shasum -a 256 findjar-* >> ...`).
3. `softprops/action-gh-release@v2` with
   `generate_release_notes: true` and the archives + sums file.

Total wall time: ~3–5 minutes for the whole pipeline.

**Note on Intel macOS**: GitHub deprecated `macos-13` runner
availability; jobs targeting them now sit in `queued` indefinitely.
We ship arm64 only for macOS — Intel mac users on macOS 11+ run the
arm64 binary via Rosetta 2 transparently (Homebrew handles this).

---

## Cutting a release

Once the one-time setup below is done, every release looks like this:

### 1. Tag and push

```bash
cd ~/projects/clojure/findjar
git checkout master
git pull
clj -M:test                # sanity-check, expect 0 failures

# Pick the version. build.clj derives it as 1.0.<git-rev-count>;
# the next tag should match what 'clj -T:build uber' would print.
git tag v1.0.136 -m "findjar 1.0.136"
git push origin v1.0.136
```

The push triggers `release.yml`. Watch progress:

```bash
gh run list --workflow=release.yml --branch v1.0.136 --limit 1
gh run watch <run-id>            # block until done
# or
open https://github.com/mbjarland/findjar/actions
```

Expected outcome: a green run with 4 jobs (3 build + 1 publish), and
a published Release at `https://github.com/mbjarland/findjar/releases/tag/v1.0.136`
containing 3 archives + `SHASUMS256.txt`.

### 2. Bump the Homebrew formula

```bash
scripts/bump-formula.sh 1.0.136
```

The script downloads `SHASUMS256.txt` from the release, then patches
`Formula/findjar.rb` in place (version + per-platform `url` + `sha256`).
Review the diff:

```bash
git diff Formula/findjar.rb
```

### 3. Push the formula to the tap repo

```bash
TAP=~/projects/homebrew-findjar     # or wherever you cloned it
cp Formula/findjar.rb "$TAP/Formula/"
git -C "$TAP" commit -am "findjar 1.0.136"
git -C "$TAP" push
```

Within seconds, anyone running `brew upgrade findjar` (or first-time
`brew install mbjarland/findjar/findjar`) gets the new version.

### 4. Optional verify

```bash
brew update
brew upgrade findjar
findjar --version       # should print '1.0.136 - <sha> - <date>'
```

---

## One-time setup

Already done for the current repo — recorded here so a fresh fork can
reproduce it.

### GitHub Actions

The workflow uses the auto-provided `GITHUB_TOKEN` (via the workflow's
`permissions: contents: write` block) to publish releases. Nothing to
configure.

### Homebrew tap repo

`mbjarland/homebrew-findjar` was created as a public repo with the
mandatory `homebrew-` prefix in its name (Homebrew detects taps by
that prefix). The tap holds a single `Formula/findjar.rb` plus a
README pointing at the main repo.

```bash
gh repo create mbjarland/homebrew-findjar --public \
  --description "Homebrew tap for findjar"
git clone git@github.com:mbjarland/homebrew-findjar.git
cd homebrew-findjar
mkdir Formula
cp /path/to/findjar/Formula/findjar.rb Formula/
git add Formula/findjar.rb && git commit -m "Initial tap" && git push
```

Anyone then installs via:

```bash
brew install mbjarland/findjar/findjar
```

(The `mbjarland/findjar` middle component is the tap name with the
`homebrew-` prefix stripped, then `findjar` again is the formula
inside the tap.)

---

## Lessons / gotchas

These are the issues we hit during the first releases and how each is
addressed:

### Shallow checkout breaks versioning

`actions/checkout@v4` defaults to `fetch-depth: 1`. `build.clj`'s
version is `1.0.<git-rev-count>`, computed from
`git rev-list --count HEAD`. On a shallow checkout that returns 1,
producing assets named `findjar-1.0.1-*`. Both jobs in `release.yml`
explicitly set `fetch-depth: 0`.

### Windows native-image launcher

Oracle GraalVM on Windows installs the launcher as
`bin/native-image.cmd`, not `bin/native-image`. `find-native-image-bin`
in `build.clj` checks both file names on Windows (in that order) before
falling back to a `where`/`which` PATH lookup.

### `zip` not on PATH on Windows runners

`windows-latest` runners don't ship a `zip` binary. The `package` task
uses `clojure.tools.build.api/zip` (pure Java) on Windows, shelling
out to `tar` only on POSIX systems where it's always present.

### Intel macOS runner availability

GitHub deprecated `macos-13` Intel runners. Jobs queueing for them
sit indefinitely. We dropped that platform from the matrix; users on
Intel macs run the arm64 binary via Rosetta 2 (transparent through
Homebrew).

### jansi clinit traps native libs in the build heap

`jansi-clj.auto` calls `AnsiConsole/systemInstall` at namespace-load
time, creating an `AnsiPrintStream` over `System.out`. Native-image
can't snapshot that into the build heap. We removed that require
entirely; `findjar.main/-main` calls `(jansi-clj.core/install!)`
explicitly at runtime.

### Reflection silently breaks at runtime in native-image

Clojure falls back to `clojure.lang.Reflector` for un-hinted method
calls, which fails in a native image without explicit reflection
config. Type hints in `findjar.hash` and `findjar.cli/un-whitespace`
plus a tiny `resources/META-INF/native-image/findjar/findjar/reachability-metadata.json`
covering jansi's `Ansi`/`Ansi$Color` and Clojure's `RT`/`Var`/`Symbol`/
`Keyword`/`Reflector` cover what we need. `--enable-all-security-services`
in the native-image flags handles `MessageDigest.getInstance`.

### Resource glob must match every embedded file

The `-H:IncludeResources` flag uses regex matching. An early version
matched only `findjar/.*\.txt`, silently dropping the (extension-less)
completion scripts at `findjar/completions/{zsh,bash,fish}` from the
binary. The current glob is `findjar/.*` plus `build/.*\.edn` for the
version manifest.

---

## Possible future automation

Currently the per-release steps 2 & 3 are manual. They could be
automated via a follow-up job in `release.yml`:

```yaml
update-formula:
  needs: release
  if: startsWith(github.ref, 'refs/tags/v')
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4   # main repo, for scripts/bump-formula.sh
      with: { fetch-depth: 0 }
    - uses: actions/checkout@v4   # tap repo, into a subdir
      with:
        repository: mbjarland/homebrew-findjar
        path: tap
        token: ${{ secrets.HOMEBREW_TAP_TOKEN }}
    - run: |
        scripts/bump-formula.sh ${GITHUB_REF_NAME#v}
        cp Formula/findjar.rb tap/Formula/
        cd tap
        git config user.name 'github-actions[bot]'
        git config user.email '41898282+github-actions[bot]@users.noreply.github.com'
        git commit -am "findjar ${GITHUB_REF_NAME}"
        git push
```

This requires a `HOMEBREW_TAP_TOKEN` repo secret — a fine-grained PAT
with `Contents: write` scoped only to the tap repo. Keeping it manual
for now is fine while the release cadence is low.
