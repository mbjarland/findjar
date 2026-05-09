# findjar

[![Build Status](https://github.com/mbjarland/findjar/actions/workflows/ci.yml/badge.svg)](https://github.com/mbjarland/findjar/actions)
[![License](https://img.shields.io/badge/License-EPL_2.0-green.svg)](https://www.eclipse.org/legal/epl-2.0/)

`findjar` searches files on disk **and** inside jar/zip archives, with
regex matching for file names, paths, and content. It's an improvement
on the unix `find` command for one specific problem JVM developers hit
constantly: locating a class or source file across a maven repo /
classpath when it might live either directly on disk or inside a jar
(or, with `--nested`, inside a jar inside another jar — Spring Boot
fatjars, Bazel/Pants uberjars, shaded clients).

![alt text](https://raw.githubusercontent.com/mbjarland/findjar/master/doc/findjar_manifest_jdk_created_by.png)

## Highlights

- **Searches archive interiors.** `.jar` and `.zip` are first-class. Use
  `--nested` to also descend into jars *inside* jars.
- **Full regex matching** for file names, paths (relative or absolute),
  and content. Glob mode (`-G '*.clj'`) when you don't want to type
  regex.
- **Grep-like content search** with line numbers, intra-line ANSI
  highlighting, and asymmetric context (`-A` / `-B` / `-x`).
- **Cat matched files**, including jar entries, with line numbers and
  highlighted matches.
- **Five hash algorithms** (md5, sha1, sha256, sha512, crc32). Compute
  several at once, or **search by hash** to find every copy of a known
  file on your classpath.
- **JSON output** (`--output json`) for piping into `jq` or editor
  integrations.
- **Smart defaults** for daily use: searches the current directory if
  no root is given, accepts multiple roots, skips `.git`, `node_modules`,
  `target`, `build`, etc., honors `.gitignore` and `NO_COLOR`, doesn't
  follow symlinks unless asked.
- **Parallel by default**, with `--parallel-jobs N` and `--no-parallel`
  knobs for HDDs and networked filesystems. Output is byte-for-byte
  identical to the serial path.

## Installation / Building

Requirements:
- Java — tested against 11, 17, and 21.
- Clojure CLI — install instructions: <https://clojure.org/guides/install_clojure>

Build the standalone jar:

```bash
clj -T:build uber
```

This produces `target/findjar-<version>-standalone.jar`. Run it directly
or alias it:

```bash
alias findjar='java -jar /path/to/findjar-<version>-standalone.jar'
```

### Native binary (optional, ~30× faster startup)

For a 25–35ms cold start (vs ~750ms for the JVM jar), use a native
binary built via GraalVM.

**Quickest install — Homebrew** (macOS, Linux):

```bash
brew install mbjarland/findjar/findjar
```

This pulls a prebuilt platform-native binary from the latest
[GitHub Release][releases], plus the man page and shell completions.

**From the GitHub Release directly**:

Download the right archive for your platform from [Releases][releases],
extract, and put `findjar` on `$PATH`. Platforms shipped:

  - `findjar-<v>-linux-x64.tar.gz`
  - `findjar-<v>-macos-arm64.tar.gz` (Apple Silicon; Intel macs on
    macOS 11+ run this transparently via Rosetta 2)
  - `findjar-<v>-windows-x64.zip`

[releases]: https://github.com/mbjarland/findjar/releases

**Build locally**:

Install Oracle GraalVM 25 (e.g. `sdk install java 25.0.3-graal`), then:

```bash
GRAALVM_HOME=$HOME/.sdkman/candidates/java/25.0.3-graal \
  clj -T:build native-image
```

Produces a self-contained `target/findjar` (~34MB on macOS arm64). No
JVM needed at runtime. The build takes ~30s. Set `GRAALVM_HOME`,
`NATIVE_IMAGE_HOME`, or `JAVA_HOME` to your Graal install — the build
task picks the first one that has `bin/native-image`.

To produce a release-ready archive (binary + completions + man page):

```bash
clj -T:build package    # → target/findjar-<v>-<platform>.tar.gz
```

The release pipeline at `.github/workflows/release.yml` runs this
step on each platform's runner; see `doc/RELEASING.md` for the
end-to-end process.

### Shell completions

`findjar` ships completion scripts for **zsh**, **bash**, and **fish**,
embedded in the binary itself. Print one with `--completions <shell>`
and pipe it to wherever your shell looks for completion files.

**zsh** — pick any directory on your `$fpath` (run `echo $fpath` to see):
```bash
mkdir -p ~/.zfunc
findjar --completions zsh > ~/.zfunc/_findjar
echo 'fpath=(~/.zfunc $fpath)' >> ~/.zshrc
echo 'autoload -Uz compinit && compinit' >> ~/.zshrc
exec zsh   # or open a new terminal
```

If you have Homebrew, the Homebrew-managed completion directory works
without extra `fpath` setup:
```bash
findjar --completions zsh > "$(brew --prefix)/share/zsh/site-functions/_findjar"
```

**bash** — `bash-completion` v2 looks under
`~/.local/share/bash-completion/completions/` (XDG):
```bash
mkdir -p ~/.local/share/bash-completion/completions
findjar --completions bash > ~/.local/share/bash-completion/completions/findjar
```

System-wide on Linux:
```bash
sudo sh -c 'findjar --completions bash > /etc/bash_completion.d/findjar'
```

**fish**:
```bash
findjar --completions fish > ~/.config/fish/completions/findjar.fish
```

Reload your shell (or `exec $SHELL`) and you can `findjar -<TAB>` to
see flags, `findjar -t <TAB>` for type selectors, etc.

## Usage at a glance

```bash
# Search the current directory for files containing "TODO":
findjar -g TODO

# Glob match against file names:
findjar -G '*.clj'

# Grep across a maven cache, restricted to jar entries, with 1-line context:
findjar ~/.m2 -n clj -g 'Rich Hickey' -t j -x 1

# Print only the paths of matching files (pipe to your editor):
findjar . -g TODO -l | xargs $EDITOR

# Cat a manifest from inside a jar:
findjar ~/.m2 -n MANIFEST.MF -c -t j

# Compute multiple hashes in one go:
findjar ~/.m2 -n string.clj -t j -s sha1 -s md5

# Find every copy of a known file by sha1:
findjar ~/.m2 --find-by-hash sha1:da39a3ee5e6b4b0d3255bfef95601890afd80709

# Recurse into nested archives (uberjars, fatjars):
findjar app.jar --nested -n MANIFEST.MF

# Quiet shell-script mode: exit 0 if any match, 1 otherwise:
if findjar . -n config.edn -q; then echo found; fi

# JSON output for jq:
findjar . -g TODO --output json | jq -s 'group_by(.path)'

# Multiple search roots, parallel-jobs limit, no gitignore:
findjar ~/.m2 ~/.gradle -g 'CVE-' --parallel-jobs 4 --no-gitignore
```

Run `findjar --help` for the full option list and `findjar --examples`
for a richer set of worked examples (including ANSI coloring).

## Output format

Default text output is grep-like and stable for shell scripting:

```
<path>:<line>  <content>           # grep hit
<hex> <algo> <path>                # hash
<<<<<<< <path>                     # cat block start
1  ...content...
>>>>>>>                            # cat block end
```

JSON output (`--output json`) emits one object per line:

```json
{"kind":"match","path":"src/foo.clj"}
{"kind":"grep","path":"src/foo.clj","line":42,"hit?":true,"text":"...","matches":[[4,8]]}
{"kind":"hash","path":"x.jar@y.clj","algo":"sha1","hex":"abc123..."}
```

## Defaults that just do the right thing

- **No search-root → cwd.** `findjar -g foo` works.
- **Multiple roots accepted.** Result paths include the root prefix so
  they're unambiguous.
- **Skipped by default**: `.git`, `.svn`, `.hg`, `.bzr`, `node_modules`,
  `target`, `build`, `.gradle`, `.cpcache`, `.idea`, `.vscode`. Override
  with `--all` or add specifics with repeated `--exclude NAME`.
- **`.gitignore` honored** (best-effort: simple globs, no negation).
  Disable with `--no-gitignore`.
- **Symlinks not followed.** Pass `-L` / `--follow` to follow.
- **Binary files skipped when grepping.** First 8KB sniffed for NUL
  bytes (matches `git grep` heuristic). `--text` forces.
- **`NO_COLOR` env var** disables ANSI coloring. `-m` does the same.
- **Errors go to stderr**, exit non-zero. Help / version / examples go
  to stdout, exit 0.

## License

Eclipse Public License v2.0 — see [LICENSE](LICENSE).

## Author

Matias Bjarland / [mbjarland@gmail.com](mailto:mbjarland@gmail.com)
