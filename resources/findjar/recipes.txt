# findjar — Recipes

Worked examples for the workflows findjar was built to solve.
`findjar --recipes` prints the same content from inside the binary.

---

## 1. "Where does this `NoSuchMethodError` come from?"

The single most painful Java production bug is "I have two copies of
this class on the classpath and they disagree." findjar's
`--duplicate-classes` walks every `.class` entry in the search roots
(combine with `--nested` for uberjars), groups by FQN, and reports
any class that lives in 2+ jars with a sha1 per copy:

```bash
findjar ~/.m2 -t j --nested --duplicate-classes
```

Output:

```
com.google.common.collect.ImmutableList
  abc123…  guava-31.0.1-jre.jar@com/google/common/collect/ImmutableList.class
  def456…  app.jar@BOOT-INF/lib/old-guava-19.0.jar@…/ImmutableList.class
```

Distinct sha1s = different bytes for the same FQN — usually a shaded
dependency mismatch. Two paths with the SAME sha1 just means the same
library is on disk twice (annoying but not a runtime hazard).

## 2. Vulnerability audit by class hash

You have the sha1 of a known-vulnerable Jackson class. Find every jar
that bundles a byte-identical copy:

```bash
findjar ~/.m2 ~/.gradle ./vendor -t j --nested \
  --find-by-hash sha1:8b86d29c79f3d34d5dba0c50f0c8e6abf6e9b41a
```

Pipe to a SARIF document for ingestion by GitHub Code Scanning:

```bash
findjar /opt/app -t j --nested \
  --find-by-hash sha256:abcd... \
  --output sarif > findjar.sarif
```

## 3. Audit every MANIFEST.MF inside an uberjar

`--manifest-summary` filters each bundled MANIFEST.MF to the curated
allow-list (Main-Class, Implementation-*, Bundle-*, Class-Path,
Created-By, Built-By, …) so you get a one-page audit instead of pages
of CI metadata:

```bash
findjar app.jar --nested --manifest-summary
```

## 4. Extract every config file from a fat jar

```bash
findjar app.jar --nested -G '*.{yml,yaml,xml,properties}' \
  --explode ./extracted-configs
```

The on-disk layout mirrors the archive structure with `@` separators
becoming directories, so `app.jar@BOOT-INF/classes/application.yml`
becomes `./extracted-configs/app.jar/BOOT-INF/classes/application.yml`.

## 5. Find every shaded copy of log4j

```bash
findjar ~/.m2 -t j --nested -n 'log4j' -l
```

`-l` prints just the (outer) jar paths, suitable for piping to
`xargs $EDITOR` or further processing.

## 6. Grep rotated log files in `/var/log`

`-t g` opens `.gz` files as one-entry virtual archives:

```bash
findjar /var/log -t g -g 'OutOfMemoryError' -A 5
```

Use `-A 5` to see the five lines after each match (typical stack-
trace location).

## 7. Spot every class that implements a specific interface

```bash
findjar ~/.m2 -t j --nested --class-info --output json |
  jq 'select(.interfaces? | index("java/io/Serializable")) | .name'
```

`--class-info` parses each `.class` entry via ASM and emits the class
name, super, interfaces, access flags, and method signatures.

## 8. "What's actually on my classpath?"

```bash
findjar ~/.m2/repository ./build/libs ./vendor \
  -t j -n MANIFEST.MF -c --manifest-summary
```

Multiple search roots are accepted; paths are prefixed with their
root so you can tell which classpath entry each result came from.

## 9. Filter by glob without leaving findjar

`--include-glob` and `--exclude-glob` operate on the path relative to
the search-root. Globstar `**` matches any number of directories:

```bash
findjar . -g TODO \
  --exclude-glob 'test/**' \
  --exclude-glob '**/generated/**' \
  --include-glob 'src/**'
```

Filters apply at the filesystem level (they decide whether an archive
is opened). Use `-n` / `-p` for entry-level filtering inside archives.

## 10. xargs-safe path output

When piping path lists to other tools, use `-0` for NUL-separated
output that survives spaces, newlines, and quotes in paths:

```bash
findjar . -g TODO -l -0 | xargs -0 $EDITOR
```

## 11. Quick existence check in shell scripts

Quiet mode with grep-compatible exit codes (0=match, 1=no match,
2=error):

```bash
if findjar . -n config.edn -q; then
  echo "found config"
fi
```

## 12. Diagnostic: "why isn't my file matching?"

`--why-skipped <path>` walks the filter chain and reports the first
rule that excluded the path:

```bash
$ findjar . --why-skipped target/classes/Foo.class
.: ancestor directory 'target' is excluded by the default exclude
   list (--all to bypass)
```

Covers the common causes: default excludes, --types mismatch,
.gitignore stack, symlink-without-follow, --max-depth.
