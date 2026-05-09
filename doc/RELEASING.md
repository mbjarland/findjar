# Releasing findjar

End-to-end process for cutting a release: tag → cross-platform native
binaries on GitHub Releases → Homebrew formula bump.

## One-time setup

### 1. GitHub Actions secrets

The release workflow uses the auto-provided `GITHUB_TOKEN` to publish
releases. Nothing to configure for the basic flow.

For the optional auto-bump-the-formula step (see "Homebrew tap" below),
you'll need a personal access token with `contents: write` on the tap
repo, stored as `HOMEBREW_TAP_TOKEN` in this repo's secrets.

### 2. Homebrew tap repo

Create a new repo named `homebrew-findjar` under your GitHub account
(the `homebrew-` prefix is mandatory — Homebrew detects taps by name).

```bash
gh repo create mbjarland/homebrew-findjar --public \
  --description "Homebrew tap for findjar"
git clone git@github.com:mbjarland/homebrew-findjar.git
cd homebrew-findjar
mkdir Formula
cp /path/to/findjar/Formula/findjar.rb Formula/
git add Formula/findjar.rb
git commit -m "Initial findjar formula"
git push
```

Users then install with:

```bash
brew install mbjarland/findjar/findjar
```

(The middle component is the tap name without the `homebrew-` prefix.)

## Per-release process

### 1. Cut the release

```bash
# Make sure master is clean and tests pass
git checkout master
git pull
clj -M:test            # 93 tests, 0 failures expected

# Pick a version. Build numbers come from rev-count, so the next tag
# matches whatever 'clj -T:build uber' prints. To see what it'll be:
clj -e '(require (quote build)) (println build/version)'   # → 1.0.130

# Tag and push
git tag v1.0.130
git push origin v1.0.130
```

The push triggers `.github/workflows/release.yml`. The workflow:

1. Builds the native binary on each of `linux-x64`, `macos-arm64`,
   `macos-x64`, `windows-x64` runners (parallel matrix).
2. Packages each binary with the man page, LICENSE, and shell
   completion scripts into `findjar-<version>-<platform>.tar.gz`
   (or `.zip` on Windows).
3. Computes SHA-256 sums and writes `SHASUMS256.txt`.
4. Creates a GitHub Release named after the tag, attaches all four
   archives + the SHASUMS file, and auto-generates release notes
   from the merged-PR list since the previous tag.

Watch progress at:
<https://github.com/mbjarland/findjar/actions>

Native-image builds take 30–60s per runner. Total wall time for the
release: ~2–3 minutes.

### 2. Update the Homebrew formula

Once the release is published, grab the SHA-256 sums from the release
page (`SHASUMS256.txt`) and update `Formula/findjar.rb`:

```ruby
version  "1.0.130"

on_macos do
  on_arm do
    url    "...findjar-1.0.130-macos-arm64.tar.gz"
    sha256 "abcd..."   # from SHASUMS256.txt
  end
  ...
end
```

Then push to the tap repo:

```bash
cd ~/projects/homebrew-findjar
cp ~/projects/clojure/findjar/Formula/findjar.rb Formula/
git add Formula/findjar.rb
git commit -m "findjar 1.0.130"
git push
```

### 3. (optional) Verify

```bash
brew update
brew install --force-bottle mbjarland/findjar/findjar    # or upgrade
findjar --version
```

## Automating the formula bump (later)

To eliminate the manual step in (2), add a job to the release workflow
that:
- Downloads the SHASUMS256.txt
- Patches `Formula/findjar.rb` with the new version + sums
- Pushes to the tap repo using `HOMEBREW_TAP_TOKEN`

Pattern:

```yaml
update-formula:
  needs: release
  if: startsWith(github.ref, 'refs/tags/v')
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
      with:
        repository: mbjarland/homebrew-findjar
        token: ${{ secrets.HOMEBREW_TAP_TOKEN }}
    - name: Bump formula
      run: ./scripts/bump-formula.sh ${{ github.ref_name }}
    - name: Commit & push
      run: |
        git -C tap config user.name 'github-actions[bot]'
        git -C tap config user.email '...'
        git -C tap add Formula/findjar.rb
        git -C tap commit -m "findjar ${{ github.ref_name }}"
        git -C tap push
```

Skipping for now to keep the first release simple.
