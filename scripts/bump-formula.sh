#!/usr/bin/env bash
# Update Formula/findjar.rb to point at a published release.
#
# Usage: scripts/bump-formula.sh <version>
#   e.g. scripts/bump-formula.sh 1.0.131
#
# Fetches SHASUMS256.txt from the matching GitHub release, extracts the
# sums for each platform asset, and patches the version + sha256 lines
# in Formula/findjar.rb in place. After running, review the diff and
# push the formula to your homebrew-findjar tap repo.

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <version>     e.g. $0 1.0.131" >&2
  exit 1
fi

version="${1#v}"   # accept either '1.0.131' or 'v1.0.131'
tag="v${version}"
repo="${FINDJAR_REPO:-mbjarland/findjar}"
formula="${FINDJAR_FORMULA:-Formula/findjar.rb}"

if [[ ! -f "$formula" ]]; then
  echo "formula not found: $formula" >&2
  echo "run from the project root, or set FINDJAR_FORMULA=/path/to/findjar.rb" >&2
  exit 1
fi

shasums_url="https://github.com/${repo}/releases/download/${tag}/SHASUMS256.txt"
echo "fetching $shasums_url"
shasums=$(curl -sfL "$shasums_url") || {
  echo "failed to fetch SHASUMS256.txt — does the release exist yet?" >&2
  exit 1
}

# Pull out the sum for each platform asset we ship.
sum_for() {
  local platform="$1"
  echo "$shasums" | awk -v p="findjar-${version}-${platform}" '$2 == p {print $1}'
}

sha_arm64=$(sum_for "macos-arm64.tar.gz")
sha_linux=$(sum_for "linux-x64.tar.gz")

for var in sha_arm64 sha_linux; do
  if [[ -z "${!var}" ]]; then
    echo "missing sum for $var in SHASUMS256.txt" >&2
    echo "$shasums" >&2
    exit 1
  fi
done

# Use sed with a tmp suffix for cross-platform compat (BSD vs GNU).
tmp="${formula}.bak"

sed -E "\
s|^(  version  *)\"[^\"]*\"|\1\"${version}\"|; \
s|/v[^/]*/findjar-[^-]+-macos-arm64\.tar\.gz|/${tag}/findjar-${version}-macos-arm64.tar.gz|g; \
s|/v[^/]*/findjar-[^-]+-linux-x64\.tar\.gz|/${tag}/findjar-${version}-linux-x64.tar.gz|g" \
  "$formula" > "$tmp"
mv "$tmp" "$formula"

# Replace each sha256 line that follows the matching url. Keyed by url
# component to avoid mixing up which sum goes where.
patch_sha() {
  local platform="$1" sum="$2"
  python3 - <<PY
import re, sys
path = "$formula"
src = open(path).read()
pat = re.compile(
    r'(url\s+"[^"]*-${version}-${platform}\.tar\.gz"\s*\n\s*sha256\s+")[0-9a-fA-F]+(")',
    re.MULTILINE)
new, n = pat.subn(r'\g<1>${sum}\g<2>', src)
if n != 1:
    print(f"could not patch sha256 for ${platform} (matched {n})", file=sys.stderr)
    sys.exit(1)
open(path, 'w').write(new)
PY
}

patch_sha "macos-arm64" "$sha_arm64"
patch_sha "linux-x64"   "$sha_linux"

echo
echo "updated $formula:"
grep -E "^  version|sha256 " "$formula"
echo
echo "next:"
echo "  git -C \$TAP_REPO checkout master"
echo "  cp $formula \$TAP_REPO/Formula/"
echo "  git -C \$TAP_REPO commit -am \"findjar ${version}\""
echo "  git -C \$TAP_REPO push"
