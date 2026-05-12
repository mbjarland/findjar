# Homebrew formula for findjar. Drop this file into a tap repo
# (typically mbjarland/homebrew-tap) and users can install via:
#
#   brew install mbjarland/tap/findjar
#
# Or, once published in homebrew-core, just:
#
#   brew install findjar
#
# After each tagged release, the URLs and sha256 digests below need to
# be bumped to point at the new release assets. The release workflow
# at .github/workflows/release.yml emits a SHASUMS256.txt file
# alongside the archives — copy from there.

class Findjar < Formula
  desc     "Search files and the contents of jar/zip archives"
  homepage "https://github.com/mbjarland/findjar"
  version  "1.0.176"   # bump per release
  license  "EPL-2.0"

  # We ship one macOS asset (arm64). Intel macs run the arm64 binary
  # via Rosetta 2 — Homebrew handles this transparently for the user.
  on_macos do
    url    "https://github.com/mbjarland/findjar/releases/download/v1.0.176/findjar-1.0.176-macos-arm64.tar.gz"
    sha256 "515c2335d948d398467a8d635adbfc56804b404460c428cc94b718c3ab89ccce"
  end

  on_linux do
    on_intel do
      url    "https://github.com/mbjarland/findjar/releases/download/v1.0.176/findjar-1.0.176-linux-x64.tar.gz"
      sha256 "50cb6271ccf768da67a83fb5e7f4dd41334fc95c213d89fa71be8363fbb151e0"
    end
  end

  def install
    # The release archive lays out:
    #   findjar-<v>-<platform>/
    #     findjar              (binary, executable)
    #     LICENSE
    #     man/findjar.1
    #     completions/{zsh,bash,fish}
    bin.install "findjar"

    # Man page
    man1.install "man/findjar.1"

    # Shell completions — Homebrew's helpers know the right paths.
    bash_completion.install "completions/bash" => "findjar"
    zsh_completion.install  "completions/zsh"  => "_findjar"
    fish_completion.install "completions/fish" => "findjar.fish"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/findjar --version")
    # The default-cwd-is-. behaviour means a bare invocation should not
    # error out and should at minimum not crash.
    system bin/"findjar", "--examples"
    # Quiet mode against an empty dir should exit 1 (no match).
    (testpath/"sub").mkpath
    assert_equal 1, shell_output("#{bin}/findjar #{testpath} -n nothing -q -m; echo $?", 0).to_i
  end
end
