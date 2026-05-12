(ns build
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.tools.build.api :as b])
  (:import [java.time Duration]
           [java.time.temporal ChronoUnit]))

;; NOTE: to load this build script in an intellij/cursive repl
;; start a nrepl in the terminal with:
;; clj -A:build:nrepl -M -m nrepl.cmdline -p 34567
;; and connect to it using a remote run configuration on port 34567 in cursive

(def lib 'mbjarland/findjar)
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(def start-time (System/currentTimeMillis))
(defn duration-string []
  (let [diff-millis (- (System/currentTimeMillis) start-time)
        duration    (Duration/of diff-millis ChronoUnit/MILLIS)]
    (format "%03d.%03d" (.toSeconds duration) (.toMillisPart duration))))

(defn log [& strs]
  (apply println (str "[" (duration-string) "]") ">" strs))

(defn clean [_]
  (log "cleaning target directory")
  (b/delete {:path "target"}))

(defn gen-version-file [_]
  (let [hash         (b/git-process {:git-args ["rev-parse" "HEAD"]})
        short        (apply str (take 7 hash))
        rev-count    (b/git-count-revs nil)
        status       (b/git-process {:git-args ["status" "--porcelain"]})
        timestamp    (quot (System/currentTimeMillis) 1000)
        version-file "gen-resources/build/version.edn"]
    (log "generating" version-file)
    (b/write-file {:path    version-file
                   :content {:ref       hash
                             :ref-short short
                             :version   version
                             :rev-count rev-count
                             :timestamp timestamp
                             :dirty?    (boolean (seq status))}})))

(b/git-count-revs nil)

(defn- sync-recipes []
  ;; doc/RECIPES.md is the source of truth; resources/findjar/recipes.txt
  ;; is the embedded copy that ships in the binary for `findjar --recipes`.
  ;; Mirror one to the other at build time so they cannot drift.
  (let [src (jio/file "doc/RECIPES.md")
        dst (jio/file "resources/findjar/recipes.txt")]
    (when (.exists src)
      (log "syncing" (.getPath src) "->" (.getPath dst))
      (jio/copy src dst))))

(defn uber [_]
  (clean nil)
  (gen-version-file nil)
  (sync-recipes)
  (log "copying src, resources, and gen-resources")
  (b/copy-dir {:src-dirs   ["src" "resources" "gen-resources"]
               :target-dir class-dir})
  (log "compiling - src ->" class-dir)
  (b/compile-clj {:basis     basis
                  :src-dirs  ["src"]
                  :class-dir class-dir})
  (log "creating uber jar" uber-file)
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis     basis
           :main      'findjar.main})
  (log "build completed!"))

;;;; ---------------------------------------------------------------------------
;;;; native-image — produces a standalone binary for the host platform.
;;;; Requires Oracle GraalVM 25 (or compatible) with native-image installed.
;;;; Set NATIVE_IMAGE_HOME, JAVA_HOME, or have `native-image` on PATH.
;;;; Usage:  clj -T:build native-image

(defn- windows? []
  (str/includes? (str/lower-case (System/getProperty "os.name")) "windows"))

(defn- which
  "Locate cmd on PATH. Returns the absolute path or nil. Uses 'where' on
  Windows, 'which' elsewhere."
  [^String cmd]
  (let [tool (if (windows?) "where" "which")
        {:keys [exit out]} (b/process {:command-args [tool cmd]
                                       :out :capture})]
    (when (zero? exit)
      ;; 'where' may print multiple matches; take the first.
      (-> out str/split-lines first str/trim))))

(defn- native-image-candidates
  "Possible paths to the native-image launcher under a Graal install root.
  On Windows the launcher is native-image.cmd; on POSIX, plain native-image."
  [home]
  (when home
    (if (windows?)
      [(str home "/bin/native-image.cmd") (str home "/bin/native-image")]
      [(str home "/bin/native-image")])))

(defn- find-native-image-bin []
  (or (some (fn [env]
              (some #(when (.canExecute (jio/file %)) %)
                    (native-image-candidates (System/getenv env))))
            ["NATIVE_IMAGE_HOME" "GRAALVM_HOME" "JAVA_HOME"])
      (which (if (windows?) "native-image.cmd" "native-image"))
      (which "native-image")
      (throw (ex-info "native-image not found. Install Oracle GraalVM 25
                       and ensure native-image is on PATH or set GRAALVM_HOME
                       (or JAVA_HOME) to a Graal installation."
                      {}))))

(defn native-image
  "Build a native binary from the uberjar via GraalVM native-image. Produces
  target/findjar (~35MB on macOS arm64). Re-runs the uber task first so the
  binary is always built from a fresh jar."
  [_]
  (uber nil)
  (let [bin  (find-native-image-bin)
        out  "target/findjar"
        args ["--no-fallback"
              ;; Init Clojure runtime + AOT'd findjar at build time so
              ;; cold start doesn't pay namespace-loading costs. tufte's
              ;; macros are similarly safe to snapshot. jansi initialises
              ;; entirely at run time; its install! call lives in -main.
              "--initialize-at-build-time=clojure,findjar,taoensso,jansi_clj,org.fusesource.jansi"
              "--initialize-at-run-time=org.fusesource.jansi.AnsiConsole,org.fusesource.jansi.AnsiPrintStream"
              ;; Bundle every JCA security provider so MessageDigest /
              ;; SecureRandom / etc. work at run time without each algo
              ;; needing a per-class reflection registration.
              "--enable-all-security-services"
              ;; Bundle every resource under findjar/ — usage.txt,
              ;; examples.txt, and completions/{zsh,bash,fish} (no
              ;; extension). The earlier '.txt' glob silently dropped the
              ;; completions, so --completions returned empty when run
              ;; outside the project's resources/ filesystem fallback.
              "-H:IncludeResources=findjar/.*"
              "-H:IncludeResources=build/.*\\.edn"
              "-H:+UnlockExperimentalVMOptions"
              ;; Reachability metadata in resources/META-INF/native-image/
              ;; covers jansi reflection (Ansi.fg/bg, Color enum, etc.)
              ;; that the agent caught during a sample JVM run. -Ob is
              ;; the recommended dev-build mode (skips most optimisation
              ;; passes; final release pipelines can switch to -O2).
              "-Ob"
              "-jar" uber-file
              out]]
    (log "native-image:" bin)
    (let [{:keys [exit]} (b/process {:command-args (cons bin args)})]
      (when (not (zero? exit))
        (throw (ex-info (str "native-image failed (exit " exit ")") {})))
      (log "native binary:" out))))

;;;; ---------------------------------------------------------------------------
;;;; package — bundle the native binary + completions + man page into a
;;;; release-ready tarball (or zip on Windows).

(defn- platform-id
  "Detect host platform for asset naming. Override via TARGET_PLATFORM env."
  []
  (or (System/getenv "TARGET_PLATFORM")
      (let [os   (str/lower-case (System/getProperty "os.name"))
            arch (str/lower-case (System/getProperty "os.arch"))
            os'  (cond (str/includes? os "windows") "windows"
                       (str/includes? os "mac")     "macos"
                       (str/includes? os "linux")   "linux"
                       :else                         os)
            ;; uname -m's "x86_64" → "x64", "aarch64"/"arm64" → "arm64"
            arch' (cond (#{"x86_64" "amd64"} arch) "x64"
                        (#{"aarch64" "arm64"} arch) "arm64"
                        :else arch)]
        (str os' "-" arch'))))

(defn snapshot-docs
  "Regenerate the doc/HELP.txt and doc/EXAMPLES.txt files from the live
  binary. Builds the native binary first (so the snapshots match what
  ships in the next release). README.md links to these so GitHub
  browsers can read them without installing."
  [_]
  (native-image nil)
  (doseq [[flag out-path] [["--help"     "doc/HELP.txt"]
                           ["--examples" "doc/EXAMPLES.txt"]]]
    (log "snapshotting" out-path)
    (let [{:keys [exit out]}
          (b/process {:command-args ["target/findjar" flag "-m"]
                      :out          :capture})]
      (when (not (zero? exit))
        (throw (ex-info (str "snapshot-docs: " flag " failed (exit " exit ")") {})))
      (spit out-path (or out ""))))
  (log "doc snapshots written"))

(defn audit-docs
  "Best-effort coverage check: walk findjar's --help output, extract
  every long flag name, and warn if any is missing from the man
  page. Catches the common drift case where a CLI option was added
  but the hand-edited man page wasn't updated.

  The README intentionally describes most flags by their short form
  (-c, -n, -g, ...) for brevity, so README coverage is left as a
  manual pre-release scan rather than an automated check.

  Does not fail the build — prose drift always needs a human read.
  Run via 'clj -T:build audit-docs' or as part of regen-docs."
  [_]
  (let [bin "target/findjar"
        _   (when-not (.canExecute (jio/file bin))
              (log "audit-docs: building" bin "first")
              (native-image nil))
        {:keys [out]} (b/process {:command-args [bin "--help" "-m"]
                                  :out          :capture})
        ;; Strip groff backslash escapes (\-\- / \-) so '\-\-name' in
        ;; the man page reads as '--name' for the includes? check.
        man-text (str/replace (slurp "man/findjar.1") "\\-" "-")
        ;; Extract long flags from --help output.
        flags    (->> (re-seq #"--[a-z][a-z0-9-]+" (or out ""))
                      (remove #{"--" "--help" "--examples" "--version"
                                "--monochrome"})
                      distinct
                      sort)
        missing  (remove #(str/includes? man-text %) flags)]
    (log "audit-docs: checking" (count flags) "long flags against man/findjar.1")
    (if (seq missing)
      (log "WARN man/findjar.1 missing:" (str/join " " missing))
      (log "ok   man/findjar.1 covers every long flag"))))

(defn regen-docs
  "One-shot pre-release doc regen:
    1. Build the native binary fresh (via snapshot-docs → native-image).
    2. Snapshot doc/HELP.txt and doc/EXAMPLES.txt from --help / --examples.
    3. Audit man page + README for any flag missing from the prose.

  Hand-edited surfaces (man/findjar.1, README.md, resources/findjar/
  *.txt, CHANGELOG.md) are NOT regenerated — those are sources of
  truth. The audit step is a coverage warning, not a build failure.

  Run before 'git tag v...' so the next release ships current docs.
  See doc/RELEASING.md for the full release checklist."
  [_]
  (snapshot-docs nil)
  (audit-docs nil)
  (log "regen-docs: review 'git diff doc/' and CHANGELOG before tagging"))

(defn package
  "Bundle target/findjar (+ man page + completions) into an archive named
  findjar-<version>-<platform>.{tar.gz,zip}. Calls native-image first so
  the binary is fresh."
  [_]
  (native-image nil)
  (let [plat       (platform-id)
        win?       (windows?)
        bin-name   (if win? "findjar.exe" "findjar")
        out-bin    (str "target/" bin-name)
        ;; native-image always emits 'findjar'; rename for windows
        _          (when (and win? (.exists (jio/file "target/findjar")))
                     (jio/copy (jio/file "target/findjar") (jio/file out-bin))
                     (.delete (jio/file "target/findjar")))
        ;; Stage into target/dist/findjar-<v>-<plat>/. The dist parent
        ;; is what we tar/zip — that way the archive always contains a
        ;; single top-level dir (findjar-<v>-<plat>/) regardless of
        ;; platform, instead of dumping files at the archive root on
        ;; Windows.
        dist-name  (str "findjar-" version "-" plat)
        dist-root  "target/dist"
        stage      (jio/file (str dist-root "/" dist-name))
        ext        (if win? "zip" "tar.gz")
        archive    (str "target/" dist-name "." ext)]
    (log "packaging" archive)
    (b/delete {:path dist-root})
    (.mkdirs stage)
    (b/copy-file {:src out-bin :target (str (.getPath stage) "/" bin-name)})
    (.setExecutable (jio/file (str (.getPath stage) "/" bin-name)) true false)
    (b/copy-file {:src "LICENSE" :target (str (.getPath stage) "/LICENSE")})
    (b/copy-dir  {:src-dirs ["man"] :target-dir (str (.getPath stage) "/man")})
    (b/copy-dir  {:src-dirs ["resources/findjar/completions"]
                  :target-dir (str (.getPath stage) "/completions")})
    ;; Build the archive. Linux/macOS shell out to tar (always present
    ;; with gzip support). Windows uses tools.build's b/zip so we don't
    ;; depend on a 'zip' binary being on PATH (windows-latest runners
    ;; don't ship one).
    (b/delete {:path archive})
    (if win?
      (b/zip {:src-dirs [dist-root]
              :zip-file archive})
      (let [{:keys [exit]} (b/process {:command-args ["tar" "-czf"
                                                       (str "../" dist-name "." ext)
                                                       dist-name]
                                        :dir          dist-root})]
        (when (not (zero? exit))
          (throw (ex-info (str "tar failed (exit " exit ")") {})))))
    (log "archive:" archive)))