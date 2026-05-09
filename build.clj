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

(defn uber [_]
  (clean nil)
  (gen-version-file nil)
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

(defn- which
  "Locate cmd on PATH. Returns the absolute path or nil."
  [^String cmd]
  (let [{:keys [exit out]} (b/process {:command-args ["/usr/bin/env" "which" cmd]
                                       :out :capture})]
    (when (zero? exit) (str/trim out))))

(defn- find-native-image-bin []
  (or (some #(let [p (some-> (System/getenv %) (str "/bin/native-image"))]
               (when (and p (.canExecute (jio/file p))) p))
            ["NATIVE_IMAGE_HOME" "GRAALVM_HOME" "JAVA_HOME"])
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