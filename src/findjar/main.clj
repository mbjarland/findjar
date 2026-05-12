(ns ^{:doc    "Entry point. Default FindJarOutput emits to stdout / out-file.
                Rendering (ANSI, cat blocks, grep formatting) lives in
                findjar.render so this namespace can focus on lifecycle."
      :author "Matias Bjarland"}
  findjar.main
  (:require [clojure.java.io :as jio]
            [findjar.cli :as cli]
            [findjar.core :as c]
            [findjar.output.buffering :as buf]
            [findjar.output.json :as json-out]
            [findjar.protocols :as p]
            [findjar.render :as r]
            [jansi-clj.core :as ansi :refer [red white]]
            [taoensso.tufte :as tufte])
  (:import [java.io File])
  (:gen-class))

;;;; ---------------------------------------------------------------------------
;;;; Default FindJarOutput — emits to *out* (and -o file when configured)

(defn default-output
  "FindJarOutput implementation that prints to *out* (and the -o file when
  set). All ANSI coloring is gated by render/use-colors?."
  []
  (reify p/FindJarOutput
    (warn [_ msg ex opts]
      (binding [r/*use-colors* (r/use-colors? opts)
                *out*          *err*]
        (println (r/style red (str "WARN: " msg)))
        (when (and ex (some? (System/getenv "FINDJAR_DEBUG")))
          (let [^Throwable t ex]
            (.printStackTrace t (java.io.PrintWriter. ^java.io.Writer *err*))))))

    (match [_ path opts]
      ;; --null / -0 swaps newline for NUL so paths with embedded
      ;; whitespace / quotes survive 'xargs -0'.
      (if (:null opts)
        (do (print (str path \u0000)) (.flush *out*))
        (println path)))

    (grep-match [_ max-line-# line-map opts]
      (binding [r/*use-colors* (r/use-colors? opts)]
        (println (r/format-grep-line max-line-# line-map opts))))

    (grep-count [_ path n _opts]
      (println (str path ":" n)))

    (class-info [_ path info opts]
      (binding [r/*use-colors* (r/use-colors? opts)]
        (println (r/format-class-info path info))))

    (dump-stream [_ path materialized opts]
      (when materialized
        (if-let [^File of (:out-file opts)]
          (with-open [w (jio/writer of :append true)]
            (.write w ^String materialized)
            (println path ">>" (.getPath of)))
          ;; println (not print) preserves master's trailing blank line
          ;; between consecutive cat blocks — scripts may rely on it.
          (println materialized))))

    (print-hash [_ path hash-type hash-value opts]
      (binding [r/*use-colors* (r/use-colors? opts)]
        (println (r/style white hash-value)
                 (name hash-type)
                 path)))))

;;;; ---------------------------------------------------------------------------
;;;; Silent output (-q): suppresses every call. Match-tracking is the
;;;; wrapper's job — see match-tracking-output below.

(defn- silent-output []
  (reify p/FindJarOutput
    (warn        [_ _ _ _]   nil)
    (match       [_ _ _]     nil)
    (grep-match  [_ _ _ _]   nil)
    (grep-count  [_ _ _ _]   nil)
    (class-info  [_ _ _ _]   nil)
    (dump-stream [_ _ _ _]   nil)
    (print-hash  [_ _ _ _ _] nil)))

;;;; ---------------------------------------------------------------------------
;;;; Match-tracking wrapper: counts how many match-producing calls fired
;;;; against the wrapped sink. -main reads the count via match-count to pick
;;;; the grep-compatible exit code (0 if any matched, 1 if not) and to feed
;;;; the --stats summary.

(defn- match-tracking-output [delegate]
  (let [hits (atom 0)]
    (with-meta
      (reify p/FindJarOutput
        (warn        [_ msg ex opts]   (p/warn        delegate msg ex opts))
        (match       [_ path opts]     (swap! hits inc)
                                       (p/match       delegate path opts))
        (grep-match  [_ max-# m opts]  (swap! hits inc)
                                       (p/grep-match  delegate max-# m opts))
        (grep-count  [_ path n opts]   (swap! hits inc)
                                       (p/grep-count  delegate path n opts))
        (class-info  [_ path i opts]   (swap! hits inc)
                                       (p/class-info  delegate path i opts))
        (dump-stream [_ path s opts]   (swap! hits inc)
                                       (p/dump-stream delegate path s opts))
        (print-hash  [_ p t v opts]    (swap! hits inc)
                                       (p/print-hash  delegate p t v opts)))
      {::hits hits})))

(defn- match-count [out]
  (-> out meta ::hits deref))

(defn- saw-match? [out] (pos? (match-count out)))

;;;; ---------------------------------------------------------------------------
;;;; Entry point

(defn- pick-output [opts]
  (let [fmt (:output opts)]
    (match-tracking-output
      (cond
        (:quiet opts)              (silent-output)
        (= :json-array fmt)        (json-out/json-array-output)
        (#{:json :ndjson} fmt)     (json-out/json-output)
        :else                      (default-output)))))

(defn- run-scan [search-roots opts]
  (let [output       (pick-output opts)
        json-array?  (= :json-array (:output opts))
        stats?       (:stats opts)
        examined     (atom 0)
        start-ms     (System/currentTimeMillis)
        scan         (if (false? (:parallel opts))
                       c/perform-scan
                       buf/parallel-scan)
        ;; With multiple roots, include the root prefix in path output so
        ;; results are unambiguous between roots. Single root keeps the
        ;; existing relative-from-root behaviour. --apath always wins.
        opts         (cond-> opts
                       (and (< 1 (count search-roots))
                            (not (:apath opts)))
                       (assoc :include-root? true)
                       stats?
                       (assoc :examined-counter examined))]
    ;; json-array prologue: open bracket BEFORE the scan so the output is
    ;; valid JSON even when nothing matches. The output sink emits commas
    ;; between records; we close the bracket after the scan.
    (when json-array? (print "["))
    (doseq [root search-roots]
      (scan root output r/render-cat opts))
    (when json-array? (println "]"))
    (when stats?
      (let [elapsed-s (/ (- (System/currentTimeMillis) start-ms) 1000.0)]
        (binding [*out* *err*]
          (println
            (format "findjar: examined %d path(s), emitted %d hit(s) in %.2fs"
                    @examined (match-count output) elapsed-s)))))
    (saw-match? output)))

(defn main-entrypoint
  "Shared by -main and repl-main. hard-exit-on-errors? controls whether bad
  CLI args call System/exit.

  Exit-code convention (grep-compatible):
    0  at least one match was emitted
    1  no match
    2  bad CLI args or other pre-scan error

  hard-exit-on-errors? false means we return the code instead of calling
  System/exit, so a REPL session can drive main-entrypoint without dying."
  [hard-exit-on-errors? args]
  ;; FORCE_COLOR forces ANSI passthrough regardless of TTY detection.
  ;; Useful for tools (freeze / asciinema / svg-term / etc.) that
  ;; capture stdout for rendering and want the escape codes preserved.
  ;; NO_COLOR / -m still win — those come into play at the
  ;; *use-colors* layer below jansi.
  ;;
  ;; Must be set BEFORE (ansi/install!) — jansi reads jansi.mode at
  ;; install time to decide whether AnsiPrintStream strips or passes
  ;; through ANSI escapes.
  (when-let [v (System/getenv "FORCE_COLOR")]
    (when (and (not= "" v) (not= "0" v))
      (System/setProperty "jansi.mode" "force")))
  ;; Install jansi at runtime (was previously done at namespace-load time
  ;; via jansi-clj.auto, which left an AnsiPrintStream in the native-image
  ;; build heap). On Unix this is roughly a passthrough; on Windows it
  ;; translates ANSI escapes to Win32 console calls.
  (ansi/install!)
  (tufte/add-basic-println-handler! {})
  (let [{:keys [search-roots opts exit-message ok?]} (cli/validate-args args)
        profile? (:profile opts)]
    (cond
      exit-message
      ;; ok? true means --help / --version / --examples / --completions
      ;; printed something informational and we should exit 0. ok? false
      ;; means bad args / nonexistent search root etc., which is exit 2
      ;; under grep semantics.
      (let [code (if ok? 0 2)]
        (if hard-exit-on-errors?
          (cli/exit code exit-message)
          (do (println "would exit with code" code "msg," exit-message)
              code)))

      :else
      (let [matched? (tufte/profile {:when profile? :nmax 10000000}
                                    (run-scan search-roots opts))]
        (if matched? 0 1)))))

(defn -main [& args]
  (let [status (try
                 (main-entrypoint true args)
                 (finally
                   ;; Flush *out* explicitly: under (:gen-class), Clojure's
                   ;; *out* is an unbuffered OutputStreamWriter, but its
                   ;; CharsetEncoder buffer holds up to 8KB that JVM exit
                   ;; will not drain on its own. Without this, large -c
                   ;; (cat) blocks silently disappear.
                   (.flush *out*)
                   ;; pmap / futures use the agent pool, whose non-daemon
                   ;; idle threads would otherwise pin the JVM open for
                   ;; 60s after the scan completes.
                   (shutdown-agents)))]
    (when (and (integer? status) (not (zero? status)))
      (System/exit status))))

(defn repl-main [& args]
  (main-entrypoint false args))
