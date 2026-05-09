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

    (match [_ path _opts]
      (println path))

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
;;;; Quiet output: suppresses all emission and tracks "did anything match?"
;;;; so -main can pick a 0/1 exit code.

(defn- quiet-output []
  (let [matched? (atom false)]
    (with-meta
      (reify p/FindJarOutput
        (warn        [_ _ _ _]   nil)
        (match       [_ _ _]     (reset! matched? true))
        (grep-match  [_ _ _ _]   (reset! matched? true))
        (grep-count  [_ _ _ _]   (reset! matched? true))
        (class-info  [_ _ _ _]   (reset! matched? true))
        (dump-stream [_ _ _ _]   (reset! matched? true))
        (print-hash  [_ _ _ _ _] (reset! matched? true)))
      {::matched? matched?})))

(defn- saw-match? [out]
  (-> out meta ::matched? deref))

;;;; ---------------------------------------------------------------------------
;;;; Entry point

(defn- pick-output [opts]
  (cond
    (:quiet opts)             (quiet-output)
    (= :json (:output opts))  (json-out/json-output)
    :else                     (default-output)))

(defn- run-scan [search-roots opts]
  (let [quiet?  (:quiet opts)
        output  (pick-output opts)
        scan    (if (false? (:parallel opts))
                  c/perform-scan
                  buf/parallel-scan)
        ;; With multiple roots, include the root prefix in path output so
        ;; results are unambiguous between roots. Single root keeps the
        ;; existing relative-from-root behaviour. --apath always wins.
        opts    (cond-> opts
                  (and (< 1 (count search-roots))
                       (not (:apath opts)))
                  (assoc :include-root? true))]
    (doseq [root search-roots]
      (scan root output r/render-cat opts))
    (when quiet? (saw-match? output))))

(defn main-entrypoint
  "Shared by -main and repl-main. hard-exit-on-errors? controls whether bad
  CLI args call System/exit. Returns the JVM exit status (0 = success,
  1 = bad args / no match in quiet mode)."
  [hard-exit-on-errors? args]
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
      (if hard-exit-on-errors?
        (cli/exit (if ok? 0 1) exit-message)
        (do (println "would exit with code" (if ok? 0 1) "msg," exit-message)
            (if ok? 0 1)))

      :else
      (let [matched? (tufte/profile {:when profile? :nmax 10000000}
                                    (run-scan search-roots opts))]
        (if (:quiet opts) (if matched? 0 1) 0)))))

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
