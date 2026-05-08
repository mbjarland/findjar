(ns ^{:doc    "Entry point. Default FindJarOutput emits to stdout / out-file.
                Cat rendering lives here (with ANSI / line numbers); it's
                injected into core/perform-scan as a function so the core is
                free of presentation concerns."
      :author "Matias Bjarland"}
  findjar.main
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [findjar.cli :as cli]
            [findjar.core :as c]
            [findjar.output.buffering :as buf]
            [findjar.protocols :as p]
            [jansi-clj.auto]
            [jansi-clj.core :refer [green red white]]
            [taoensso.tufte :as tufte])
  (:import [java.io File])
  (:gen-class))

;;;; ---------------------------------------------------------------------------
;;;; Coloring

(def ^:dynamic *use-colors* true)

(defn use-colors? [opts] (not (:monochrome opts)))

(defn style
  "Optionally ANSI-color str. color-fn is e.g. jansi-clj.core/red."
  [color-fn s]
  (if *use-colors* (color-fn s) s))

(defn split-at-idxs
  "Split s into substrings at the given (sorted, non-decreasing) indices.
  (split-at-idxs \"abcdef\" [2 4]) => [\"ab\" \"cd\" \"ef\"]"
  [^String s idxs]
  (let [n   (count s)
        all (-> [0] (into idxs) (conj n))]
    (mapv (fn [[a b]] (subs s a b))
          (partition 2 1 all))))

(defn highlight-matches
  "Color the matched ranges within line. match-idxs is the seq of {:start :end}
  produced by core/match-idxs. hit-color-fn is a jansi color fn."
  [hit? line match-idxs hit-color-fn]
  (if-not hit?
    line
    (let [tokens (split-at-idxs line (mapcat (juxt :start :end) match-idxs))]
      (->> tokens
           (map-indexed (fn [i tok] (if (odd? i) (style hit-color-fn tok) tok)))
           (apply str)))))

;;;; ---------------------------------------------------------------------------
;;;; Cat rendering — stream lines, format on the fly. Returns the formatted
;;;; string (with optional ANSI) ready for emission.

;; Fixed-width line-number column. Files with more than 999999 lines simply
;; lose alignment past the cap — they still render correctly. Avoiding a
;; precount keeps cat usable on large files.
(def ^:private cat-pad-width 6)

(defn render-cat
  "Materialize stream-factory's content as a printable cat block. When
  out-file is set in opts we suppress ANSI and line-number prefixes so the
  written file is plain. Returns nil if the stream couldn't be opened —
  the failure is reported via output's warn."
  [output path stream-factory opts]
  (let [to-file? (some? (:out-file opts))
        grep     (:grep opts)
        fmt      (str "%" cat-pad-width "d ")]
    (c/with-reader output opts stream-factory
      (fn [reader]
        (binding [*use-colors* (and (not to-file?) (use-colors? opts))]
          (with-out-str
            (println (style red "<<<<<<<") path)
            (doseq [[n line] (map-indexed vector (line-seq reader))]
              (let [idxs   (when grep (c/match-idxs grep line))
                    line   (if (seq idxs)
                             (highlight-matches true line idxs red)
                             line)
                    prefix (if to-file? "" (format fmt (inc n)))]
                (println (str (style green prefix) line))))
            (println (style red ">>>>>>>"))))))))

;;;; ---------------------------------------------------------------------------
;;;; Default FindJarOutput — emits to *out* (and -o file when configured)

(defn- format-grep-line [max-line-# {:keys [path line-# hit? line match-idxs]} opts]
  (let [context? (pos? (or (:context opts) 0))
        width    (count (str max-line-#))
        num-col  (format (str "%" width "d ") (inc line-#))]
    (str path
         (if (and (not hit?) context?) " " ":")
         num-col
         (highlight-matches hit? line match-idxs red))))

(defn default-output
  "FindJarOutput implementation that prints to *out* (and the -o file when
  set). All ANSI-coloring is gated by use-colors?/(:monochrome opts)."
  []
  (reify p/FindJarOutput
    (warn [_ msg _ex opts]
      (binding [*use-colors* (use-colors? opts)
                *out*        *err*]
        (println (style red (str "WARN: " msg)))))

    (match [_ path _opts]
      (println path))

    (grep-match [_ max-line-# line-map opts]
      (binding [*use-colors* (use-colors? opts)]
        (println (format-grep-line max-line-# line-map opts))))

    (dump-stream [_ path materialized opts]
      (when materialized
        (if-let [^File of (:out-file opts)]
          (with-open [w (jio/writer of :append true)]
            (.write w ^String materialized)
            (println path ">>" (.getPath of)))
          (print materialized))))

    (print-hash [_ path hash-type hash-value opts]
      (binding [*use-colors* (use-colors? opts)]
        (println (style white hash-value)
                 (name hash-type)
                 path)))))

;;;; ---------------------------------------------------------------------------
;;;; Entry point

(defn- run-scan [search-root opts]
  (let [output (default-output)
        scan   (if (false? (:parallel opts))
                 c/perform-scan
                 buf/parallel-scan)]
    (scan search-root output render-cat opts)))

(defn main-entrypoint
  "Shared by -main and repl-main. hard-exit-on-errors? controls whether bad
  CLI args call System/exit."
  [hard-exit-on-errors? args]
  (tufte/add-basic-println-handler! {})
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        profile? (:profile opts)]
    (if exit-message
      (if hard-exit-on-errors?
        (cli/exit (if ok? 0 1) exit-message)
        (println "would exit with code" (if ok? 0 1) "msg," exit-message))
      (tufte/profile {:when profile? :nmax 10000000}
                     (run-scan search-root opts)))))

(defn -main [& args]
  (try
    (main-entrypoint true args)
    (finally
      ;; Flush *out* explicitly: under (:gen-class), Clojure's *out* is an
      ;; unbuffered OutputStreamWriter, but its CharsetEncoder buffer holds
      ;; up to 8KB that JVM exit will not drain on its own. Without this,
      ;; large -c (cat) blocks silently disappear.
      (.flush *out*)
      ;; pmap / futures use the agent pool, whose non-daemon idle threads
      ;; would otherwise pin the JVM open for 60s after the scan completes.
      (shutdown-agents))))

(defn repl-main [& args]
  (main-entrypoint false args))
