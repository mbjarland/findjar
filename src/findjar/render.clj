(ns findjar.render
  "Pure-ish text rendering for findjar's stdout output: ANSI coloring,
  intra-line match highlighting, cat block formatting, and grep-line
  formatting. Kept separate from findjar.main so the entry point can
  focus on lifecycle (CLI, scan, exit codes) and so the render functions
  are easy to unit-test."
  (:require [clojure.string :as str]
            [findjar.core :as c]
            [jansi-clj.core :refer [green red]])
  (:import [java.io File]))

;;;; ---------------------------------------------------------------------------
;;;; Coloring

(def ^:dynamic *use-colors* true)

(defn- no-color-env?
  "Honor the NO_COLOR convention (https://no-color.org). Any non-empty value
  of NO_COLOR disables ANSI coloring."
  []
  (let [v (System/getenv "NO_COLOR")]
    (and (some? v) (not= "" v))))

(defn use-colors?
  "ANSI coloring is off if -m is set or NO_COLOR is set in the environment."
  [opts]
  (and (not (:monochrome opts)) (not (no-color-env?))))

(defn style
  "Optionally ANSI-color s. color-fn is e.g. jansi-clj.core/red."
  [color-fn s]
  (if *use-colors* (color-fn s) s))

;;;; ---------------------------------------------------------------------------
;;;; Intra-line highlighting

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
;;;; Cat block — two streaming passes (count, then format) so width is exact
;;;; without buffering the whole file into memory.

(defn- count-lines [output opts stream-factory]
  (or (c/with-reader output opts stream-factory
        (fn [reader] (count (line-seq reader))))
      0))

(defn render-cat
  "Materialize stream-factory's content as a printable cat block. When
  out-file is set in opts we suppress ANSI and line-number prefixes so
  the written file is plain. Returns nil if the stream couldn't be opened
  — failure is reported via output's warn."
  [output path stream-factory opts]
  (let [to-file? (some? (:out-file opts))
        grep     (:grep opts)
        max-w    (count (str (count-lines output opts stream-factory)))]
    (c/with-reader output opts stream-factory
      (fn [reader]
        (binding [*use-colors* (and (not to-file?) (use-colors? opts))]
          (with-out-str
            (println (style red "<<<<<<<") path)
            (doseq [[n line] (map-indexed vector (line-seq reader))]
              (let [idxs    (when grep (c/match-idxs grep line))
                    line    (if (seq idxs)
                              (highlight-matches true line idxs red)
                              line)
                    display (inc n)
                    pad-len (- max-w (count (str display)))
                    prefix  (if to-file?
                              ""
                              (str (apply str (repeat pad-len \space))
                                   display " "))]
                (println (str (style green prefix) line))))
            (println (style red ">>>>>>>"))))))))

;;;; ---------------------------------------------------------------------------
;;;; Grep line

(defn format-grep-line [max-line-# {:keys [path line-# hit? line match-idxs]} opts]
  ;; Padding intentionally matches master's output verbatim: bare display
  ;; number, then (inc (- width len)) trailing spaces. Users may have
  ;; scripts parsing this output; preserve byte-for-byte compat.
  (let [context? (pos? (+ (or (:context opts) 0)
                          (or (:after opts) 0)
                          (or (:before opts) 0)))
        display  (inc line-#)
        width    (count (str max-line-#))
        len      (count (str display))
        pad      (apply str (repeat (inc (- width len)) \space))]
    (str path
         (if (and (not hit?) context?) " " ":")
         display
         pad
         (highlight-matches hit? line match-idxs red))))
