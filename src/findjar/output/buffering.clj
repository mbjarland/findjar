(ns findjar.output.buffering
  "Buffering FindJarOutput + a pmap-based parallel scan driver.

   Each worker scans one file into its own Buffer, capturing protocol calls
   as data tuples. A single foreground replayer consumes the buffers in
   input order and dispatches to the real output. Worker output never
   interleaves and per-file ordering is preserved.

   dump-stream's third arg is already a materialized String (rendered
   before the protocol call), so capturing-and-replaying is safe across
   the source jar's lifetime."
  (:require [findjar.core :as c]
            [findjar.protocols :as p])
  (:import [java.io File]
           [java.util ArrayList]))

;;;; ---------------------------------------------------------------------------
;;;; Buffer — a single-thread-owned FindJarOutput that records calls into
;;;; an ArrayList. We use a deftype rather than reify+atom+transient because
;;;; the ownership model is "one thread writes, then someone reads" — there's
;;;; no value in dressing that up as STM.

(deftype Buffer [^ArrayList calls]
  p/FindJarOutput
  (warn [_ msg ex opts]
    (.add calls [:warn msg ex opts]))
  (match [_ path opts]
    (.add calls [:match path opts]))
  (grep-match [_ max-line-# line-map opts]
    (.add calls [:grep-match max-line-# line-map opts]))
  (grep-count [_ path n opts]
    (.add calls [:grep-count path n opts]))
  (dump-stream [_ path materialized opts]
    (.add calls [:dump-stream path materialized opts]))
  (print-hash [_ path hash-type hash-value opts]
    (.add calls [:print-hash path hash-type hash-value opts])))

(defn buffer
  "A fresh Buffer. Single-threaded: one worker writes, one consumer reads
  via buffer-calls."
  ^Buffer []
  (->Buffer (ArrayList.)))

(defn buffer-calls
  "Snapshot the recorded calls as a persistent vector. Pure read; safe to
  call multiple times (though typical usage is once per buffer)."
  [^Buffer b]
  (vec (.calls b)))

(defn replay!
  "Replay a sequence of recorded calls against a real FindJarOutput."
  [calls output]
  (doseq [[kind & args] calls]
    (case kind
      :warn        (apply p/warn        output args)
      :match       (apply p/match       output args)
      :grep-match  (apply p/grep-match  output args)
      :grep-count  (apply p/grep-count  output args)
      :dump-stream (apply p/dump-stream output args)
      :print-hash  (apply p/print-hash  output args))))

;;;; ---------------------------------------------------------------------------
;;;; Bounded pmap — same shape as clojure.core/pmap but lets the caller pick
;;;; the read-ahead window (= effective parallelism). Useful when the default
;;;; cores+2 is wrong for the workload (HDD or networked filesystem).

(defn- pmap-n
  "Like pmap but caps in-flight futures at n. nil/non-positive n falls back
  to the standard pmap."
  [n f coll]
  (if (and n (pos? n))
    (let [rets (map #(future (f %)) coll)
          step (fn step [[x & xs :as vs] fs]
                 (lazy-seq
                   (if-let [s (seq fs)]
                     (cons (deref x) (step xs (rest s)))
                     (map deref vs))))]
      (step rets (drop n rets)))
    (pmap f coll)))

;;;; ---------------------------------------------------------------------------
;;;; parallel-scan

(defn- scan-into-buffer
  "Run scan-file against a fresh buffer; if the worker throws, convert into
  a :warn call so the doseq replay loop sees a consistent stream of calls
  rather than a stack trace."
  [render-cat opts to-path ^File f]
  (let [b (buffer)]
    (try
      (c/scan-file b render-cat opts f (to-path f))
      (catch Throwable t
        (p/warn b
                (str "scanning " (.getPath f) " - " (.getMessage t))
                t opts)))
    (buffer-calls b)))

(defn parallel-scan
  "Like core/perform-scan but parallelises file scanning. Each file is
  scanned into its own Buffer; recorded calls are replayed in input order
  against the real output. Worker output never interleaves, per-file
  ordering is preserved, and exceptions in a worker become warnings rather
  than killing the scan.

  Parallelism is bounded by (:parallel-jobs opts), defaulting to whatever
  pmap picks (cores+2)."
  [search-root real-output render-cat opts]
  (let [opts    (c/munge-regexes opts)
        to-path (c/path-fn search-root opts)
        files   (c/candidate-files search-root opts)
        n       (:parallel-jobs opts)]
    (doseq [calls (pmap-n n #(scan-into-buffer render-cat opts to-path %) files)]
      (replay! calls real-output))))
