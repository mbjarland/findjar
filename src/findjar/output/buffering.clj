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
  (class-info [_ path info opts]
    (.add calls [:class-info path info opts]))
  (dump-stream [_ path materialized opts]
    (.add calls [:dump-stream path materialized opts]))
  (print-hash [_ path hash-type hash-value opts]
    (.add calls [:print-hash path hash-type hash-value opts]))
  (duplicate-class [_ fqn occurrences opts]
    (.add calls [:duplicate-class fqn occurrences opts])))

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
      :class-info  (apply p/class-info  output args)
      :dump-stream (apply p/dump-stream output args)
      :print-hash       (apply p/print-hash       output args)
      :duplicate-class  (apply p/duplicate-class  output args))))

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

(defn- synchronized-output
  "Wrap a FindJarOutput so concurrent worker threads don't interleave
  their emissions. Used by parallel-scan in --unordered mode where
  workers write directly to the real output instead of through a
  per-file Buffer."
  [delegate]
  (let [lock (Object.)]
    (reify p/FindJarOutput
      (warn            [_ m e o]   (locking lock (p/warn            delegate m e o)))
      (match           [_ p o]     (locking lock (p/match           delegate p o)))
      (grep-match      [_ mx m o]  (locking lock (p/grep-match      delegate mx m o)))
      (grep-count      [_ p n o]   (locking lock (p/grep-count      delegate p n o)))
      (class-info      [_ p i o]   (locking lock (p/class-info      delegate p i o)))
      (dump-stream     [_ p s o]   (locking lock (p/dump-stream     delegate p s o)))
      (print-hash      [_ p t v o] (locking lock (p/print-hash      delegate p t v o)))
      (duplicate-class [_ f os o]  (locking lock (p/duplicate-class delegate f os o))))))

(defn parallel-scan
  "Like core/perform-scan but parallelises file scanning. Two modes:

   ordered (default): each file is scanned into its own Buffer and
   replayed in input order against the real output. Worker output never
   interleaves, per-file ordering is preserved. Trade-off: output for
   file N is held back until files 1..N-1 finish.

   unordered (--unordered): workers emit directly to a sync-wrapped
   real output. First match shows up as soon as ANY worker finds it
   (great for 'findjar ... | head' on huge corpora) but output order
   no longer matches filesystem order.

  Parallelism is bounded by (:parallel-jobs opts), defaulting to
  whatever pmap picks (cores+2). Exceptions in a worker become warnings
  rather than killing the scan."
  [search-root real-output render-cat opts]
  (let [opts    (c/munge-regexes opts)
        to-path (c/path-fn search-root opts)
        files   (c/candidate-files search-root opts)
        n       (:parallel-jobs opts)]
    (if (:unordered opts)
      (let [sync-out (synchronized-output real-output)]
        (doseq [_ (pmap-n n
                          (fn [^File f]
                            (try
                              (c/scan-file sync-out render-cat opts f (to-path f))
                              (catch Throwable t
                                (p/warn sync-out
                                        (str "scanning " (.getPath f)
                                             " - " (.getMessage t))
                                        t opts))))
                          files)]
          nil))
      (doseq [calls (pmap-n n #(scan-into-buffer render-cat opts to-path %) files)]
        (replay! calls real-output)))))
