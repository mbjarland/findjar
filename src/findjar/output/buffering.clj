(ns findjar.output.buffering
  "Buffering FindJarOutput + a pmap-based parallel scan driver.

   Idea: each worker scans one file into its own buffering output, capturing
   protocol calls as data tuples. A single foreground replayer consumes the
   buffers in input order and dispatches to the real output. Worker output
   never interleaves and per-file ordering is preserved.

   dump-stream's third arg is already a materialized String (rendered before
   the protocol call), so capturing-and-replaying is safe across the jar's
   lifetime."
  (:require [findjar.core :as c]
            [findjar.protocols :as p]))

(defn buffering-output
  "A FindJarOutput that records every call as a tuple in an internal vector.
  Use buffer->calls to retrieve the recorded calls."
  []
  (let [calls (atom (transient []))]
    (with-meta
      (reify p/FindJarOutput
        (warn [_ msg ex opts]
          (swap! calls conj! [:warn msg ex opts]))
        (match [_ path opts]
          (swap! calls conj! [:match path opts]))
        (grep-match [_ max-line-# line-map opts]
          (swap! calls conj! [:grep-match max-line-# line-map opts]))
        (dump-stream [_ path materialized opts]
          (swap! calls conj! [:dump-stream path materialized opts]))
        (print-hash [_ path hash-type hash-value opts]
          (swap! calls conj! [:print-hash path hash-type hash-value opts])))
      {::calls calls})))

(defn buffer->calls
  "Drain a buffering-output, returning the persistent vector of recorded calls."
  [buffer]
  (-> buffer meta ::calls deref persistent!))

(defn replay!
  "Replay a sequence of recorded calls against a real FindJarOutput."
  [calls output]
  (doseq [call calls]
    (case (first call)
      :warn        (p/warn        output (nth call 1) (nth call 2) (nth call 3))
      :match       (p/match       output (nth call 1) (nth call 2))
      :grep-match  (p/grep-match  output (nth call 1) (nth call 2) (nth call 3))
      :dump-stream (p/dump-stream output (nth call 1) (nth call 2) (nth call 3))
      :print-hash  (p/print-hash  output (nth call 1) (nth call 2) (nth call 3) (nth call 4)))))

(defn parallel-scan
  "Like core/perform-scan but parallelises file scanning via pmap. Each file
  is scanned into a buffering output; recorded calls are replayed in input
  order against the real output. So per-file ordering is preserved and worker
  output never interleaves, but absolute throughput improves significantly
  on a maven repo full of jars."
  [search-root real-output render-cat opts]
  (let [opts    (c/munge-regexes opts)
        to-path (c/path-fn search-root opts)
        files   (c/candidate-files search-root opts)
        scan-one (fn [f]
                   (let [buf (buffering-output)]
                     (c/scan-file buf render-cat opts f (to-path f))
                     (buffer->calls buf)))]
    (doseq [calls (pmap scan-one files)]
      (replay! calls real-output))))
