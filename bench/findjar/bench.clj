(ns findjar.bench
  "Micro-benchmarks for the hot paths in findjar. Run with `clj -M:bench`.

  Targets:
    1. compile-glob — JIT-friendly regex builder; matters because every
       --include-glob / --exclude-glob compiles once per scan.
    2. match-idxs — per-line during grep; the inner loop of -g.
    3. gitignore-matcher — per-candidate-file during the walk.
    4. perform-scan over the fixture tree — end-to-end."
  (:require [clojure.java.io :as jio]
            [criterium.core :as crit]
            [findjar.core :as c]
            [findjar.recording-output :as ro]
            [findjar.test-fixtures :as fix])
  (:import [java.io File]))

(defn- raw-cat [_ _ stream-factory _]
  (with-open [s (stream-factory)] (slurp s)))

(defn- run-bench [label f]
  (println (str "\n== " label " ==\n"))
  (let [r (crit/quick-benchmark (f) {:samples 30})]
    (crit/report-result r)
    r))

(defn -main [& _]
  (run-bench "compile-glob '**/*.clj'"
             (fn [] (c/compile-glob "**/*.clj")))

  (run-bench "match-idxs single hit"
             (let [pat (re-pattern "Hickey")
                   line "  ^{:author \"Rich Hickey\" :doc \"...\"}"]
               (fn [] (c/match-idxs pat line))))

  (run-bench "match-idxs five hits per line"
             (let [pat (re-pattern "a")
                   line "a quick brown fox jumps over a lazy aardvark afterwards"]
               (fn [] (c/match-idxs pat line))))

  (let [root ^File (fix/build-fixture-root)]
    (try
      (run-bench "perform-scan over fixture (no filters)"
                 (let [opts {:types #{:default "jar"}}]
                   (fn []
                     (let [out (ro/recording-output)]
                       (c/perform-scan root out raw-cat opts)
                       (count (ro/calls-of out))))))

      (run-bench "perform-scan grep across fixture"
                 (let [opts {:types #{:default "jar"} :grep #"Rich Hickey"}]
                   (fn []
                     (let [out (ro/recording-output)]
                       (c/perform-scan root out raw-cat opts)))))
      (finally (fix/delete-recursively! root))))

  (shutdown-agents))
