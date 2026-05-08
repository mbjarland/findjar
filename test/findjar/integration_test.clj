(ns findjar.integration-test
  "End-to-end tests: drive perform-scan and parallel-scan against a fresh
  fixture tree, asserting on the calls a recording FindJarOutput captures."
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [findjar.core :as c]
            [findjar.main :as main]
            [findjar.output.buffering :as buf]
            [findjar.recording-output :as ro]
            [findjar.test-fixtures :as fix])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; ----------------------------------------------------------------------------
;; Fixture lifecycle

(def ^:dynamic *root* nil)

(use-fixtures :once
  (fn [t]
    (let [root (fix/build-fixture-root)]
      (binding [*root* root]
        (try (t) (finally (fix/delete-recursively! root)))))))

(defn- raw-cat
  "Minimal cat renderer for tests — just returns the file's contents as a
  string. No ANSI, no line numbers."
  [_output _path stream-factory _opts]
  (with-open [s (stream-factory)]
    (slurp s)))

(defn- run
  "Run a serial scan against the fixture, returning the recording output."
  [opts]
  (let [out (ro/recording-output)
        opts (merge {:types #{:default "jar"}} opts)]
    (c/perform-scan *root* out raw-cat opts)
    out))

(defn- run-parallel
  [opts]
  (let [out (ro/recording-output)
        opts (merge {:types #{:default "jar"}} opts)]
    (buf/parallel-scan *root* out raw-cat opts)
    out))

;; ----------------------------------------------------------------------------
;; No filters: every candidate file is reported as a path match

(deftest list-all-default-types
  (let [paths (set (ro/paths-of (run {}) :match))]
    (testing "disk files are listed"
      (is (contains? paths "alpha.txt"))
      (is (contains? paths "beta.clj"))
      (is (contains? paths "empty.txt"))
      (is (contains? paths "nested/gamma.txt")))
    (testing "jar entries are listed with @ separator"
      (is (contains? paths "lib.jar@clojure/string.clj"))
      (is (contains? paths "lib.jar@META-INF/MANIFEST.MF")))
    (testing "zip is excluded by default (only n + j default-on)"
      (is (not (some #(str/includes? % "data.zip@") paths))))))

(deftest types-filter-jar-only
  (let [out (run {:types #{"jar"}})
        paths (set (ro/paths-of out :match))]
    (is (every? #(str/includes? % "lib.jar@") paths))
    (is (not (some #(re-find #"\.txt$" %) paths)))))

(deftest types-filter-zip-only
  (let [out (run {:types #{"zip"}})
        paths (set (ro/paths-of out :match))]
    (is (= #{"data.zip@data/numbers.txt"} paths))))

(deftest types-filter-disk-only
  (let [out (run {:types #{:default}})
        paths (set (ro/paths-of out :match))]
    (testing "no archive entries"
      (is (not (some #(str/includes? % "@") paths))))
    (testing "all disk files present"
      (is (contains? paths "alpha.txt"))
      (is (contains? paths "beta.clj"))
      (is (contains? paths "empty.jar"))
      (is (contains? paths "lib.jar")))))

;; ----------------------------------------------------------------------------
;; Name / path / apath filters

(deftest name-filter
  (testing "case-sensitive name match"
    (let [paths (set (ro/paths-of (run {:name #"^alpha"}) :match))]
      (is (= #{"alpha.txt"} paths))))
  (testing "name regex matches archive entry by file name only, not full path"
    (let [paths (set (ro/paths-of (run {:name #"^string\.clj$"}) :match))]
      (is (contains? paths "lib.jar@clojure/string.clj")))))

(deftest path-filter
  (let [paths (set (ro/paths-of (run {:path #"^nested/"}) :match))]
    (is (= #{"nested/gamma.txt"} paths))))

(deftest apath-filter
  (let [out (run {:apath #"alpha\.txt$"})
        paths (ro/paths-of out :match)]
    (is (= 1 (count paths)))
    (is (.startsWith ^String (first paths) "/")
        "apath returns absolute path")))

(deftest flags-case-insensitive
  (let [paths (set (ro/paths-of (run {:name #"ALPHA" :flags "i"}) :match))]
    (is (= #{"alpha.txt"} paths))))

;; ----------------------------------------------------------------------------
;; Grep

(deftest grep-disk-file
  (let [out  (run {:grep #"world"})
        hits (->> (ro/calls-of out)
                  (filter #(= :grep (first %)))
                  (filter (comp :hit? #(nth % 2))))]
    (is (= 1 (count hits)))
    (let [m (nth (first hits) 2)]
      (is (= "alpha.txt" (:path m)))
      (is (= 1 (:line-# m)))
      (is (= "world" (:line m))))))

(deftest grep-inside-jar
  (let [out  (run {:grep #"Rich Hickey"})
        hits (->> (ro/calls-of out)
                  (filter #(= :grep (first %)))
                  (filter (comp :hit? #(nth % 2)))
                  (map #(nth % 2)))
        paths (set (map :path hits))]
    (is (contains? paths "lib.jar@clojure/string.clj"))
    (is (contains? paths "beta.clj"))))

(deftest grep-with-context
  (let [out  (run {:grep #"world" :context 1})
        rows (->> (ro/calls-of out)
                  (filter #(= :grep (first %)))
                  (map #(nth % 2))
                  (filter #(= "alpha.txt" (:path %)))
                  (sort-by :line-#))]
    (testing "1 hit + 2 context lines around it"
      (is (= [0 1 2] (mapv :line-# rows)))
      (is (= [false true false] (mapv :hit? rows)))
      (is (= ["hello" "world" "clojure rocks"] (mapv :line rows))))))

;; ----------------------------------------------------------------------------
;; Hashing

(deftest hash-output-includes-algorithm
  (let [out  (run {:name #"^alpha\.txt$" :hash [:sha1]})
        rows (filter #(= :hash (first %)) (ro/calls-of out))]
    (is (= 1 (count rows)))
    (let [[_ path htype hval _] (first rows)]
      (is (= "alpha.txt" path))
      (is (= :sha1 htype))
      ;; sha1 of "hello\nworld\nclojure rocks\n"
      (is (= "cccc310785c1b7e82379306807694140ccb06739" hval)))))

(deftest hash-multiple-algorithms
  (let [out (run {:name #"^alpha\.txt$" :hash [:sha1 :md5 :crc32]})
        types (->> (ro/calls-of out)
                   (filter #(= :hash (first %)))
                   (map #(nth % 2))
                   set)]
    (is (= #{:sha1 :md5 :crc32} types))))

;; ----------------------------------------------------------------------------
;; Cat (-c) — exercises the full materialization path including jar streams

(deftest cat-disk-file
  (let [out  (run {:name #"^alpha\.txt$" :cat true})
        rows (filter #(= :dump (first %)) (ro/calls-of out))]
    (is (= 1 (count rows)))
    (is (= "hello\nworld\nclojure rocks\n" (nth (first rows) 2)))))

(deftest cat-jar-entry
  (let [out  (run {:name #"^MANIFEST\.MF$" :cat true})
        rows (filter #(= :dump (first %)) (ro/calls-of out))]
    (is (= 1 (count rows)))
    (is (str/includes? (nth (first rows) 2) "Manifest-Version: 1.0"))))

;; ----------------------------------------------------------------------------
;; -o (out-file) — exercises main/render-cat + default-output's file-write path

(deftest cat-with-out-file-writes-and-strips-ansi
  (let [tmp (.toFile (Files/createTempFile "findjar-cat-" ".txt"
                                            (into-array FileAttribute [])))]
    (try
      (let [output (main/default-output)
            opts   {:name #"^alpha\.txt$" :cat true :out-file tmp
                    :monochrome true :types #{:default "jar"}}]
        (c/perform-scan *root* output main/render-cat opts)
        (let [contents (slurp tmp)]
          (testing "file-mode output suppresses line-number prefixes"
            (is (str/includes? contents "hello"))
            (is (str/includes? contents "world"))
            (is (str/includes? contents "clojure rocks")))
          (testing "no ANSI escape sequences when written to a file"
            (is (not (re-find #"\[" contents))))))
      (finally
        (.delete tmp)))))

;; ----------------------------------------------------------------------------
;; Robustness: empty.jar must not crash the scan

(deftest empty-jar-does-not-crash
  (let [out (run {:types #{"jar"}})]
    (testing "no warnings emitted for the zero-byte jar"
      (is (empty? (filter #(= :warn (first %)) (ro/calls-of out)))))
    (testing "scan still completes and reports lib.jar entries"
      (is (some #(= "lib.jar@clojure/string.clj" %)
                (ro/paths-of out :match))))))

;; ----------------------------------------------------------------------------
;; Parallel scan must produce identical, ordered results — not just same set.

(deftest parallel-preserves-input-order
  (let [opts {:types #{:default "jar" "zip"}}]
    (testing "match calls appear in the same input order serial vs parallel"
      (is (= (ro/paths-of (run opts) :match)
             (ro/paths-of (run-parallel opts) :match))))))

(deftest parallel-grep-matches-serial
  (let [opts {:grep #"Rich Hickey"}
        gather (fn [out]
                 (->> (ro/calls-of out)
                      (filter #(= :grep (first %)))
                      (map #(nth % 2))
                      (filter :hit?)
                      (map (juxt :path :line-#))))]
    (is (= (gather (run opts))
           (gather (run-parallel opts))))))

(deftest parallel-jobs-honoured
  (testing "explicit --parallel-jobs limits parallelism but produces same output"
    (let [opts {:parallel-jobs 2 :grep #"Rich Hickey"}
          parallel-paths (->> (ro/calls-of (run-parallel opts))
                              (filter #(= :grep (first %)))
                              (map #(nth % 2))
                              (filter :hit?)
                              (map :path))
          serial-paths   (->> (ro/calls-of (run {:grep #"Rich Hickey"}))
                              (filter #(= :grep (first %)))
                              (map #(nth % 2))
                              (filter :hit?)
                              (map :path))]
      (is (= serial-paths parallel-paths)))))

;; ----------------------------------------------------------------------------
;; Worker exception is contained as a warn, doesn't kill the scan.

(deftest parallel-worker-exception-becomes-warn
  (let [out  (ro/recording-output)
        ;; render-cat that throws unconditionally; with :cat true we'll hit
        ;; it for the matching file and the scan should not abort.
        bad-render (fn [_ _ _ _] (throw (RuntimeException. "boom")))
        opts {:cat true :name #"^MANIFEST\.MF$"
              :types #{:default "jar"}}]
    (buf/parallel-scan *root* out bad-render opts)
    (testing "worker exception is captured as a warn"
      (is (some #(= :warn (first %)) (ro/calls-of out))))
    (testing "the warn message mentions the failing file"
      (let [warns (filter #(= :warn (first %)) (ro/calls-of out))]
        (is (some #(re-find #"lib\.jar" (nth % 1)) warns))))))
