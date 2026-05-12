(ns findjar.integration-test
  "End-to-end tests: drive perform-scan and parallel-scan against a fresh
  fixture tree, asserting on the calls a recording FindJarOutput captures."
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [findjar.cli :as cli]
            [findjar.core :as c]
            [findjar.main :as main]
            [findjar.output.buffering :as buf]
            [findjar.protocols :as p]
            [findjar.recording-output :as ro]
            [findjar.render :as r]
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
    (testing "every match is a jar-entry path (contains @)"
      (is (every? #(.contains ^String % "@") paths)))
    (testing "no plain disk files"
      (is (not (contains? paths "alpha.txt"))))
    (testing "lib.jar entries appear"
      (is (contains? paths "lib.jar@clojure/string.clj"))
      (is (contains? paths "lib.jar@META-INF/MANIFEST.MF")))))

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
;; -o (out-file) — exercises render/render-cat + default-output's file-write path

(deftest cat-with-out-file-writes-and-strips-ansi
  (let [tmp (.toFile (Files/createTempFile "findjar-cat-" ".txt"
                                            (into-array FileAttribute [])))]
    (try
      (let [output (main/default-output)
            opts   {:name #"^alpha\.txt$" :cat true :out-file tmp
                    :monochrome true :types #{:default "jar"}}]
        (c/perform-scan *root* output r/render-cat opts)
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

;; ----------------------------------------------------------------------------
;; -l / --files-only with -g

(deftest files-only-collapses-grep-to-paths
  (let [out  (run {:grep #"Rich Hickey" :files-only true})
        ;; with --files-only, hits become :match calls (no :grep calls)
        match-paths (set (ro/paths-of out :match))
        grep-calls  (filter #(= :grep (first %)) (ro/calls-of out))]
    (testing "no per-line :grep emissions"
      (is (empty? grep-calls)))
    (testing "one :match per file containing the pattern"
      (is (contains? match-paths "beta.clj"))
      (is (contains? match-paths "lib.jar@clojure/string.clj")))))

;; ----------------------------------------------------------------------------
;; --all and default directory exclusions

(deftest excluded-dirs-are-skipped-by-default
  (let [paths (set (ro/paths-of (run {}) :match))]
    (testing "fixture's target/ and .git/ are not traversed"
      (is (not (some #(.contains ^String % "target/") paths)))
      (is (not (some #(.contains ^String % ".git/") paths))))))

(deftest all-flag-disables-exclusions
  (let [paths (set (ro/paths-of (run {:all true}) :match))]
    (testing "with --all, target/ and .git/ contents are reachable"
      (is (contains? paths "target/junk.txt"))
      (is (contains? paths ".git/HEAD")))))

(deftest empty-jar-warns-but-does-not-crash
  (let [out   (run {:types #{"jar"}})
        warns (filter #(= :warn (first %)) (ro/calls-of out))]
    (testing "the zero-byte jar produces a warn (no longer silent)"
      (is (= 1 (count warns)))
      (is (re-find #"empty\.jar" (nth (first warns) 1)))
      (is (re-find #"zero-byte" (nth (first warns) 1))))
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

;; ----------------------------------------------------------------------------
;; --max-depth, --exclude, --no-gitignore, .gitignore, symlinks

(deftest max-depth-zero-emits-nothing
  (let [paths (set (ro/paths-of (run {:max-depth 0}) :match))]
    (is (empty? paths))))

(deftest max-depth-one-keeps-only-direct-children
  (let [paths (set (ro/paths-of (run {:max-depth 1}) :match))]
    (testing "alpha.txt at root is included"
      (is (contains? paths "alpha.txt")))
    (testing "nested/gamma.txt at depth 2 is excluded"
      (is (not (contains? paths "nested/gamma.txt"))))))

(deftest exclude-flag-adds-to-default-exclusions
  (let [paths (set (ro/paths-of (run {:exclude #{"nested"}}) :match))]
    (testing "nested/ contents are excluded"
      (is (not (some #(.startsWith ^String % "nested/") paths))))
    (testing "default exclusions still apply (target/, .git/)"
      (is (not (some #(.startsWith ^String % "target/") paths))))))

(deftest gitignore-applied-by-default
  (let [paths (set (ro/paths-of (run {:all true}) :match))]
    (testing "fixture's .gitignore excludes 'ignored/' and '*.log'"
      (is (not (some #(.startsWith ^String % "ignored/") paths)))
      (is (not (contains? paths "trace.log"))))
    (testing "files not matched by gitignore are included"
      (is (contains? paths "alpha.txt")))))

(deftest gitignore-negation-re-includes
  (let [paths (set (ro/paths-of (run {:all true}) :match))]
    (testing "trace.log is excluded by *.log"
      (is (not (contains? paths "trace.log"))))
    (testing "keep.log is re-included by !keep.log"
      (is (contains? paths "keep.log")))))

(deftest gitignore-recursion-per-directory
  ;; sub-with-gi/.gitignore says '*.skip', which should hide
  ;; sub-with-gi/local.skip but not affect anything outside that subtree.
  (let [paths (set (ro/paths-of (run {:all true}) :match))]
    (testing "subtree-local pattern hides matched files in that subtree"
      (is (not (contains? paths "sub-with-gi/local.skip"))))
    (testing "non-matched files in the same subtree are still included"
      (is (contains? paths "sub-with-gi/should-stay.txt")))))

(deftest no-gitignore-flag-disables-gitignore
  (let [paths (set (ro/paths-of (run {:all true :no-gitignore true}) :match))]
    (testing "with --no-gitignore, ignored/ and *.log and *.skip are all reachable"
      (is (contains? paths "ignored/secret.clj"))
      (is (contains? paths "trace.log"))
      (is (contains? paths "sub-with-gi/local.skip")))))

;; ----------------------------------------------------------------------------
;; -v / --invert-match

(deftest grep-invert-match
  (let [out  (run {:grep #"world" :invert true})
        rows (->> (ro/calls-of out)
                  (filter #(= :grep (first %)))
                  (map #(nth % 2))
                  (filter #(= "alpha.txt" (:path %))))]
    (testing "non-matching lines emit hits"
      (is (= #{0 2} (set (map :line-# rows))))   ; "hello" and "clojure rocks"
      (is (every? :hit? rows)))
    (testing "non-matching hits have empty :match-idxs (no highlight)"
      (is (every? #(empty? (:match-idxs %)) rows)))))

;; ----------------------------------------------------------------------------
;; -w / --word-regexp — actually applied via cli/validate-args, but verify
;; the resulting pattern is what core sees.

(deftest grep-word-regexp-via-cli
  (let [{:keys [opts]} (cli/validate-args ["-g" "main" "-w" "/tmp"])]
    (is (re-find  (:grep opts) "the main thread"))
    (is (re-find  (:grep opts) "(main)"))
    (is (not (re-find (:grep opts) "remained")))
    (is (not (re-find (:grep opts) "domain")))))

;; ----------------------------------------------------------------------------
;; --count / --max-count

(deftest grep-count
  (let [out  (run {:grep #"Rich Hickey" :count true})
        counts (->> (ro/calls-of out)
                    (filter #(= :count (first %)))
                    (map (fn [c] [(nth c 1) (nth c 2)]))
                    (into {}))]
    (testing "one :count call per file with at least one match"
      ;; beta.clj has exactly one line containing 'Rich Hickey'
      (is (= 1 (get counts "beta.clj")))
      (is (= 1 (get counts "lib.jar@clojure/string.clj"))))
    (testing "no per-line :grep calls when --count is set"
      (is (empty? (filter #(= :grep (first %)) (ro/calls-of out)))))))

(deftest grep-max-count
  ;; beta.clj has 1 'Rich Hickey' hit; alpha.txt has 0; lib.jar@string.clj
  ;; has 1. So most files have at most 1 match. Use a low max-count and
  ;; verify ordering.
  (let [out  (run {:grep #"Rich Hickey" :max-count 1})
        per-file-hits
        (->> (ro/calls-of out)
             (filter #(= :grep (first %)))
             (map #(nth % 2))
             (filter :hit?)
             (group-by :path))]
    (testing "no file emits more than max-count hit lines"
      (doseq [[_ hits] per-file-hits]
        (is (<= (count hits) 1))))))

;; ----------------------------------------------------------------------------
;; -A / -B asymmetric grep context

(deftest grep-after-only
  (let [out (run {:grep #"world" :after 1})
        rows (->> (ro/calls-of out)
                  (filter #(= :grep (first %)))
                  (map #(nth % 2))
                  (filter #(= "alpha.txt" (:path %)))
                  (sort-by :line-#))]
    (testing "1 hit + 1 line of after-context, no before"
      (is (= [1 2] (mapv :line-# rows)))
      (is (= [true false] (mapv :hit? rows))))))

(deftest grep-before-only
  (let [out (run {:grep #"world" :before 1})
        rows (->> (ro/calls-of out)
                  (filter #(= :grep (first %)))
                  (map #(nth % 2))
                  (filter #(= "alpha.txt" (:path %)))
                  (sort-by :line-#))]
    (testing "1 line of before-context + the hit, no after"
      (is (= [0 1] (mapv :line-# rows)))
      (is (= [false true] (mapv :hit? rows))))))

;; ----------------------------------------------------------------------------
;; Binary file skipping

(deftest binary-files-skipped-when-grepping
  (let [out (run {:grep #"hello"
                  :types #{:default}})
        paths (set (ro/paths-of out :grep))]
    (testing "binary.dat (NUL in first 8KB) is not grepped"
      (is (not (some #(= "binary.dat" %) paths))))))

(deftest text-flag-forces-binary-grep
  (let [;; --text: even binary.dat gets read; the regex is plain so it'll
        ;; emit a non-hit line for any text content present
        out (run {:grep #"^Hello$" :text true :types #{:default}
                  :name #"^binary\.dat$"})]
    (testing "with --text the binary file is read (no skip warning, no error)"
      (is (empty? (filter #(= :warn (first %)) (ro/calls-of out)))))))

;; ----------------------------------------------------------------------------
;; Nested jar recursion

(deftest nested-flag-recurses-into-inner-jars
  (let [paths (set (ro/paths-of
                     (run {:nested true :types #{"jar"}})
                     :match))]
    (testing "without --nested, inner.jar entries aren't reachable"
      (let [paths-no-nested (set (ro/paths-of
                                   (run {:types #{"jar"}})
                                   :match))]
        (is (not (some #(.contains ^String % "inner.jar@") paths-no-nested)))))
    (testing "with --nested, the inner jar's entries appear under outer.jar@inner.jar@"
      (is (contains? paths "outer.jar@inner.jar@deep/token.txt")))))

(deftest nested-grep-finds-marker
  (let [out (run {:nested true :grep #"NESTED-MARKER" :types #{"jar"}})
        hits (->> (ro/calls-of out)
                  (filter #(= :grep (first %)))
                  (map #(nth % 2))
                  (filter :hit?))]
    (is (= 1 (count hits)))
    (is (= "outer.jar@inner.jar@deep/token.txt" (:path (first hits))))))

;; ----------------------------------------------------------------------------
;; tar / tar.gz support

(deftest tar-entries-listed
  (let [out (run {:types #{"tar"}})
        paths (set (ro/paths-of out :match))]
    (testing "uncompressed tar archive's entries appear with @-separator"
      (is (contains? paths "demo.tar@hello.txt"))
      (is (contains? paths "demo.tar@deep/x.txt")))
    (testing "gzipped tar archive too"
      (is (contains? paths "demo.tar.gz@hello.txt"))
      (is (contains? paths "demo.tar.gz@deep/x.txt")))))

(deftest grep-inside-tar
  (let [out (run {:types #{"tar"} :grep #"deep tar"})
        hits (->> (ro/calls-of out)
                  (filter #(= :grep (first %)))
                  (map #(nth % 2))
                  (filter :hit?))]
    (testing "grep finds the marker in both tar and tar.gz"
      (is (= 2 (count hits)))
      (is (every? #(re-find #"^demo\.tar(\.gz)?@deep/x\.txt$" %)
                  (map :path hits))))))

;; ----------------------------------------------------------------------------
;; --manifest sugar

(deftest manifest-sugar-extracts-mf-entries
  (let [out  (run {:manifest true})
        rows (filter #(= :dump (first %)) (ro/calls-of out))]
    (testing "every dumped block is a MANIFEST.MF entry from a jar"
      (is (seq rows))
      (is (every? #(re-find #"jar@.*MANIFEST\.MF$" (nth % 1)) rows)))
    (testing "the dumped content includes the manifest header"
      (is (some #(re-find #"Manifest-Version:" (nth % 2)) rows)))))

;; ----------------------------------------------------------------------------
;; --class-info via ASM

(deftest class-info-on-fixture-class
  (let [out (run {:class-info true :name #"^Greeter\.class$"
                  :types #{:default}})
        rows (filter #(= :class-info (first %)) (ro/calls-of out))]
    (is (= 1 (count rows)))
    (let [[_ path info _] (first rows)]
      (is (= "Greeter.class" path))
      (is (= "fixture/Greeter" (:name info)))
      (is (= "java/lang/Object" (:super info)))
      (is (= ["java/io/Serializable"] (vec (:interfaces info))))
      (is (contains? (:access info) :public))
      (testing "method names captured"
        (let [method-names (set (map :name (:methods info)))]
          (is (contains? method-names "<init>"))
          (is (contains? method-names "hello")))))))

(deftest class-info-skips-non-class-entries
  (let [out (run {:class-info true :name #"^alpha\.txt$"
                  :types #{:default}})]
    (testing "non-.class files don't produce class-info calls"
      (is (empty? (filter #(= :class-info (first %)) (ro/calls-of out)))))))

;; ----------------------------------------------------------------------------
;; --find-by-hash

(deftest find-by-hash-emits-match
  ;; sha1 of "hello\nworld\nclojure rocks\n" is the alpha-content sha1.
  (let [target "cccc310785c1b7e82379306807694140ccb06739"
        out (run {:find-by-hash [{:algo :sha1 :hex target}]
                  :types #{:default}})
        paths (set (ro/paths-of out :match))]
    (is (contains? paths "alpha.txt"))
    (testing "non-matching files don't emit"
      (is (not (contains? paths "beta.clj"))))))

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
;; Exit codes — grep-compatible: 0 if match, 1 if not, 2 on bad CLI args.

(defn- run-main
  "Drive findjar.main/main-entrypoint with stdout / stderr suppressed,
  returning the exit code. hard-exit? = false so System/exit is never
  called and the function just returns the code.

  Uses System/setOut so jansi.install!'s rebind of *out* (which fires
  inside main-entrypoint) still lands somewhere we control."
  [& args]
  (let [orig-out System/out
        orig-err System/err
        bos-out  (java.io.ByteArrayOutputStream.)
        bos-err  (java.io.ByteArrayOutputStream.)]
    (try
      (System/setOut (java.io.PrintStream. bos-out true "UTF-8"))
      (System/setErr (java.io.PrintStream. bos-err true "UTF-8"))
      (main/main-entrypoint false args)
      (finally
        (System/setOut orig-out)
        (System/setErr orig-err)))))

(defn- run-main-capture
  "Like run-main but returns {:exit :stdout :stderr}."
  [& args]
  (let [orig-out System/out
        orig-err System/err
        bos-out  (java.io.ByteArrayOutputStream.)
        bos-err  (java.io.ByteArrayOutputStream.)]
    (try
      (System/setOut (java.io.PrintStream. bos-out true "UTF-8"))
      (System/setErr (java.io.PrintStream. bos-err true "UTF-8"))
      (let [code (main/main-entrypoint false args)]
        (.flush *out*)
        {:exit   code
         :stdout (.toString bos-out "UTF-8")
         :stderr (.toString bos-err "UTF-8")})
      (finally
        (System/setOut orig-out)
        (System/setErr orig-err)))))

(deftest exit-code-0-when-match-found
  (is (= 0 (run-main (.getPath *root*) "-n" "alpha.txt"))))

(deftest exit-code-1-when-no-match
  (is (= 1 (run-main (.getPath *root*)
                     "-n" "definitely-not-a-real-filename-12345"))))

(deftest exit-code-1-when-grep-matches-nothing
  (is (= 1 (run-main (.getPath *root*) "-g" "ZZZZZ-not-here"))))

(deftest exit-code-2-on-bad-cli-args
  (is (= 2 (run-main "/nonexistent/path/please")))
  (is (= 2 (run-main (.getPath *root*) "-t" "x"))))

(deftest exit-code-0-on-help-flag
  ;; --help is informational, exit 0
  (is (= 0 (run-main "--help"))))

(deftest null-terminator-separates-paths-with-nul
  ;; Unit-level: drive default-output's match method directly with *out*
  ;; bound. End-to-end capture is harder because ansi/install! (called
  ;; inside main-entrypoint) rebinds *out* after the test sets it.
  (let [out (#'main/default-output)
        sw  (java.io.StringWriter.)]
    (binding [*out* sw]
      (p/match out "path/one.txt" {:null true})
      (p/match out "path/two.txt" {:null true}))
    (testing "paths are NUL-terminated, no newlines"
      (is (= (str "path/one.txt" (char 0) "path/two.txt" (char 0))
             (str sw))))))

(deftest null-default-still-uses-newlines
  (let [out (#'main/default-output)
        sw  (java.io.StringWriter.)]
    (binding [*out* sw]
      (p/match out "path/one.txt" {})
      (p/match out "path/two.txt" {}))
    (testing "without :null, output uses newlines"
      (is (= "path/one.txt\npath/two.txt\n" (str sw))))))

(deftest stats-flag-does-not-break-exit-code
  ;; --stats prints to stderr at end of scan; just smoke-test the path.
  (is (= 0 (run-main (.getPath *root*) "-n" "alpha.txt" "--stats")))
  (is (= 1 (run-main (.getPath *root*) "-n" "nope-not-here" "--stats"))))

(deftest exit-code-quiet-mode-is-grep-compatible
  ;; -q just suppresses output; exit code is the same as without -q.
  (is (= 0 (run-main (.getPath *root*) "-n" "alpha.txt" "-q")))
  (is (= 1 (run-main (.getPath *root*) "-n" "nope-not-here" "-q"))))

;; ----------------------------------------------------------------------------
;; Directory entries inside an archive must NOT contribute to action output
;; (hashing, cat, grep, find-by-hash, class-info, manifest). They were
;; previously hashed, producing da39a3ee... (sha1 of empty) for every
;; META-INF/, BOOT-INF/, etc.

(defn- write-jar-with-dir-entries! [^File f]
  (jio/make-parents f)
  (with-open [zos (java.util.zip.ZipOutputStream. (jio/output-stream f))]
    ;; Explicit directory entries (trailing slash) — the bug surface.
    (.putNextEntry zos (java.util.zip.ZipEntry. "META-INF/"))
    (.closeEntry zos)
    (.putNextEntry zos (java.util.zip.ZipEntry. "BOOT-INF/lib/"))
    (.closeEntry zos)
    ;; A real file entry alongside.
    (.putNextEntry zos (java.util.zip.ZipEntry. "META-INF/MANIFEST.MF"))
    (let [b (.getBytes "Manifest-Version: 1.0\n" "UTF-8")]
      (.write zos b 0 (alength b)))
    (.closeEntry zos)))

(deftest directory-entries-not-hashed
  (let [tmp ^File (-> (Files/createTempDirectory "findjar-dirtest-"
                        (into-array FileAttribute []))
                      (.toFile))]
    (try
      (let [jar (jio/file tmp "with-dirs.jar")]
        (write-jar-with-dir-entries! jar))
      (let [out  (ro/recording-output)
            opts {:types #{:default "jar"} :hash [:sha1]}]
        (c/perform-scan tmp out raw-cat opts)
        (let [hash-paths (set (map #(nth % 1)
                                   (filter #(= :hash (first %))
                                           (ro/calls-of out))))]
          (testing "no directory entry receives a hash row"
            (is (not (some #(.endsWith ^String % "/") hash-paths))
                (str "got hash rows for: " hash-paths)))
          (testing "the file entry still gets hashed"
            (is (some #(.endsWith ^String % "MANIFEST.MF") hash-paths)))))
      (finally (fix/delete-recursively! tmp)))))

(deftest directory-entries-still-listed-in-path-mode
  (let [tmp ^File (-> (Files/createTempDirectory "findjar-dirlist-"
                        (into-array FileAttribute []))
                      (.toFile))]
    (try
      (let [jar (jio/file tmp "with-dirs.jar")]
        (write-jar-with-dir-entries! jar))
      (let [out  (ro/recording-output)
            opts {:types #{:default "jar"}}]
        (c/perform-scan tmp out raw-cat opts)
        (let [paths (set (ro/paths-of out :match))]
          (testing "directory entries appear in default path-list mode"
            (is (some #(re-find #"META-INF/$" %) paths)))
          (testing "file entries also appear"
            (is (some #(re-find #"MANIFEST\.MF$" %) paths)))))
      (finally (fix/delete-recursively! tmp)))))

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
