(ns findjar.cli-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [findjar.cli :as cli]))

(defn- parsed
  "Parse args and return either {:exit-message ... :ok? ...} or
  {:search-roots ... :opts ...}."
  [& args]
  (cli/validate-args args))

(deftest no-args-defaults-to-cwd
  (let [{:keys [search-roots opts exit-message]} (parsed)]
    (is (nil? exit-message))
    (is (= 1 (count search-roots)))
    (is (= "." (.getPath ^java.io.File (first search-roots))))
    (is (some? opts))))

(deftest multiple-search-roots-accepted
  (let [{:keys [search-roots]} (parsed "/tmp" "/var")]
    (is (= 2 (count search-roots)))
    (is (= ["/tmp" "/var"] (mapv #(.getPath ^java.io.File %) search-roots)))))

(deftest non-directory-fails
  (let [r (parsed "/this/path/does/not/exist/probably")]
    (is (str/includes? (:exit-message r) "non-directory search root"))))

(deftest some-bad-roots-fails-with-list
  (let [r (parsed "/tmp" "/this/does/not/exist" "/var")]
    (is (str/includes? (:exit-message r) "non-directory"))
    (is (str/includes? (:exit-message r) "this/does/not/exist"))))

(deftest path-and-apath-mutually-exclusive
  (let [r (parsed "/tmp" "-p" "x" "-a" "y")]
    (is (str/includes? (:exit-message r) "path (-p) and apath (-a)"))))

(deftest help-and-examples
  (is (:ok? (parsed "/tmp" "--help")))
  (is (:ok? (parsed "/tmp" "--examples"))))

(deftest hash-flag-accumulates
  (let [{:keys [opts]} (parsed "/tmp" "-s" "sha1" "-s" "md5")]
    (is (= [:sha1 :md5] (:hash opts)))))

(deftest hash-flag-rejects-unknown
  (let [r (parsed "/tmp" "-s" "totally-not-a-hash")]
    (is (str/includes? (:exit-message r) "hash must be one of"))))

(deftest types-flag-parses
  (let [{:keys [opts]} (parsed "/tmp" "-t" "jz")]
    (is (= #{"jar" "zip"} (:types opts)))))

(deftest types-flag-rejects-unknown
  (let [r (parsed "/tmp" "-t" "qx")]
    (is (str/includes? (:exit-message r) "type must be"))))

(deftest parallel-on-by-default
  (let [{:keys [opts]} (parsed "/tmp")]
    (is (true? (:parallel opts)))
    (is (not (contains? opts :no-parallel)))))

(deftest no-parallel-flag
  (let [{:keys [opts]} (parsed "/tmp" "--no-parallel")]
    (is (false? (:parallel opts)))))

(deftest hash-helpers
  (testing "hash-selectors lists all algorithms"
    (let [s (cli/hash-selectors)]
      (doseq [name ["md5" "sha1" "sha256" "sha512" "crc32"]]
        (is (str/includes? s name)))))
  (testing "parse-hash-selector round-trips"
    (is (= :sha1 (cli/parse-hash-selector "sha1")))
    (is (nil? (cli/parse-hash-selector "nope")))))

(deftest parallel-jobs-flag
  (testing "valid parallel-jobs is plumbed through"
    (let [{:keys [opts]} (parsed "/tmp" "--parallel-jobs" "3")]
      (is (= 3 (:parallel-jobs opts)))))
  (testing "non-positive parallel-jobs is rejected"
    (is (str/includes? (:exit-message (parsed "/tmp" "--parallel-jobs" "0"))
                       "must be a positive integer")))
  (testing "non-numeric parallel-jobs is rejected"
    (is (str/includes? (:exit-message (parsed "/tmp" "--parallel-jobs" "lots"))
                       "Error"))))

(deftest english-list-test
  (testing "0/1/2/3+ items"
    (is (= ""              (cli/english-list [])))
    (is (= "a"             (cli/english-list ["a"])))
    (is (= "a and b"       (cli/english-list ["a" "b"])))
    (is (= "a, b, and c"   (cli/english-list ["a" "b" "c"])))
    (is (= "a, b, c, and d" (cli/english-list ["a" "b" "c" "d"])))))

(deftest files-only-flag
  (let [{:keys [opts]} (parsed "/tmp" "-l")]
    (is (true? (:files-only opts))))
  (let [{:keys [opts]} (parsed "/tmp" "--files-only")]
    (is (true? (:files-only opts))))
  (let [{:keys [opts]} (parsed "/tmp")]
    (is (not (:files-only opts)))))

(deftest all-flag
  (let [{:keys [opts]} (parsed "/tmp" "--all")]
    (is (true? (:all opts))))
  (let [{:keys [opts]} (parsed "/tmp")]
    (is (not (:all opts)))))

(deftest version-flag-short-circuits
  (let [{:keys [exit-message ok?]} (parsed "--version")]
    (is ok?)
    (is (str/starts-with? exit-message "findjar ")))
  (let [{:keys [ok?]} (parsed "-V")]
    (is ok?)))

(deftest quiet-flag-parses
  (let [{:keys [opts]} (parsed "/tmp" "-q")]
    (is (true? (:quiet opts))))
  (let [{:keys [opts]} (parsed "/tmp" "--quiet")]
    (is (true? (:quiet opts)))))

(deftest follow-flag-parses
  (is (true? (:follow (:opts (parsed "/tmp" "-L")))))
  (is (nil?  (:follow (:opts (parsed "/tmp"))))))

(deftest max-depth-flag-parses
  (let [{:keys [opts]} (parsed "/tmp" "--max-depth" "3")]
    (is (= 3 (:max-depth opts))))
  (let [r (parsed "/tmp" "--max-depth" "-1")]
    (is (str/includes? (:exit-message r) "must be >= 0"))))

(deftest exclude-flag-accumulates
  (let [{:keys [opts]} (parsed "/tmp" "--exclude" "dist" "--exclude" "out")]
    (is (= #{"dist" "out"} (:exclude opts)))))

(deftest no-gitignore-flag
  (is (true? (:no-gitignore (:opts (parsed "/tmp" "--no-gitignore"))))))

(deftest output-format-flag
  (let [{:keys [opts]} (parsed "/tmp" "--output" "json")]
    (is (= :json (:output opts))))
  (let [r (parsed "/tmp" "--output" "yaml")]
    (is (str/includes? (:exit-message r) "must be 'text' or 'json'"))))

(deftest after-before-flags
  (is (= 3 (:after  (:opts (parsed "/tmp" "-A" "3")))))
  (is (= 2 (:before (:opts (parsed "/tmp" "-B" "2")))))
  (is (str/includes? (:exit-message (parsed "/tmp" "-A" "-1"))
                     "must be >= 0")))

(deftest glob-flag-and-name-mutex
  (let [{:keys [opts]} (parsed "/tmp" "-G" "*.clj")]
    (testing "glob compiles to a regex on the :name slot"
      (is (some? (:name opts)))
      (is (re-find (:name opts) "foo.clj"))
      (is (not (re-find (:name opts) "foo.cljc")))))
  (let [r (parsed "/tmp" "-G" "*.clj" "-n" "x")]
    (is (str/includes? (:exit-message r) "--glob (-G) and --name (-n)"))))

(deftest unknown-flag-chars-rejected
  (let [r (parsed "/tmp" "-f" "iq")]
    (is (str/includes? (:exit-message r)
                       "must be a combination of i, m, s, u, x, d"))))

(deftest find-by-hash-parses
  (let [{:keys [opts]} (parsed "/tmp"
                                "--find-by-hash"
                                "sha1:da39a3ee5e6b4b0d3255bfef95601890afd80709")]
    (is (= [{:algo :sha1 :hex "da39a3ee5e6b4b0d3255bfef95601890afd80709"}]
           (:find-by-hash opts))))
  (let [r (parsed "/tmp" "--find-by-hash" "nope")]
    (is (str/includes? (:exit-message r) "must be of the form")))
  (let [r (parsed "/tmp" "--find-by-hash" "weird:abc")]
    (is (str/includes? (:exit-message r) "unknown algorithm"))))

(deftest text-and-nested-flags
  (is (true? (:text   (:opts (parsed "/tmp" "--text")))))
  (is (true? (:nested (:opts (parsed "/tmp" "--nested"))))))

(deftest invert-match-flag
  (is (true? (:invert (:opts (parsed "/tmp" "-v")))))
  (is (true? (:invert (:opts (parsed "/tmp" "--invert-match"))))))

(deftest word-flag-wraps-grep-with-boundaries
  (let [{:keys [opts]} (parsed "/tmp" "-g" "foo" "-w")]
    (testing ":word is set"
      (is (true? (:word opts))))
    (testing "the grep pattern is wrapped with \\b boundaries"
      (let [^java.util.regex.Pattern p (:grep opts)]
        (is (str/starts-with? (.pattern p) "\\b"))
        (is (str/ends-with?   (.pattern p) "\\b"))
        (is (re-find p "hello foo world"))
        (is (not (re-find p "hello fooo world")))))))

(deftest count-and-max-count-flags
  (is (true? (:count (:opts (parsed "/tmp" "--count")))))
  (is (= 5 (:max-count (:opts (parsed "/tmp" "--max-count" "5")))))
  (is (str/includes? (:exit-message (parsed "/tmp" "--max-count" "0"))
                     "must be a positive integer")))

(deftest version-string-test
  (let [v (cli/version-string)]
    (testing "non-empty"
      (is (string? v))
      (is (pos? (count v))))
    (testing "either dev or version with hash + date"
      ;; In a built jar this would be e.g. "1.0.111 - abc1234 - 2024.05.08 ..."
      ;; In repl/test invocation it's typically "dev" or the on-disk edn.
      (is (or (= v "dev")
              (re-find #"^\d+\.\d+\.\d+" v))))))
