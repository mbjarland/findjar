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
