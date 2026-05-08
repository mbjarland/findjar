(ns findjar.cli-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [findjar.cli :as cli]))

(defn- parsed
  "Parse args and return either {:exit-message ... :ok? ...} or
  {:search-root ... :opts ...}."
  [& args]
  (cli/validate-args args))

(deftest no-args-fails
  (let [r (parsed)]
    (is (str/includes? (:exit-message r) "no search root"))
    (is (not (:ok? r)))))

(deftest multiple-search-roots-fails
  (let [r (parsed "/tmp" "/tmp/other")]
    (is (str/includes? (:exit-message r) "multiple search-roots"))))

(deftest non-directory-fails
  (let [r (parsed "/this/path/does/not/exist/probably")]
    (is (str/includes? (:exit-message r) "invalid non-directory"))))

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
