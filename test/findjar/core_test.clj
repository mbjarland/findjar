(ns findjar.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as jio]
            [findjar.core :as c])
  (:import [java.io File]))

(deftest match-idxs-test
  (testing "no matches returns empty vector"
    (is (= [] (c/match-idxs #"xyz" "abcdef"))))
  (testing "single match"
    (is (= [{:start 1 :end 3}] (c/match-idxs #"bc" "abcdef"))))
  (testing "multiple non-overlapping matches"
    (is (= [{:start 0 :end 2} {:start 6 :end 8}]
           (c/match-idxs #"22" "2233442255")))))

(deftest name-part-test
  (testing "path with separators returns final segment"
    (is (= "c.txt" (c/name-part "a/b/c.txt"))))
  (testing "no separator returns the whole string"
    (is (= "c.txt" (c/name-part "c.txt"))))
  (testing "trailing slash returns empty"
    (is (= "" (c/name-part "a/b/")))))

(deftest file-ext-test
  (testing "lowercase extension"
    (is (= "txt" (c/file-ext (jio/file "foo.txt")))))
  (testing "uppercase extension is normalized to lowercase"
    (is (= "jar" (c/file-ext (jio/file "Foo.JAR")))))
  (testing "no extension returns nil"
    (is (nil? (c/file-ext (jio/file "Makefile")))))
  (testing "trailing dot returns nil"
    (is (nil? (c/file-ext (jio/file "weird.")))))
  (testing "dotfile with no extension returns nil"
    (is (nil? (c/file-ext (jio/file ".gitignore"))))))

(deftest hash-by-desc-test
  (is (= :sha1 (c/hash-by-desc "sha1")))
  (is (= :md5  (c/hash-by-desc "md5")))
  (is (nil? (c/hash-by-desc "made-up"))))

(deftest hash-algorithms-shape
  (testing "every entry has required keys"
    (doseq [[k v] c/hash-algorithms]
      (is (keyword? k))
      (is (fn? (:digest v)) (str k " is missing :digest"))
      (is (string? (:desc v)) (str k " is missing :desc")))))

(deftest file-finders-shape
  (testing "every entry has required keys"
    (doseq [[k v] c/file-finders]
      (is (or (keyword? k) (string? k)))
      (is (fn? (:scan v))     (str k " is missing :scan"))
      (is (string? (:desc v)) (str k " is missing :desc"))
      (is (char? (:char v))   (str k " is missing :char"))
      (is (contains? v :default) (str k " is missing :default")))))

(deftest munge-regexes-test
  (testing "no flags is a no-op"
    (let [opts {:name #"foo" :grep #"bar"}]
      (is (= opts (c/munge-regexes opts)))))
  (testing "case-insensitive flag actually matches case-insensitively"
    (let [opts (c/munge-regexes {:flags "i" :name #"FOO"})]
      (is (re-find (:name opts) "foobar"))))
  (testing "-i / :ignore-case behaves like -f i"
    (let [opts (c/munge-regexes {:ignore-case true :name #"FOO"})]
      (is (re-find (:name opts) "foobar"))))
  (testing "-i composes with -f m without doubling the i"
    (let [opts (c/munge-regexes {:ignore-case true :flags "m" :name #"FOO"})]
      (is (re-find (:name opts) "FOOBAR"))
      (is (re-find (:name opts) "foobar"))))
  (testing "-i with already-present 'i' in flags is idempotent"
    (let [opts (c/munge-regexes {:ignore-case true :flags "i" :name #"FOO"})]
      (is (re-find (:name opts) "foobar"))))
  (testing "non-pattern keys untouched"
    (let [opts (c/munge-regexes {:flags "i" :context 3 :name #"a"})]
      (is (= 3 (:context opts))))))

(deftest unknown-flag-chars-test
  (is (= #{}            (c/unknown-flag-chars "imsuxd")))
  (is (= #{\q}          (c/unknown-flag-chars "iq")))
  (is (= #{\1 \q}       (c/unknown-flag-chars "1q"))))

(deftest binary-stream?-test
  (testing "ASCII content is not binary"
    (is (false? (c/binary-stream?
                  (java.io.ByteArrayInputStream.
                    (.getBytes "hello world\n" "UTF-8"))))))
  (testing "NUL byte in first 8KB triggers binary detection"
    (is (true? (c/binary-stream?
                 (java.io.ByteArrayInputStream.
                   (byte-array [(byte 0x48) (byte 0x00) (byte 0x65)]))))))
  (testing "empty stream is not binary"
    (is (false? (c/binary-stream?
                  (java.io.ByteArrayInputStream. (byte-array 0)))))))
