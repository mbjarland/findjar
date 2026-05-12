(ns findjar.json-output-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [findjar.output.json :as json-out]
            [findjar.protocols :as p]))

(defn- capture
  "Run f against a json-output, returning the printed lines."
  [f]
  (let [sw (java.io.StringWriter.)]
    (binding [*out* sw]
      (f (json-out/json-output)))
    (->> (.toString sw)
         str/split-lines
         (remove str/blank?))))

(deftest match-shape
  (let [[line] (capture (fn [out] (p/match out "src/foo.clj" {})))]
    (is (str/includes? line "\"kind\":\"match\""))
    (is (str/includes? line "\"path\":\"src/foo.clj\""))))

(deftest grep-shape
  (let [line-map {:path "src/foo.clj" :line-# 9 :hit? true
                  :line "    :foo true"
                  :match-idxs [{:start 4 :end 8}]}
        [line] (capture (fn [out] (p/grep-match out 9 line-map {})))]
    (is (str/includes? line "\"kind\":\"grep\""))
    (is (str/includes? line "\"line\":10"))   ; inc'd to 1-based
    (is (str/includes? line "\"hit?\":true"))
    (is (str/includes? line "\"matches\":[[4,8]]"))))

(deftest hash-shape
  (let [[line] (capture
                 (fn [out]
                   (p/print-hash out "x.clj" :sha1 "abc123" {})))]
    (is (str/includes? line "\"kind\":\"hash\""))
    (is (str/includes? line "\"algo\":\"sha1\""))
    (is (str/includes? line "\"hex\":\"abc123\""))))

(deftest dump-shape
  (let [[line] (capture
                 (fn [out]
                   (p/dump-stream out "x.clj" "hello\nworld\n" {})))]
    (is (str/includes? line "\"kind\":\"cat\""))
    (is (str/includes? line "\"content\":\"hello\\nworld\\n\""))))

(deftest warn-shape
  (let [[line] (capture
                 (fn [out]
                   (p/warn out "broken jar" nil {})))]
    (is (str/includes? line "\"kind\":\"warn\""))
    (is (str/includes? line "\"message\":\"broken jar\""))))

(deftest escapes-special-chars
  (let [[line] (capture
                 (fn [out]
                   (p/match out "weird\"\\path\nfoo" {})))]
    (is (str/includes? line "\"path\":\"weird\\\"\\\\path\\nfoo\""))))

;; ----------------------------------------------------------------------------
;; --output json-array: records joined by commas, no surrounding brackets
;; (main.clj prints those before/after the scan).

(defn- capture-raw
  "Run f against a json-array-output, returning the raw stdout string."
  [f]
  (let [sw (java.io.StringWriter.)]
    (binding [*out* sw]
      (f (json-out/json-array-output)))
    (.toString sw)))

(deftest json-array-output-no-records-emits-nothing
  (is (= "" (capture-raw (fn [_])))))

(deftest json-array-output-single-record-no-comma
  (let [s (capture-raw (fn [out] (p/match out "foo.clj" {})))]
    (is (str/starts-with? s "{\"kind\":\"match\""))
    (is (not (str/includes? s ",{")))
    (is (not (str/ends-with? s "\n")))))

(deftest json-array-output-multiple-records-comma-separated
  (let [s (capture-raw (fn [out]
                         (p/match out "a.clj" {})
                         (p/match out "b.clj" {})
                         (p/match out "c.clj" {})))]
    ;; Records are joined by commas between adjacent records — three
    ;; records means exactly two inter-record separators. Each record
    ;; itself also contains a comma between its kind and path fields,
    ;; so the exact total isn't the cleanest assertion. Pattern-match
    ;; the shape instead:
    (is (re-find #"^\{[^{}]+\},\{[^{}]+\},\{[^{}]+\}$" s))
    ;; And confirm the prefix wraps as a valid JSON array.
    (is (re-find #"^\[\{.*\},\{.*\},\{.*\}\]\s*$" (str "[" s "]")))))

;; ----------------------------------------------------------------------------
;; --output sarif: SARIF 2.1.0 results joined by commas (caller adds wrapper).

(defn- capture-sarif [f]
  (let [sw (java.io.StringWriter.)]
    (binding [*out* sw]
      (f (json-out/sarif-output)))
    (.toString sw)))

(deftest sarif-result-shape-for-match
  (let [s (capture-sarif (fn [out] (p/match out "src/foo.clj" {})))]
    (is (str/includes? s "\"ruleId\":\"match\""))
    (is (str/includes? s "\"level\":\"note\""))
    (is (str/includes? s "\"artifactLocation\":{\"uri\":\"src/foo.clj\"}"))))

(deftest sarif-result-shape-for-duplicate-class
  (let [s (capture-sarif
            (fn [out]
              (p/duplicate-class out "com.example.Foo"
                                 [{:path "a.jar@com/example/Foo.class" :hash "abc"}
                                  {:path "b.jar@com/example/Foo.class" :hash "def"}]
                                 {})))]
    (is (str/includes? s "\"ruleId\":\"duplicate-class\""))
    (is (str/includes? s "\"level\":\"error\""))
    (is (str/includes? s "com.example.Foo"))))

(deftest sarif-cat-is-not-a-finding
  (let [s (capture-sarif
            (fn [out]
              (p/dump-stream out "x.clj" "...content..." {})))]
    (is (= "" s))))
