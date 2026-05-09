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
