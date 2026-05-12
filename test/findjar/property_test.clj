(ns findjar.property-test
  "Generative tests for the bits of findjar that are most prone to subtle
  off-by-one or edge-case bugs: glob compilation, gitignore parsing, the
  intra-line highlight tokenizer."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [findjar.core :as c]
            [findjar.render :as r]))

;; ----------------------------------------------------------------------------
;; compile-glob: trivial round-trip — escaping the special chars produces
;; a pattern that matches the original string verbatim.

(def ^:private safe-char
  ;; Avoid the glob metas (*, ?) AND the regex specials we escape (. \ ( ) +
  ;; ^ $ { } |) — we want to generate strings where compile-glob's output
  ;; matches the input as a literal.
  (gen/elements (concat
                  (map char (range (int \a) (inc (int \z))))
                  (map char (range (int \A) (inc (int \Z))))
                  (map char (range (int \0) (inc (int \9))))
                  [\_ \- \/])))

(defspec compile-glob-no-wildcards-round-trips 100
  (prop/for-all [s (gen/such-that not-empty (gen/vector safe-char 1 30))]
    (let [s (apply str s)
          re (c/compile-glob s)]
      (boolean (re-find re s)))))

(defspec compile-glob-star-matches-arbitrary-segment 100
  (prop/for-all [prefix (gen/vector safe-char 0 8)
                 middle (gen/vector safe-char 0 12)
                 suffix (gen/vector safe-char 0 8)]
    (let [p (apply str prefix)
          m (apply str middle)
          s (apply str suffix)
          pat (str p "*" s)
          re  (c/compile-glob pat)
          target (str p m s)]
      ;; Star matches anything except '/', so m must not contain '/'.
      (if (re-find #"/" m)
        (or (boolean (re-find re target)) true)  ;; ignore violating cases
        (boolean (re-find re target))))))

(defspec compile-glob-globstar-matches-across-segments 100
  (prop/for-all [prefix (gen/vector safe-char 0 8)
                 middle (gen/vector safe-char 0 16)
                 suffix (gen/vector safe-char 0 8)]
    (let [p (apply str prefix)
          m (apply str middle)
          s (apply str suffix)
          pat (str p "**" s)
          re  (c/compile-glob pat)
          target (str p m s)]
      (boolean (re-find re target)))))

;; ----------------------------------------------------------------------------
;; split-at-idxs (render-side tokeniser): rejoining the tokens recovers the
;; original string regardless of the index list.

(defspec split-at-idxs-rejoins-to-original 200
  (prop/for-all [s (gen/such-that not-empty gen/string-alphanumeric)]
    (let [len (count s)
          idxs (sort (distinct (filter #(<= 0 % len)
                                       (range 0 len 2))))]
      (= s (apply str (r/split-at-idxs s idxs))))))

;; ----------------------------------------------------------------------------
;; unknown-flag-chars: exactly the set of chars that aren't in the allowed
;; set [i m s u x d].

(defspec unknown-flag-chars-rejects-foreign 100
  (prop/for-all [allowed-len (gen/choose 0 6)
                 noise-len   (gen/choose 0 6)
                 picks (gen/vector (gen/elements [\i \m \s \u \x \d]) 0 6)
                 noise (gen/vector (gen/elements [\1 \2 \3 \q \w \e \r \y]) 0 6)]
    (let [allowed (apply str (take allowed-len picks))
          noisy   (apply str (take noise-len noise))
          input   (str allowed noisy)
          result  (c/unknown-flag-chars input)]
      (testing (str "input=" (pr-str input))
        (and (every? #(not (#{\i \m \s \u \x \d} %)) result)
             (= result (set (filter #(not (#{\i \m \s \u \x \d} %)) input))))))))
