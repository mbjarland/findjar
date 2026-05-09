(ns findjar.recording-output
  "A test FindJarOutput that records every call as a tuple. Used for asserting
  on what core/perform-scan emits without coupling tests to console output."
  (:require [findjar.protocols :as p]))

(defn recording-output []
  (let [calls (atom [])]
    (with-meta
      (reify p/FindJarOutput
        (warn        [_ msg ex opts]            (swap! calls conj [:warn msg ex opts]))
        (match       [_ path opts]              (swap! calls conj [:match path opts]))
        (grep-match  [_ max-line-# m opts]      (swap! calls conj [:grep max-line-# m opts]))
        (grep-count  [_ path n opts]            (swap! calls conj [:count path n opts]))
        (class-info  [_ path info opts]         (swap! calls conj [:class-info path info opts]))
        (dump-stream [_ path materialized opts] (swap! calls conj [:dump path materialized opts]))
        (print-hash  [_ path htype hval opts]   (swap! calls conj [:hash path htype hval opts])))
      {::calls calls})))

(defn calls-of [out]
  (-> out meta ::calls deref))

(defn paths-of [out kind]
  (->> (calls-of out)
       (filter #(= kind (first %)))
       (map second)
       sort
       vec))
