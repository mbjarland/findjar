(ns findjar.output.json
  "JSON FindJarOutput. One JSON object per line (jsonl), so consumers can
  pipe straight to jq or stream-parse. We hand-roll a minimal escaper to
  avoid adding a dependency for ~30 lines of code."
  (:require [findjar.protocols :as p]))

(defn- escape-char ^String [^Character c]
  (case (int c)
    0x08 "\\b"
    0x09 "\\t"
    0x0a "\\n"
    0x0c "\\f"
    0x0d "\\r"
    0x22 "\\\""
    0x5c "\\\\"
    (if (< (int c) 0x20)
      (format "\\u%04x" (int c))
      (str c))))

(defn- escape-string [^String s]
  (let [sb (StringBuilder. (+ 2 (count s)))]
    (.append sb \")
    (doseq [c s] (.append sb (escape-char c)))
    (.append sb \")
    (.toString sb)))

(defn- write-value [v]
  (cond
    (nil? v)              "null"
    (string? v)           (escape-string v)
    (or (true? v) (false? v)) (str v)
    (number? v)           (str v)
    (keyword? v)          (escape-string (name v))
    (map? v)              (str "{"
                               (->> v
                                    (map (fn [[k val]]
                                           (str (escape-string (if (keyword? k) (name k) (str k)))
                                                ":"
                                                (write-value val))))
                                    (clojure.string/join ","))
                               "}")
    (sequential? v)       (str "["
                               (clojure.string/join "," (map write-value v))
                               "]")
    :else                 (escape-string (str v))))

(defn- emit-line! [m]
  (println (write-value m)))

(defn json-output
  "FindJarOutput emitting one JSON object per line. Schema:
    {\"kind\": \"match\"|\"grep\"|\"hash\"|\"cat\"|\"warn\",
     \"path\": ...,
     ... kind-specific fields ...}"
  []
  (reify p/FindJarOutput
    (warn [_ msg _ex _opts]
      (emit-line! {:kind "warn" :message msg}))
    (match [_ path _opts]
      (emit-line! {:kind "match" :path path}))
    (grep-match [_ _max-line-# {:keys [path line-# hit? line match-idxs]} _opts]
      (emit-line! (cond-> {:kind   "grep"
                           :path   path
                           :line   (inc line-#)
                           :hit?   hit?
                           :text   line}
                    (seq match-idxs)
                    (assoc :matches (mapv (juxt :start :end) match-idxs)))))
    (dump-stream [_ path materialized _opts]
      (emit-line! {:kind "cat" :path path :content materialized}))
    (print-hash [_ path hash-type hash-value _opts]
      (emit-line! {:kind "hash" :path path :algo (name hash-type) :hex hash-value}))))
