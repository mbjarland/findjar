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
    (or (sequential? v) (set? v))
                          (str "["
                               (clojure.string/join "," (map write-value v))
                               "]")
    :else                 (escape-string (str v))))

(defn- record-of
  "Build the JSON map for a given findjar event."
  [kind args]
  (case kind
    :warn       {:kind "warn"  :message (first args)}
    :match      {:kind "match" :path    (first args)}
    :grep       (let [{:keys [path line-# hit? line match-idxs]} (first args)]
                  (cond-> {:kind "grep" :path path :line (inc line-#)
                           :hit? hit? :text line}
                    (seq match-idxs)
                    (assoc :matches (mapv (juxt :start :end) match-idxs))))
    :count      {:kind "count"      :path (first args) :count (second args)}
    :class-info (assoc (second args) :kind "class-info" :path (first args))
    :cat        {:kind "cat"  :path (first args) :content (second args)}
    :hash       (let [[path htype hval] args]
                  {:kind "hash" :path path :algo (name htype) :hex hval})
    :duplicate  (let [[fqn occs] args]
                  {:kind "duplicate-class" :fqn fqn :occurrences (vec occs)})))

;; ----------------------------------------------------------------------------
;; jsonl ("json lines") emitter — one JSON object per line. Default --output
;; json format, also surfaced as --output ndjson.

(defn- emit-line! [m]
  (println (write-value m))
  (.flush *out*))

(defn json-output
  "FindJarOutput emitting one JSON object per line. Schema:
    {\"kind\": \"match\"|\"grep\"|\"hash\"|\"cat\"|\"warn\",
     \"path\": ...,
     ... kind-specific fields ...}"
  []
  (reify p/FindJarOutput
    (warn        [_ msg _ex _opts]                        (emit-line! (record-of :warn       [msg])))
    (match       [_ path _opts]                           (emit-line! (record-of :match      [path])))
    (grep-match  [_ _max-# m _opts]                       (emit-line! (record-of :grep       [m])))
    (grep-count  [_ path n _opts]                         (emit-line! (record-of :count      [path n])))
    (class-info  [_ path info _opts]                      (emit-line! (record-of :class-info [path info])))
    (dump-stream [_ path materialized _opts]              (emit-line! (record-of :cat        [path materialized])))
    (print-hash  [_ path hash-type hash-value _opts]      (emit-line! (record-of :hash       [path hash-type hash-value])))
    (duplicate-class [_ fqn occurrences _opts]            (emit-line! (record-of :duplicate  [fqn occurrences])))))

;; ----------------------------------------------------------------------------
;; json-array emitter — same records, wrapped in a single top-level JSON
;; array so consumers can use `jq` directly without `-s`. main.clj emits the
;; opening '[' before the scan and the closing ']' after; this sink handles
;; the per-record commas.

(defn json-array-output
  "FindJarOutput emitting records joined by commas. Caller is responsible
  for printing the surrounding '[' and ']' / final newline."
  []
  (let [first? (atom true)
        emit! (fn [m]
                (when-not @first? (print ","))
                (reset! first? false)
                (print (write-value m))
                (.flush *out*))]
    (reify p/FindJarOutput
      (warn        [_ msg _ex _opts]                       (emit! (record-of :warn       [msg])))
      (match       [_ path _opts]                          (emit! (record-of :match      [path])))
      (grep-match  [_ _max-# m _opts]                      (emit! (record-of :grep       [m])))
      (grep-count  [_ path n _opts]                        (emit! (record-of :count      [path n])))
      (class-info  [_ path info _opts]                     (emit! (record-of :class-info [path info])))
      (dump-stream [_ path materialized _opts]             (emit! (record-of :cat        [path materialized])))
      (print-hash  [_ path hash-type hash-value _opts]     (emit! (record-of :hash       [path hash-type hash-value])))
      (duplicate-class [_ fqn occurrences _opts]           (emit! (record-of :duplicate  [fqn occurrences]))))))
