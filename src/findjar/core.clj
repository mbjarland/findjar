(ns findjar.core
  (:require [findjar.cli :as cli]
            [clojure.string :as str]
            [clojure.java.io :as jio]
            [pandect.algo.sha1 :refer [sha1]]
            [pandect.algo.md5 :refer [md5]]
            [pandect.algo.crc32 :refer [crc32*]])
  (:gen-class)
  (:import (java.io File)
           (java.util.zip ZipFile ZipEntry)
           (java.util.regex Pattern)))

(defprotocol FindJarHandler
  (warn [file type msg opts])
  (match [display-name])
  (grep-match [display-name line-nr hit? line opts])
  (dump-stream [display-name stream-factory opts])
  (hash [display-name stream-factory hashes opts]))


(comment
  (def output-handlers
    {:warning     (fn [file type msg opts])
     :match       (fn [display-name])
     :grep-match  (fn [display-name line-nr hit? line opts])
     :dump-stream (fn [display-name stream-factory opts])
     :hash        (fn [display-name stream-factory hashes opts])
     })
  )

(defn file-ext [^File f]
  (let [p (.getCanonicalPath f)
        i (.lastIndexOf p ".")]
    (if (and (pos? i) (not (= (inc i) (count p))))
      (subs p (inc i))
      nil)))

(comment
  (valid-file?
    (jio/file "findjar.jar")
    {:active-file-types {"jar" {:key \k :value \v}
                         "clj" {:key \c :value \v}}}
    {:warning (fn [f type msg opts]
                (println msg))}))

(defn valid-file? [^File f opts handlers]                   ;;TODO: use multimethod file type
  (let [active-types (:active-file-types opts)
        ext          (.toLowerCase (file-ext f))
        warn-handler (:warning handlers)]
    (and
      (.isFile f)
      (if (.canRead f) true (warn-handler f :can-not-read
                                          "WARN: can not read file"
                                          opts))
      (get active-types ext))))

(defn relative-path [^File search-root ^File f]
  (subs
    (str/replace (.getCanonicalPath f) (.getCanonicalPath search-root) "")
    1))

(defn print-line [^String display-name ^long n ^Boolean hit? ^String line]
  (println (str (str/trim display-name) ":" (inc n) (if hit? ">" ":") (str/trim line))))

(defn dump-stream [stream-factory display-name opts]
  (let [of (:out-file opts)]
    (if of
      (do
        (with-open [w (jio/writer of :append true)]
          (.write w (str "<<<<<<< " display-name \newline))
          (jio/copy (stream-factory) w)
          (.write w (str ">>>>>>>" \newline)))
        (println display-name ">>" (.getPath of)))
      (with-open [stream (stream-factory)
                  reader (jio/reader stream)]
        (println "<<<<<<<" display-name)
        (let [s (line-seq reader)]
          (doseq [[n line] (map-indexed vector s)]
            (println (inc n) (str/trim line))))
        (println ">>>>>>>")))))

(defn md5-stream [stream-factory display-name]
  (with-open [stream (stream-factory)]
    (println (md5 stream) display-name)))

(defn sha1-stream [stream-factory display-name]
  (with-open [stream (stream-factory)]
    (println (sha1 stream) display-name)))

(defn matches-with-context [sliding context pattern display-name]
  (reduce
    (fn [a [n lines]]
      (if (re-find pattern (nth lines context))
        (concat a
                (keep
                  (fn [[cn l]]
                    (when l [display-name cn (= cn n) l]))
                  (map-indexed #(vector (+ (- n context) %1) %2) lines)))

        a))
    []
    (map-indexed vector sliding)))

(defn remove-duplicated-lines
  "removes lines which are duplicated by the context lines
  window functionality. Multiple lines with the same line
  number should be folded into one and when possible, matching
  lines should win in this filters. Incoming matches are represented
  as [display-name line-number match? line]"
  [matches]
  (reduce-kv
    (fn [a _ group]
      (if-let [ml (first (filter #(nth % 2) group))]
        (conj a ml)
        (conj a (first group))))
    []
    (group-by second matches)))

(defn grep-stream [stream-factory display-name opts]
  "iterate through the file using a sliding window of
  context lines before the 'current line' and context lines
  after, output result on console on matches"
  (with-open [stream (stream-factory)
              reader (jio/reader stream)]
    (let [s       (line-seq reader)
          pattern (:grep opts)
          context (or (:context opts) 0)
          window  (inc (* 2 context))
          pad     (repeat context nil)                      ;TODO: fix padding with empty string
          sliding (partition window 1 (concat pad s pad))
          matches (matches-with-context sliding context pattern display-name)
          uniques (remove-duplicated-lines matches)]
      (doseq [match (sort-by second uniques)]
        (apply print-line match)))))

(defn grep-stream-original [stream-factory display-name opts]
  "iterate through the file using a sliding window of
  context lines before the 'current line' and context lines
  after, output result on console on matches"
  (with-open [stream (stream-factory)
              reader (jio/reader stream)]
    (let [s       (line-seq reader)
          pattern (:grep opts)
          context (or (:context opts) 0)
          window  (inc (* 2 context))
          pad     (repeat context nil)                      ;TODO: fix padding with empty string
          sliding (partition window 1 (concat pad s pad))]
      (doseq [[n lines] (map-indexed vector sliding)]
        (when (re-find pattern (nth lines context))
          (doseq [[cn l] (map-indexed #(vector (+ (- n context) %1) %2) lines)]
            (print-line display-name cn (= cn n) l)))))))


(defn stream-line-matches? [stream-factory ^Pattern pattern]
  (with-open [stream (stream-factory)]
    (let [reader (jio/reader stream)]
      (some (fn [line] (re-find pattern line)) (line-seq reader)))))

;; TODO: Match on paths - implement path and apath in the cond below
(defn print-stream-matches [opts file-name display-name stream-factory handlers]
  (let [{:keys [name grep path apath cat hashes]} opts
        macro-op (or cat md5 sha1)]
    (cond
      (and apath (not (re-find apath display-name))) nil    ; no apath match
      (and path (not (re-find path display-name))) nil      ; no path match
      (and name (not (re-find name file-name))) nil         ; no name match
      (not (or macro-op grep)) ((:match handlers) display-name) ; normal non-grep match
      (and grep macro-op (not (stream-line-matches? stream-factory grep))) nil ;grep+macro and no matches -> nil
      hashes ((:hash handlers) hashes stream-factory display-name opts)
      md5 (md5-stream stream-factory display-name) ((:hash handlers) display-name :md5) ;;TODO!!!!
      sha1 (sha1-stream stream-factory display-name)
      cat (dump-stream stream-factory display-name opts)
      grep (grep-stream stream-factory display-name opts))))


; TODO: make this a multi-method to enables more file formats
; TODO: make -type accept a set to support "-t fjg" for files, jar files, gzip files etc
(defn find-in-jar [^File f display-name opts handlers]
  (try
    (with-open [^ZipFile zip (ZipFile. f)]
      (doseq [^ZipEntry entry (enumeration-seq (.entries zip))]
        (let [entry-path     (.getName entry)
              entry-name     (.getName (jio/file entry-path))
              display-name   (str (str/trim display-name) "@" (str/trim entry-path))
              stream-factory #(.getInputStream zip entry)]
          (print-stream-matches opts entry-name display-name stream-factory handlers))))
    (catch Exception e
      ((:warning handlers) f :error-opening (str "error opening " (.getPath f) ", error:" (.getMessage e)) opts))))

;; extensible file type handlers
(defmulti cli-file-type identity)
(defmethod cli-file-type \j [_]
  {:ext               "jar"
   :desc              "jar files"
   :search-by-default true})

(defmethod cli-file-type \z [_]
  {:ext               "zip"
   :desc              "zip files"
   :search-by-default false})

(defmethod cli-file-type \d [_]
  {:ext               nil
   :desc              "disk files"
   :search-by-default true})

(defmulti find-in-file-type (fn [^File f _ _ _] (file-ext f)))

(defmethod find-in-file-type "jar" [^File f display-name opts handlers]
  (find-in-jar f display-name opts handlers))

(defmethod find-in-file-type "zip" [^File f display-name opts handlers]
  (find-in-jar f display-name opts handlers))

(defmethod find-in-file-type :default [^File f display-name opts handlers]
  (let [file-name      (.getName f)
        stream-factory #(jio/input-stream f)]
    (print-stream-matches opts file-name display-name stream-factory handlers)))

;; extensible file hashing
(defmulti cli-hash-type (fn [cli-type-char] cli-type-char))
(defmethod cli-hash-type \m [_]
  {:id   :md5
   :desc "md5 hash"})
(defmethod cli-hash-type \s [_]
  {:id   :sha1
   :desc "sha1 hash"})


(defn calculate-pandect-hash
  "internal function to calculate pandect hashes"
  [type pandect-fn display-name stream-factory handlers]
  (let [hash (with-open [stream (stream-factory)]
               (pandect-fn stream))]
    ((:hash handlers) display-name type hash)))

(defmulti calculate-hash (fn [id _ _ _] id))

(defmethod calculate-hash :md5
  ^{:cli {:desc "md5 hash"}}
  [_ display-name stream-factory handlers]
  (calculate-pandect-hash :md5 md5 display-name stream-factory handlers))

(defmethod calculate-hash :sha1 [_ display-name stream-factory handlers]
  (calculate-pandect-hash :sha1 sha1 display-name stream-factory handlers))



;;TODO: (:active-file-types opts) is a map of
;;TODO: active file types {"jar" blah "zip" blah}
(defn find-in-file [^File f ^File root opts handlers]
  (when (valid-file? f opts handlers)
    (let [display-name (if (:apath opts) (.getCanonicalPath f)
                                         (relative-path root f))]
      (find-in-file-type f display-name opts handlers))))

(defn munge-regexes [opts]
  (let [{:keys [flags name grep path apath]} opts]
    (if (not flags)
      opts
      (let [f (str "(?" flags ")")]
        (reduce
          (fn [acc k]
            (let [v (k acc)]
              (if v
                (assoc acc k (re-pattern (str f (.pattern v))))
                acc)))
          opts
          [:name :grep :path :apath])))))

(defn perform-file-scan [opts handlers]
  (let [root (:search-root opts)
        opts (munge-regexes opts)]
    (doseq [^File f (filter #(-> % .isFile) (file-seq root))]
      (find-in-file f root opts handlers))))

(defn default-handlers []
  (reify FindJarHandler
    (warn [file type msg opts]
      (println "WARN: can not read file -" (.getPath file)))
    (match [display-name]
      (println display-name))
    (grep-match [display-name line-nr hit? line opts]
      (println (str (str/trim display-name)
                    ":" (inc line-nr) (if hit? ">" ":")
                    (str/trim line))))
    (dump-stream [display-name stream-factory opts]
      (dump-stream stream-factory display-name opts))       ;;TODO: switch arg order
    (hash [display-name stream-factory hashes opts]
      (doseq [hash hashes]
        (calculate-hash hash display-name stream-factory handlers)))))

(defn -main [& args]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        handlers (default-handlers)
        opts     (assoc opts :search-root search-root)

        ]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (perform-file-scan opts handlers))))

(defn repl-main [& args]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)]
    (if exit-message
      (println "would exit with code " (if ok? 0 1))
      (perform-file-scan (assoc opts :search-root search-root)))))

(comment

  (repl-main "/home/mbjarland/projects/kpna/packages/ATG10.2/"
             "-n" "GLOBAL.properties"
             "-g" "logging")

  )
