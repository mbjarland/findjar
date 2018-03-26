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

(comment
  (def output-handlers
    {:warning     (fn [file type msg opts])
     :match       (fn [display-name])
     :grep-match  (fn [display-name line-nr hit? line opts])
     :dump-stream (fn [display-name stream-factory opts])
     :hash        (fn [display-name type hash])
     })

  )
(defn valid-file? [^File f opts]                            ;;TODO: use multimethod file type
  (let [valid-type (if (-> f .getName (.endsWith ".jar")) \j \f)
        type       (:type opts)]
    (and
      (.isFile f)
      (if (.canRead f) true (println "WARN: can not read file -" (.getPath f)))
      (or (not type) (= type valid-type)))))

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
(defn print-stream-matches [opts file-name display-name stream-factory]
  (let [{:keys [name grep path apath cat md5 sha1]} opts
        macro-op (or cat md5 sha1)]
    (cond
      (and apath (not (re-find apath display-name))) nil    ; no apath match
      (and path (not (re-find path display-name))) nil      ; no path match
      (and name (not (re-find name file-name))) nil
      (not (or macro-op grep)) (println display-name)
      (and grep macro-op (not (stream-line-matches? stream-factory grep))) nil
      md5 (md5-stream stream-factory display-name)
      sha1 (sha1-stream stream-factory display-name)
      cat (dump-stream stream-factory display-name opts)
      grep (grep-stream stream-factory display-name opts))))

(defn file-ext [^File f]
  (let [p (.getCanonicalPath f)
        i (.lastIndexOf p ".")]
    (if (pos? i)
      (subs p i)
      nil)))

; TODO: make this a multi-method to enables more file formats
; TODO: make -type accept a set to support "-t fjg" for files, jar files, gzip files etc
(defn find-in-jar [^File f display-name opts]
  (try
    (with-open [^ZipFile zip (ZipFile. f)]
      (doseq [^ZipEntry entry (enumeration-seq (.entries zip))]
        (let [entry-path     (.getName entry)
              entry-name     (.getName (jio/file entry-path))
              display-name   (str (str/trim display-name) "@" (str/trim entry-path))
              stream-factory #(.getInputStream zip entry)]
          (print-stream-matches opts entry-name display-name stream-factory))))
    (catch Exception e
      (println "error opening" (.getPath f) ", error:" (.getMessage e))
      (throw e))))

;; extensible file type handlers
(defmulti cli-file-type identity)
(defmethod cli-file-type "jar" [_]
  {:type-char         \j
   :desc              "jar files"
   :search-by-default true})

(defmethod cli-file-type "zip" [_]
  {:type-char         \z
   :desc              "zip files"
   :search-by-default false})

(defmethod cli-file-type :default [_]
  {:type-char         \d
   :desc              "disk files"
   :search-by-default true})

(defmulti find-in-file-type
          "todo: implement
          [if file type specified
            then use only the specified file types
            else use search-by-default file types
            this will come in as a list of :active-filetypes
            in opts"
          (fn [^File f display-name opts] (file-ext f)))

(defmethod find-in-file-type "jar" [^File f display-name opts]
  (find-in-jar f display-name opts))

(defmethod find-in-file-type "zip" [^File f display-name opts]
  (find-in-jar f display-name opts))

(defmethod find-in-file-type :default [^File f display-name opts]
  (let [file-name      (.getName f)
        stream-factory #(jio/input-stream f)]
    (print-stream-matches opts file-name display-name stream-factory)))

;; extensible file hashing
(defmulti cli-hash-type identity)
(defmethod cli-hash-type :md5 [type]
  {:type-char \m
   :desc      "md5 hash"})
(defmethod cli-hash-type :sha1 [type]
  {:type-char \s
   :desc      "sha1 hash"})




(defn find-in-file [^File f ^File root opts handlers]
  (when (valid-file? f opts)
    (let [display-name (if (:apath opts) (.getCanonicalPath f)
                                         (relative-path root f))]
      (find-in-file-type f display-name opts))))

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

(defn build-output-handlers []
  {:warning     (fn [file type msg opts]
                  (println "WARN: can not read file -" (.getPath file))

                  :match (fn [display-name]
                           (println display-name))

                  :grep-match (fn [display-name line-nr hit? line opts]
                                (println (str (str/trim display-name)
                                              ":" (inc line-nr) (if hit? ">" ":")
                                              (str/trim line)))))
   :dump-stream dump-stream
   :hash        (fn [display-name type hash])}
                  ;(println (sha1 stream) display-name))}
  )

(defn -main [& args]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        output-handlers {}
        opts (assoc opts :search-root search-root)

        ]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (perform-file-scan opts output-handlers))))

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
