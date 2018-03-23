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

(defn valid-file? [^File f opts]
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
          (.write w (str ">>> " display-name \newline))
          (jio/copy (stream-factory) w)
          (.write w (str \newline)))
        (println display-name ">>" (.getPath of)))
      (with-open [stream (stream-factory)
                  reader (jio/reader stream)]
        (println ">>>" display-name)
        (let [s (line-seq reader)]
          (doseq [[n line] (map-indexed vector s)]
            (println (inc n) (str/trim line))))
        (println)))))

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

(defn print-stream-matches [opts file-name display-name stream-factory]
  (let [{:keys [name grep path apath cat md5 sha1]} opts
        macro-op (or cat md5 sha1)]
    (cond
      (and name (not (re-find name display-name))) nil
      (not (or macro-op grep)) (println display-name)
      (and grep macro-op (not (stream-line-matches? stream-factory grep))) nil
      md5 (md5-stream stream-factory display-name)
      sha1 (sha1-stream stream-factory display-name)
      cat (dump-stream stream-factory display-name opts)
      grep (grep-stream stream-factory display-name opts))))

(defn find-in-jar [^File f display-name opts]
  (try
    (with-open [^ZipFile zip (ZipFile. f)]
      (doseq [^ZipEntry entry (enumeration-seq (.entries zip))]
        (let [entry-name     (.getName entry)
              display-name   (str (str/trim display-name) "@" (str/trim entry-name))
              stream-factory #(.getInputStream zip entry)]
          (print-stream-matches opts entry-name display-name stream-factory))))
    (catch Exception e
      (println "error opening jar file" (.getPath f) ", error:" (.getMessage e))
      (throw e))))

(defn find-in-file [^File f ^File root opts]
  (when (valid-file? f opts)
    (let [display-name (if (:apath opts) (.getCanonicalPath f)
                                         (relative-path root f))
          file-name    (.getName f)]
      (if (-> file-name (.endsWith ".jar"))
        (find-in-jar f display-name opts)
        (let [stream-factory #(jio/input-stream f)]
          (print-stream-matches opts file-name display-name stream-factory))))))

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

(defn perform-file-scan [opts]
  (let [root (:search-root opts)
        opts (munge-regexes opts)]
    (doseq [^File f (filter #(-> % .isFile) (file-seq root))]
      (find-in-file f root opts))))

(defn -main [& args]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (perform-file-scan (assoc opts :search-root search-root)))))

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
