(ns findjar.core
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]
            [pandect.algo.sha1 :refer [sha1]]
            [pandect.algo.md5 :refer [md5]]
            [pandect.algo.crc32 :refer [crc32]]
            ;[taoensso.tufte :as tufte :refer [p profiled profile defnp]]
            )
  (:import (java.util.zip ZipFile ZipEntry)
           (java.io File))
  (:gen-class))

(defprotocol FindJarHandler
  "a protocol definition to move all side effecting things
  to the edges of the findjar code. We send in a handler
  into the findjar entry point and all side effecting things
  are then handled by the handler"
  (warn [this file type msg ex opts]
    "called on errors during file scan. First argument is a java '
    file object, second a keyword indicating error type, third a
    message describing the issue, and fourth the full set of options
    used to start the file scan")
  (match [this path]
    "called when a normal (as in a non-grep) file name/path match
    is encountered")
  (grep-match [this max-line-# match opts]
    "called when a matching line is found in the content of a file
    (as opposed to matching only on file name/path). See docstring for
    'match' for description of path. The second argument
    is the max line number which will need to be displayed in
    the containing file (for padding the output), the third argument
    is a a map on the format:

    {:path s :line-# n :max-line-# p :line data :hit? b
     :start-col j :end-col k}

    The fourth argument is the full set of options used to start the
    file scan")
  (dump-stream [this path stream-factory opts]
    "called to dump the entire contents of a file (when using the -c
    option) to some target location as specified by the opts. By default
    this will be either to stdout or to a target file (with the -o option),
    but protocol implementations can choose to do something else. See
    the docstring for 'match' for a description of the first argument. The
    second argument is a no-arguments function which will return an input
    stream to the matching file when called, the third argument is
    the full set of options used to start the file scan")
  (print-hash [this path hash-type hash-value opts]
    "called to calculate and output a hash for a file. See the docstring
    for 'match' for a description of the first argument. See docstring
    for 'dump-stream' for a description of the second argument. The third
    argument is a map where the keys are keywords indicating the user
    requested hash operations (:md5 :sha1) and the values are the
    meta-data for these hash operations. The last argument is a the full
    set of options used to start the file scan"))

(defn relative-path [^File search-root ^File f]
  (subs
    (str/replace (.getCanonicalPath f)
                 (.getCanonicalPath search-root) "")
    1))

(defn calculate-pandect-hash
  "internal function to calculate pandect hashes"
  [pandect-fn stream-factory]
  (with-open [stream (stream-factory)]
    (pandect-fn stream)))

(defmulti calculate-hash (fn [id _ _ _] id))

(defmethod calculate-hash :md5 [_]
  {:fn   (fn [stream-factory]
           (calculate-pandect-hash md5 stream-factory))
   :desc "md5"})

(defmethod calculate-hash :sha1 [_]
  {:fn   (fn [stream-factory]
           (calculate-pandect-hash sha1 stream-factory))
   :desc "sha1"})

(defmethod calculate-hash :crc32 [_]
  {:fn   (fn [stream-factory]
           (calculate-pandect-hash crc32 stream-factory))
   :desc "crc32"})

(defn match-idxs [pattern str]
  (let [matcher (re-matcher pattern str)]
    (loop [r nil]
      (if (re-find matcher)
        (recur (conj (if (nil? r) [] r)
                     {:start (.start matcher)
                      :end   (.end matcher)}))
        r))))

(defn sliding->matches
  [path match-line-# context lines match-idxs]
  (keep
    (fn [[cn line]]
      (when line
        (let [hit? (= cn match-line-#)
              m    {:path   path
                    :line-# cn
                    :hit?   hit?
                    :line   line}]
          (if hit? (assoc m :match-idxs match-idxs) m))))
    (map-indexed #(vector (+ (- match-line-# context) %1) %2) lines)))

(defn matches-with-context [sliding context pattern path]
  (reduce
    (fn [a [match-# lines]]
      (let [match-idxs (match-idxs pattern (nth lines context))]
        (if match-idxs
          (concat a (sliding->matches path
                                      match-#
                                      context
                                      lines
                                      match-idxs))
          a)))
    []
    (map-indexed vector sliding)))

(defn remove-duplicated-lines
  "removes lines which are duplicated by the context lines
  window functionality. Multiple lines with the same line number
  should be folded into one and when possible, matching lines
  should win in this filter. Incoming matches are represented as
  {:path s :line-nr c :hit? h :start-col s :end-col d}"
  [matches]
  (reduce-kv
    (fn [a _ group]
      (if-let [ml (first (filter :hit? group))]
        (conj a ml)
        (conj a (first group))))
    []
    (group-by :line-# matches)))

(comment
  (grep-stream "path"
               #(jio/input-stream "test.txt")
               (findjar.main/default-handler)
               {:context 2
                :grep    #"22"})
  )

(defn grep-stream [path stream-factory handler opts]
  "iterate through the file using a sliding window of
  context lines before the 'current line' and context lines
  after, output result on console on matches"
  (with-open [stream (stream-factory)
              reader (jio/reader stream)]
    (let [s          (line-seq reader)
          pattern    (:grep opts)
          context    (or (:context opts) 0)
          window     (inc (* 2 context))
          pad        (repeat context nil)                   ;TODO: fix padding with empty string
          sliding    (partition window 1 (concat pad s pad))
          matches    (matches-with-context sliding context pattern path)
          uniques    (remove-duplicated-lines matches)
          max-line-# (apply max (map :line-# uniques))]
      ;[path line-number match? line]
      (doseq [match (sort-by :line-# uniques)]
        (grep-match handler max-line-# match opts)))))

;(defn grep-stream-original [stream-factory path opts]
;  "iterate through the file using a sliding window of
;  context lines before the 'current line' and context lines
;  after, output result on console on matches"
;  (with-open [stream (stream-factory)
;              reader (jio/reader stream)]
;    (let [s       (line-seq reader)
;          pattern (:grep opts)
;          context (or (:context opts) 0)
;          window  (inc (* 2 context))
;          pad     (repeat context nil)                      ;TODO: fix padding with empty string
;          sliding (partition window 1 (concat pad s pad))]
;      (doseq [[n lines] (map-indexed vector sliding)]
;        (when (re-find pattern (nth lines context))
;          (doseq [[cn l] (map-indexed #(vector (+ (- n context) %1) %2) lines)]
;            (print-line path cn (= cn n) l)))))))


(defn stream-line-matches? [stream-factory ^Pattern pattern]
  (with-open [stream (stream-factory)]
    (let [reader (jio/reader stream)]
      (some (fn [line] (re-find pattern line)) (line-seq reader)))))

(defn calculate-hashes [path stream-factory hashes handler opts]
  (doseq [h hashes]
    (let [hash (calculate-hash h stream-factory)]
      (print-hash handler path h hash opts))))

;; TODO: Match on paths - implement path and apath in the cond below
(defn print-stream-matches [opts file-name file-path stream-factory handler]
  (let [{:keys [name grep path apath cat hashes]} opts
        macro-op (or cat md5 sha1)]
    (cond
      (and apath (not (re-find apath file-path))) nil       ; no apath match
      (and path (not (re-find path file-path))) nil         ; no path match
      (and name (not (re-find name file-name))) nil         ; no name match
      (not (or macro-op grep)) (match handler file-path)    ; normal non-grep match
      (and grep macro-op (not (stream-line-matches? stream-factory grep))) nil ;grep+macro and no matches -> nil
      hashes (calculate-hashes file-path stream-factory hashes handler opts)
      cat (dump-stream handler file-path stream-factory opts)
      grep (grep-stream file-path stream-factory handler opts))))


; TODO: make sure all uses of stream-factory use with-open
; TODO: make this a multi-method to enables more file formats
; TODO: make -type accept a set to support "-t fjg" for files, jar files, gzip files etc
(defn find-in-jar
  [jar path opts handler]
  (when (pos? (.length jar))
    (try
      (with-open [^ZipFile zip (ZipFile. ^File jar)]
        (doseq [^ZipEntry entry (enumeration-seq (.entries zip))]
          (let [entry-path     (.getName entry)
                entry-name     (.getName (jio/file entry-path))
                path           (str (str/trim path) "@" (str/trim entry-path))
                stream-factory #(.getInputStream zip entry)]
            (print-stream-matches opts entry-name path stream-factory handler))))
      (catch Exception e
        (warn handler jar :error-opening
              (str (.getSimpleName (class e)) " opening " (.getPath jar) " - " (.getMessage e))
              e
              opts)))))

(defn default-dump-stream [stream-factory path opts]
  (let [of (:out-file opts)]
    (if of
      (do
        (with-open [w (jio/writer of :append true)]
          (.write w (str "<<<<<<< " path \newline))
          (jio/copy (stream-factory) w)
          (.write w (str ">>>>>>>" \newline)))
        (println path ">>" (.getPath of)))
      (with-open [stream (stream-factory)
                  reader (jio/reader stream)]
        (println "<<<<<<<" path)
        (let [s (line-seq reader)]
          (doseq [[n line] (map-indexed vector s)]
            (println (inc n) (str/trim line))))
        (println ">>>>>>>")))))

(defn file-ext [^File f]
  (let [p (.getCanonicalPath f)
        i (.lastIndexOf p ".")]
    (if (and (pos? i) (not (= (inc i) (count p))))
      (subs p (inc i))
      nil)))

(defn valid-file? [^File f default? opts handler]           ;;TODO: use multimethod file type
  (when (.isFile f)
    (let [active-types (:types opts)]
      (boolean
        (and (or default?
                 (some #(and (string? %)
                             (.endsWith (.getName f) %))
                       active-types))
             (or (.canRead f)
                 (do (warn handler f :can-not-read "WARN: can not read file" nil opts)
                     false)))))))

(defmulti file-type-scanner
          (fn [^File f] (file-ext f)))


(defmethod file-type-scanner "jar"
  [^File f]
  {:fn      (fn [path opts handler]
              (find-in-jar f path opts handler))
   :desc    "files in jar files"
   :default true
   :char    \j})

(defmethod file-type-scanner "zip"
  [^File f]
  {:fn      (fn [path opts handler]
              (find-in-jar f path opts handler))
   :desc    "files in zip files"
   :default false
   :char    \z})

(defmethod file-type-scanner :default
  [^File f]
  {:fn      (fn [path opts handler]
              (let [file-name      (.getName f)
                    stream-factory #(jio/input-stream f)]
                (print-stream-matches opts file-name path stream-factory handler)))
   :desc    "files on disk"
   :default true
   :char    \d})


;;TODO: (:active-file-types opts) is a map of
;;TODO: active file types {"jar" blah "zip" blah}
(defn find-in-file [^File root ^File f handler opts]
  (let [path    (if (:apath opts) (.getCanonicalPath f)
                                  (relative-path root f))
        scanner (:fn (file-type-scanner f))]
    (scanner path opts handler)))

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

(defn perform-file-scan [search-root handler opts]
  (let [default? (some #{:default} (:types opts))
        opts     (munge-regexes opts)
        files    (filter #(valid-file? % default? opts handler) (file-seq search-root))]
    (doseq [^File f files]
      :find-in-file (find-in-file search-root f handler opts))))

