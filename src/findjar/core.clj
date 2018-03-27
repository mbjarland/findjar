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
  "a protocol definition to move all side effecting things
  to the edges of the findjar code. We send in a handler
  into the findjar entry point and all side effecting things
  are then handled by the handler"
  (warn [this file type msg opts]
    "called on errors during file scan. First argument is a java '
    file object, second a keyword indicating error type, third a
    message describing the issue, and fourth the full set of options
    used to start the file scan")
  (match [this display-name]
    "called when a normal (as in a non-grep) file name/path match
    is encountered. First argument is normally the path to the file,
    or for files within archives such as jar files, a display name
    of some sort (i.e. <jar-file-path>@<jar-entry-path>")
  (grep-match [this display-name line-nr hit? line opts]
    "called when a matching line is found in the content of a file
    (as opposed to matching on file name/path). See docstring for
    'match' for description of display name. The second argument
    is the line number in the containing file, the third argument
    is a boolean indicating whether this is the actual matching line
    or part of the 'context lines' surrounding the match (when using
    the -x flag for context). The fourth argument is the string data
    for the matching line and the last argument is the full set of options
    used to start the file scan")
  (dump-stream [this display-name stream-factory opts]
    "called to dump the entire contents of a file (when using the -c
    option) to some target location as specified by the opts. By default
    this will be either to stdout or to a target file (with the -o option),
    but protocol implementations can choose to do something else. See
    the docstring for 'match' for a description of the first argument. The
    second argument is a no-arguments function which will return an input
    stream to the matching file when called, the third argument is
    the full set of options used to start the file scan")
  (print-hash [this display-name hash-type hash-value opts]
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

(defmethod calculate-hash :md5
  ^{:cli {:desc "md5 hash"}}
  [_ stream-factory]
  (calculate-pandect-hash md5 stream-factory))

(defmethod calculate-hash :sha1
  ^{:cli {:desc "sha1 hash"}}
  [_ stream-factory]
  (calculate-pandect-hash sha1 stream-factory))


;(defn print-line [^String display-name ^long n ^Boolean hit? ^String line]
;  (println (str (str/trim display-name) ":" (inc n) (if hit? ">" ":") (str/trim line))))
;
;(defn md5-stream [stream-factory display-name]
;  (with-open [stream (stream-factory)]
;    (println (md5 stream) display-name)))
;
;(defn sha1-stream [stream-factory display-name]
;  (with-open [stream (stream-factory)]
;    (println (sha1 stream) display-name)))


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

(defn grep-stream [display-name stream-factory handler opts]
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
      ;[display-name line-number match? line]
      (doseq [[_ n hit? line] (sort-by second uniques)]
        (grep-match handler display-name n hit? line opts)))))

;(defn grep-stream-original [stream-factory display-name opts]
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
;            (print-line display-name cn (= cn n) l)))))))


(defn stream-line-matches? [stream-factory ^Pattern pattern]
  (with-open [stream (stream-factory)]
    (let [reader (jio/reader stream)]
      (some (fn [line] (re-find pattern line)) (line-seq reader)))))

(defn calculate-hashes [display-name stream-factory hashes handler opts]
  (doseq [h hashes]
    (let [hash (calculate-hash h stream-factory)]
      (print-hash handler display-name h hash opts))))

;; TODO: Match on paths - implement path and apath in the cond below
(defn print-stream-matches [opts file-name display-name stream-factory handler]
  (let [{:keys [name grep path apath cat hashes]} opts
        macro-op (or cat md5 sha1)]
    (cond
      (and apath (not (re-find apath display-name))) nil    ; no apath match
      (and path (not (re-find path display-name))) nil      ; no path match
      (and name (not (re-find name file-name))) nil         ; no name match
      (not (or macro-op grep)) (match handler display-name) ; normal non-grep match
      (and grep macro-op (not (stream-line-matches? stream-factory grep))) nil ;grep+macro and no matches -> nil
      hashes (calculate-hashes display-name stream-factory hashes handler opts)
      cat (dump-stream handler display-name stream-factory opts)
      grep (grep-stream display-name stream-factory handler opts))))


; TODO: make this a multi-method to enables more file formats
; TODO: make -type accept a set to support "-t fjg" for files, jar files, gzip files etc
(defn find-in-jar
  [^File f display-name opts handler]
  (try
    (with-open [^ZipFile zip (ZipFile. f)]
      (doseq [^ZipEntry entry (enumeration-seq (.entries zip))]
        (let [entry-path     (.getName entry)
              entry-name     (.getName (jio/file entry-path))
              display-name   (str (str/trim display-name) "@" (str/trim entry-path))
              stream-factory #(.getInputStream zip entry)]
          (print-stream-matches opts entry-name display-name stream-factory handler))))
    (catch Exception e
      (warn handler f :error-opening
            (str "error opening " (.getPath f) ", error:" (.getMessage e))
            opts))))

(defn default-dump-stream [stream-factory display-name opts]
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

;; extensible file type handlers
(comment
  (defmulti cli-file-type identity)
  (defmethod cli-file-type \j [_]
    {:ext     "jar"
     :desc    "jar files"
     :default true})

  (defmethod cli-file-type \z [_]
    {:ext     "zip"
     :desc    "zip files"
     :default false})

  (defmethod cli-file-type \d [_]
    {:ext     nil
     :desc    "disk files"
     :default true})
  )

;; extensible file hashing
(comment
  (defmulti cli-hash-type (fn [cli-type-char] cli-type-char))
  (defmethod cli-hash-type \m [_]
    {:id   :md5
     :desc "md5 hash"})
  (defmethod cli-hash-type \s [_]
    {:id   :sha1
     :desc "sha1 hash"})
  )


;;TODO: (:active-file-types opts) is a map of
;;TODO: active file types {"jar" blah "zip" blah}
(defn find-in-file [^File root ^File f handler opts]
  (when (valid-file? f opts handler)
    (let [display-name (if (:apath opts) (.getCanonicalPath f)
                                         (relative-path root f))]
      (file-type-scanner f display-name opts handler))))

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
  (let [opts  (munge-regexes opts)
        files (filter #(.isFile %) (file-seq search-root))]
    (doseq [^File f files]
      (find-in-file search-root f handler opts))))

(defn default-handler
  "returns an implementation of the FindJarHandler protocol. This moves all
  side-effecting things out of the rest of the code and into this single place.
  It also makes the rest of the code more testable and makes it possible for
  users of this code to modify the behavior by supplying their own handler"
  []
  (reify FindJarHandler
    (warn [_ file type msg opts]
      (println "WARN: can not read file -" (.getPath file)))

    (match [_ display-name]
      (println display-name))

    (grep-match [_ display-name line-nr hit? line opts]
      (println (str (str/trim display-name)
                    ":" (inc line-nr) (if hit? ">" ":")
                    (str/trim line))))

    (dump-stream [_ display-name stream-factory opts]
      (default-dump-stream stream-factory display-name opts)) ;;TODO: switch arg order

    (print-hash [_ display-name hash-type hash-value opts]
      (println hash-value display-name))))

(defn -main [& args]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        handler (default-handler)]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (perform-file-scan search-root handler opts))))

(defn repl-main [& args]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        handler (default-handler)]
    (prn :opts opts)
    (if (or exit-message true)
      (println "would exit with code " (if ok? 0 1) "msg," exit-message)
      (perform-file-scan search-root handler opts))))

(comment

  (repl-main "/home/mbjarland/projects/kpna/packages/ATG10.2/"
             "-n" "GLOBAL.properties"
             "-g" "logging")

  )
