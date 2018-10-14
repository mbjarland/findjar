(ns findjar.core
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]
            [pandect.algo.sha1 :refer [sha1]]
            [pandect.algo.md5 :refer [md5]]
            [pandect.algo.crc32 :refer [crc32]]
            [taoensso.tufte :as tufte :refer [p profiled profile defnp]]
            )
  (:import (java.util.zip ZipFile ZipEntry)
           (java.io File)
           [java.util Map])
  (:gen-class))

(defprotocol FindJarHandler
  "a protocol definition to move all side effecting things
  to the edges of the findjar code. We send in a handler
  into the findjar entry point and all side effecting things
  are then handled by the handler"
  (warn [this msg ex]
    "called on errors during file scan. First argument is a java '
    file object, second a keyword indicating error type, third a
    message describing the issue, and fourth the full set of options
    used to start the file scan")
  (match [this path]
    "called when a normal (as in a non-grep) file name/path match
    is encountered")
  (grep-match [this max-line-# line-map opts]
    "called when a matching line is found in the content of a file
    (as opposed to matching only on file name/path). See docstring for
    'match' for description of path. The second argument
    is the max line number which will need to be displayed in
    the containing file (for padding the output), the third argument
    is a a map on the format:

    {:path p :line-# n :hit? true :line data
     :match-idxs [{:start 14, :end 16}
                  {:start 16, :end 18}
                  {:start 18, :end 20}]}]

    where the match-idxs is only present if the line is a hit.

    The fourth argument is the full set of options used to start the
    file scan")
  (dump-stream [this path content-provider opts]
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
  [pandect-fn content-provider]
  (content-provider (fn [stream] (pandect-fn stream)) nil))

(defmulti calculate-hash (fn [id] id))

(defmethod calculate-hash :md5 [_]
  {:fn   (fn [content-provider]
           (calculate-pandect-hash md5 content-provider))
   :desc "md5"})

(defmethod calculate-hash :sha1 [_]
  {:fn   (fn [content-provider]
           (calculate-pandect-hash sha1 content-provider))
   :desc "sha1"})

(defmethod calculate-hash :crc32 [_]
  {:fn   (fn [content-provider]
           (calculate-pandect-hash crc32 content-provider))
   :desc "crc32"})

(defn match-idxs [pattern str]
  (let [matcher (re-matcher pattern str)]
    (loop [r nil]
      (if (re-find matcher)
        (recur (conj (if (nil? r) [] r)
                     {:start (.start matcher)
                      :end   (.end matcher)}))
        r))))

(defn window->matching-lines
  ""
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

(defn dedupe-line-maps
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


(defn find-line-maps-with-context
  "takes a sliding window of lines, a number indicating the number of
  context lines, a regex pattern to match for, a path to use for displaying
  output and generates a collection of 'matching lines' represented as maps on the
  following format (context 2) :

    ({:path 'path', :line-# 0,  :hit? false, :line '1111'}
     {:path 'path', :line-# 1,  :hit? true,  :line '2222', :match-idxs [{:start 0, :end 2} {:start 2, :end 4}]}
     {:path 'path', :line-# 2,  :hit? false, :line '3333'}
     {:path 'path', :line-# 3,  :hit? false, :line 'to be222or not222to be'}
     {:path 'path', :line-# 1,  :hit? false, :line '2222'}
     {:path 'path', :line-# 2,  :hit? false, :line '3333'}
     {:path 'path', :line-# 3,  :hit? true,  :line 'to be222or not222to be', :match-idxs [{:start 5, :end 7} {:start 14, :end 16}]}
     {:path 'path', :line-# 4,  :hit? false, :line '5555'}
     {:path 'path', :line-# 5,  :hit? false, :line '6666'}
     ...)

  the argument sliding window is represented as a list of lists:

   '('(nil nil '1111' '2222' '3333') '(nil '1111' '2222' '3333'...) ...)

  where 1111 is the content on the first line etc."
  [sliding context pattern path]
  (reduce
   (fn [a [window-# lines]]
     (let [match-idxs (match-idxs pattern (nth lines context))]
       (if match-idxs                                       ; if there was a match
         (concat a (window->matching-lines path
                                           window-#
                                           context
                                           lines
                                           match-idxs))
         a)))
   []
   (map-indexed vector sliding)))

(comment
 (grep-stream "path"
              #(jio/input-stream "test.txt")
              (findjar.main/default-handler)
              {:context 2
               :grep    #"22"})

 (grep-stream "path"
              #(jio/input-stream "test.txt")
              (findjar.main/default-handler)
              {:context 2
               :grep    #"12345"})

 )

(defn grep-stream [path content-provider handler opts]
  "iterate through the file using a sliding window of
  context lines before the 'current line' and context lines
  after, output result on console on matches"
  (content-provider
   nil
   (fn [reader]
     (let [s         (line-seq reader)
           pattern   (:grep opts)
           context   (or (:context opts) 0)
           window    (inc (* 2 context))
           pad       (repeat context nil)                   ;TODO: fix padding with empty string
           sliding   (partition window 1 (concat pad s pad))
           line-maps (find-line-maps-with-context sliding context pattern path)]
       (when (not-empty line-maps)
         (let [uniques    (dedupe-line-maps line-maps)
               max-line-# (reduce max (map :line-# uniques))]
           ;[path line-number match? line]
           (doseq [line-map (sort-by :line-# uniques)]
             (grep-match handler max-line-# line-map opts))))))))

(defn stream-line-matches2? [stream-factory pattern]
  (with-open [stream (stream-factory)]
    (let [reader (jio/reader stream)]
      (some (fn [line] (re-find pattern line)) (line-seq reader)))))

(defn stream-line-matches? [content-provider pattern]
  (content-provider
   nil
   (fn [reader]
     (some (fn [line] (re-find pattern line)) (line-seq reader)))))

(defn calculate-hashes
  "hash-types is a coll of keywords :md5 :sha1 etc"
  [path content-provider hash-types handler opts]
  (doseq [hash-type hash-types]
    (let [hash-fn    (:fn (calculate-hash hash-type))
          hash-value (hash-fn content-provider)]
      (print-hash handler path hash-type hash-value opts))))

(defn print-stream-matches
  "this method is central to the findjar functionality.
   It takes the options, file name and path, stream factory and a
   handler implementing the protocol defined above and executes
   searches based on the provided data"
  [opts file-name file-path content-provider handler]
  (let [{:keys [name grep path apath cat hash]} opts
        macro-op (or cat hash)]
    (cond
      (and name (not (re-find name file-name))) nil         ; no name match -> exit
      (and path (not (re-find path file-path))) nil         ; no path match -> exit
      (and apath (not (re-find apath file-path))) nil       ; no apath match -> exit
      (not (or macro-op grep)) (match handler file-path)    ; normal non-grep match
      (and grep macro-op (not (stream-line-matches? content-provider grep))) nil ;grep+macro and no matches -> nil
      hash (calculate-hashes file-path content-provider hash handler opts)
      cat (dump-stream handler file-path content-provider opts)
      grep (grep-stream file-path content-provider handler opts))))

(defn make-content-provider [stream-factory handler]
  (fn [stream-handler reader-handler]
    (let [rh (fn [stream]
               (with-open [reader (jio/reader stream)]
                 (reader-handler reader)))]
      (try
        (with-open [stream (stream-factory)]
          (cond
            (and stream-handler reader-handler) (do (stream-handler stream) (rh stream))
            stream-handler (stream-handler stream)
            reader-handler (rh stream)))
        (catch Exception e
          (warn handler (.getMessage e) e)
          nil)))))


; TODO: make this a multi-method to enables more file formats
; TODO: make -type accept a set to support "-t fjg" for files, jar files, gzip files etc
(defn find-in-jar
  [jar path opts handler]
  (when (pos? (.length jar))
    (try
      (with-open [^ZipFile zip (ZipFile. ^File jar)]
        (doseq [^ZipEntry entry (enumeration-seq (.entries zip))]
          (let [entry-path       (.getName entry)
                entry-name       (.getName (jio/file entry-path))
                path             (str (str/trim path) "@" (str/trim entry-path))
                stream-factory   #(.getInputStream zip entry)
                content-provider (make-content-provider stream-factory handler)]
            (print-stream-matches opts entry-name path content-provider handler))))
      (catch Exception e
        (warn handler
              (str (.getSimpleName (class e)) " opening " (.getPath jar) " - " (.getMessage e))
              e)))))

(defn file-ext [^File f]
  (let [n (.getName f)
        i (.lastIndexOf n (int \.))]
    (when (and (pos? i) (not (= (inc i) (count n))))
      (subs n (inc i)))))

(defn valid-file-fn [opts handler]                          ;;TODO: use multimethod file type
  (let [search-in-disk-files (some #{:default} (:types opts))
        active-types         (:types opts)]                 ;types is a list [:default "jar" "zip"] etc
    (fn [^File f]
      (p :0-check-valid-file
         (when (p :0-valid-file-is-file (.isFile f))
           (boolean
            (or search-in-disk-files
                (some #(and (string? %) (.endsWith (.getName f) %))
                      active-types))))))))

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
              (let [file-name        (.getName f)
                    stream-factory   #(jio/input-stream f)
                    content-provider (make-content-provider stream-factory handler)]
                (print-stream-matches opts file-name path content-provider handler)))
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
  (let [default?    (some #{:default} (:types opts))
        opts        (munge-regexes opts)
        valid-file? (valid-file-fn opts handler)
        files       (filter #(p :0-find-valid-files
                                (valid-file? %))
                            (file-seq search-root))]
    (p :0-iterate-files
       (doseq [f files]
         (p :1-find-in-file (find-in-file search-root f handler opts))))))

