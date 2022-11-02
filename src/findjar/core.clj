(ns findjar.core
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [findjar.hash :as hash])
  (:import [java.io File]
           [java.util.zip ZipEntry ZipFile])
  (:gen-class))

(defprotocol FileContent
  "a protocol definition to represent file content from either normal disk files
  or files within zip/jar archives. As we use the protocol FindJarOutput to push
  the output stdout to the edges of the system and stdout out of this namespace,
  we similarly need an abstraction for retrieving the contents of a file to be
  able to hash/display lines/dump etc file contents.

  This protocol allows us to create a 'content provider' which implements this
  protocol and assuming you get handed a content provider (file-content in this
  case) to a specific file, it allows you to do the following:

    (as-stream file-content #(do-something-with-file-content-stream %))
    (as-reader file-content #(do-something-with-file-content-reader %))

  where the stream handed to the anonymous function is an InputStream to the
  file contents (either on disk or inside a zip/jar archive) and the reader
  is similar but for receiving the content in text format."
  (as-stream [this stream-handler]
    "called when you want to read the file contents as a binary stream.
    stream-handler is a one argument function which will receive an open InputStream
    to the file contents as the argument. It is not the responsibility of the
    stream-handler to close the stream.")
  (as-reader [this reader-handler]
    "called when you want to read the file contents as a reader of text data.
    reader-handler is a one argument function which will receive an open java.io.Reader
    to the file contents as the argument. It is not the responsibility of the
    stream-handler to close the reader."))

(defprotocol FindJarOutput
  "a protocol definition to move all side effecting things (mainly io to stdout)
  to the edges of the findjar code. We send in an output handler implementation
  into the findjar entry point and all side effecting things are then handled by
  the output handler leaving the code in this namespace less polluted by
  side effects."
  (warn [this msg ex opts]
    "called on errors during file scan. First argument is a java
    file object, second a keyword indicating error type, third a
    message describing the issue, and fourth the full set of options
    used to start the file scan")
  (match [this path opts]
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
  (dump-stream [this path file-content opts]
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

(defmulti calculate-hash (fn [id] id))

(defmethod calculate-hash :md5 [_]
  {:fn   (fn [file-content] (as-stream file-content #(hash/digest "MD5" %)))
   :desc "md5"})

(defmethod calculate-hash :sha1 [_]
  {:fn   (fn [file-content] (as-stream file-content #(hash/digest "SHA-1" %)))
   :desc "sha1"})

(defmethod calculate-hash :sha256 [_]
  {:fn   (fn [file-content] (as-stream file-content #(hash/digest "SHA-256" %)))
   :desc "sha256"})

(defmethod calculate-hash :sha512 [_]
  {:fn   (fn [file-content] (as-stream file-content #(hash/digest "SHA-512" %)))
   :desc "sha512"})

(defmethod calculate-hash :crc32 [_]
  {:fn   (fn [file-content] (as-stream file-content #(hash/crc-32 %)))
   :desc "crc32"})

(defn match-idxs
  ""
  [pattern str]
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
      (if-let [match-idxs (match-idxs pattern (nth lines context))]
        (concat a (window->matching-lines path              ; if there was a match
                                          window-#
                                          context
                                          lines
                                          match-idxs))
        a))
    []
    (map-indexed vector sliding)))

(comment
  (grep-stream "path"
               #(jio/input-stream "test.txt")
               (findjar.main/default-output)
               {:context 2
                :grep    #"22"})

  (grep-stream "path"
               #(jio/input-stream "test.txt")
               (findjar.main/default-output)
               {:context 2
                :grep    #"12345"})

  )

(defn grep-stream
  "iterate through the file using a sliding window of
  context lines before the 'current line' and context lines
  after, output result on console on matches"
  [output path file-content opts]
  (as-reader
    file-content
    (fn [reader]
      (let [s         (line-seq reader)
            pattern   (:grep opts)
            context   (or (:context opts) 0)
            window    (inc (* 2 context))
            pad       (repeat context nil)                  ;TODO: fix padding with empty string
            sliding   (partition window 1 (concat pad s pad))
            line-maps (find-line-maps-with-context sliding context pattern path)]
        (when (not-empty line-maps)
          (let [uniques    (dedupe-line-maps line-maps)
                max-line-# (reduce max (map :line-# uniques))]
            ;[path line-number match? line]
            (doseq [line-map (sort-by :line-# uniques)]
              (grep-match output max-line-# line-map opts))))))))

(defn stream-line-matches?
  ""
  [file-content pattern]
  (as-reader
    file-content
    (fn [reader]
      (some (fn [line] (re-find pattern line)) (line-seq reader)))))

(defn calculate-hashes
  "hash-types is a coll of keywords :md5 :sha1 etc"
  [output path file-content hash-types opts]
  (doseq [hash-type hash-types]
    (let [hash-fn    (:fn (calculate-hash hash-type))
          hash-value (hash-fn file-content)]
      (print-hash output path hash-type hash-value opts))))

(defn print-stream-matches
  "this method is central to the findjar functionality.
   It takes the output handler implementing the output protocol
   defined above, options, file name and path, and a stream factory
   and executes searches based on the provided data"
  [output opts file-name file-path file-content]
  (let [{:keys [name grep path apath cat hash]} opts
        macro-op (or cat hash)]
    (cond
      (and name (not (re-find name file-name))) nil         ; no name match -> exit
      (and path (not (re-find path file-path))) nil         ; no path match -> exit
      (and apath (not (re-find apath file-path))) nil       ; no apath match -> exit
      (not (or macro-op grep)) (match output file-path opts) ; normal non-grep match
      (and grep macro-op (not (stream-line-matches? file-content grep))) nil ;grep+macro and no matches -> nil
      hash (calculate-hashes output file-path file-content hash opts)
      cat (dump-stream output file-path file-content opts)
      grep (grep-stream output file-path file-content opts))))

(defn wrap-file-content
  ""
  [stream-factory output opts]
  (reify FileContent
    (as-stream [_ stream-handler]
      (try
        (with-open [stream (stream-factory)]
          (stream-handler stream))
        (catch Exception e
          (warn output (.getMessage e) e opts)
          nil)))
    (as-reader [this reader-handler]
      (as-stream this (fn [stream]
                        (with-open [reader (jio/reader stream)]
                          (reader-handler reader)))))))

(def ^Integer slash (int \/))

(defn name-part
  "given a/b/c.txt return c.txt"
  [^String path]
  (let [i (.lastIndexOf path slash)]
    (if (= i -1) path (subs path i))))

; TODO: make -type accept a set to support "-t fjg" for files, jar files, gzip files etc
(defn find-in-jar
  [^File jar ^String path opts output]
  (when (pos? (.length jar))
    (let [prefix (str (str/trim path) \@)]
      (try
        (with-open [^ZipFile zip (ZipFile. ^File jar)]
          (doseq [^ZipEntry entry (enumeration-seq (.entries zip))]
            (let [entry-path     (.getName entry)
                  entry-name     (name-part entry-path)     ;(.getName (jio/file entry-path)))
                  jar-path       (.toString (.append (StringBuilder. prefix) entry-path)) ; (str prefix entry-path))
                  stream-factory #(.getInputStream zip entry)
                  file-content   (wrap-file-content stream-factory output opts)]
              (print-stream-matches output opts entry-name jar-path file-content))))
        (catch Exception e
          (warn output
                (str (.getSimpleName (class e)) " opening " (.getPath jar) " - " (.getMessage e))
                e
                opts))))))

(defn file-ext [^File f]
  (let [n (.getName f)
        i (.lastIndexOf n (int \.))]
    (when (and (pos? i) (not (= (inc i) (count n))))
      (subs n (inc i)))))

(defn valid-file-fn [opts]                                  ;;TODO: prevent in-jar search when disk files only
  (let [active-types (:types opts)                          ;types is a set #{:default "jar" "zip"} etc
        exts         (remove #{:default} active-types)]
    (fn [^File f]
      (when (.isFile f)
        (boolean
          (or (active-types :default)                       ;; if disk files -> need to let everything through for name/path matching
              (some #(.endsWith (.getName f) %) exts)))))))

;; cases
;  file           flags      result                      exception
;  ----           -----      ------
;  normal         d          default
;  normal         jz         <excluded by file filters>
;  jar            d          default                     x
;  jar            dz         default                     x
;  jar            j          jar
;  jar            d          default                     x
;  zip            dz         default                     x
;  zip            j          jar
;  zip            j          jar
;  zip            j          jar

(defmulti file-finder
  (fn [{:keys [^File file types]}]
    (let [ext (file-ext file)]
      (if (and (#{"jar" "zip"} ext)
               (not (types ext)))
        :default
        ext
        ))))

(comment
  (file-finder {:file (clojure.java.io/file "bob.jar")})
  )

(defmethod file-finder "jar"
  [{:keys [file]}]
  {:fn      (fn [^String path opts output]
              (find-in-jar file path opts output))
   :desc    "files in jar files"
   :default true
   :char    \j})

(defmethod file-finder "zip"
  [{:keys [file]}]
  {:fn      (fn [^String path opts output]
              (find-in-jar file path opts output))
   :desc    "files in zip files"
   :default false
   :char    \z})

(defmethod file-finder :default
  [{:keys [file]}]
  {:fn      (fn [^String path opts output]
              (let [file-name      (.getName file)
                    stream-factory #(jio/input-stream file)
                    file-content   (wrap-file-content stream-factory output opts)]
                (print-stream-matches output opts file-name path file-content)))
   :desc    "files on disk"
   :default true
   :char    \d})


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

(defn perform-scan [search-root output opts]
  (let [opts      (munge-regexes opts)
        valid-fn  (valid-file-fn opts)
        files     (filter #(valid-fn %)
                          (file-seq search-root))
        root-len  (inc (count (.getPath search-root)))
        absolute? (:apath opts)
        to-path   (fn [^File f]
                    (if absolute? (.getCanonicalPath f)
                      (subs (.getPath f) root-len)))]
    (doseq [f files]
      (let [finder (:fn (file-finder (assoc opts :file f)))
            path   (to-path f)]
        (finder path opts output)))))
