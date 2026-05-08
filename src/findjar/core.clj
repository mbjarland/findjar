(ns findjar.core
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [findjar.hash :as hash]
            [findjar.protocols :as p])
  (:import [java.io File InputStream]
           [java.util.regex Pattern]
           [java.util.zip ZipEntry ZipFile]))

;;;; ---------------------------------------------------------------------------
;;;; Stream helpers — replace the old FileContent protocol.
;;;; A "stream-factory" is a 0-arg fn that returns a fresh InputStream each
;;;; time it's called. Helpers manage with-open and warn-on-error so callers
;;;; don't repeat the boilerplate.

(defn with-stream
  "Open the stream produced by stream-factory, hand it to f, close it.
  On exception, call (warn output msg ex opts) and return nil."
  [output opts stream-factory f]
  (try
    (with-open [^InputStream s (stream-factory)]
      (f s))
    (catch Exception e
      (p/warn output (.getMessage e) e opts)
      nil)))

(defn with-reader
  "Open stream-factory as a reader, hand it to f, close it. Same error
  semantics as with-stream."
  [output opts stream-factory f]
  (with-stream output opts stream-factory
    (fn [s] (with-open [r (jio/reader s)] (f r)))))

;;;; ---------------------------------------------------------------------------
;;;; Hash algorithm registry — plain map keyed by algorithm keyword.
;;;; Each entry: {:digest (fn [InputStream] -> hex-string)
;;;;              :desc   "string shown in CLI"}

(def hash-algorithms
  {:md5    {:digest #(hash/digest "MD5"     %) :desc "md5"}
   :sha1   {:digest #(hash/digest "SHA-1"   %) :desc "sha1"}
   :sha256 {:digest #(hash/digest "SHA-256" %) :desc "sha256"}
   :sha512 {:digest #(hash/digest "SHA-512" %) :desc "sha512"}
   :crc32  {:digest hash/crc-32               :desc "crc32"}})

(defn hash-by-desc
  "Return the algorithm keyword whose :desc matches s (the user-facing
  string from the CLI), or nil."
  [s]
  (some (fn [[k {:keys [desc]}]] (when (= desc s) k)) hash-algorithms))

;;;; ---------------------------------------------------------------------------
;;;; Regex helpers

(defn match-idxs
  "Return a vector of {:start i :end j} for every match of pattern in s.
  Empty when there are no matches."
  [^Pattern pattern ^String s]
  (let [m (re-matcher pattern s)]
    (loop [acc (transient [])]
      (if (.find m)
        (recur (conj! acc {:start (.start m) :end (.end m)}))
        (persistent! acc)))))

(def ^:private flag-bits
  ;; java.util.regex.Pattern compile-time flag bits
  {\i Pattern/CASE_INSENSITIVE
   \m Pattern/MULTILINE
   \s Pattern/DOTALL
   \u Pattern/UNICODE_CASE
   \x Pattern/COMMENTS
   \d Pattern/UNIX_LINES})

(defn- compile-with-flags ^Pattern [^Pattern pattern flags]
  (let [bits (reduce (fn [acc c] (bit-or acc (get flag-bits c 0))) 0 flags)]
    (Pattern/compile (.pattern pattern) bits)))

(defn munge-regexes
  "Apply the user-supplied regex flags (string of single-char flags) to all
  pattern opts in opts."
  [opts]
  (if-let [flags (some-> opts :flags seq)]
    (reduce (fn [acc k]
              (if-let [^Pattern v (get acc k)]
                (assoc acc k (compile-with-flags v flags))
                acc))
            opts
            [:name :grep :path :apath])
    opts))

;;;; ---------------------------------------------------------------------------
;;;; Grep — sliding-window context

(defn- window->matching-lines [path match-line-# context lines match-idxs]
  (keep
    (fn [[cn line]]
      (when line
        (let [hit? (= cn match-line-#)
              m    {:path path :line-# cn :hit? hit? :line line}]
          (cond-> m hit? (assoc :match-idxs match-idxs)))))
    (map-indexed #(vector (+ (- match-line-# context) %1) %2) lines)))

(defn- dedupe-line-maps
  "Multiple context windows may emit the same :line-#. Fold duplicates,
  preferring the one with :hit? true."
  [matches]
  (reduce-kv
    (fn [a _ group]
      (conj a (or (first (filter :hit? group))
                  (first group))))
    []
    (group-by :line-# matches)))

(defn- find-line-maps-with-context [sliding context pattern path]
  (reduce
    (fn [a [window-# lines]]
      (let [idxs (match-idxs pattern (nth lines context))]
        (if (seq idxs)
          (concat a (window->matching-lines path window-# context lines idxs))
          a)))
    []
    (map-indexed vector sliding)))

(defn grep-stream
  "Read file-content via stream-factory, slide a (1+2*context) window over
  its lines, emit p/grep-match for every matching line + context."
  [output path stream-factory opts]
  (with-reader output opts stream-factory
    (fn [reader]
      (let [pattern   (:grep opts)
            context   (or (:context opts) 0)
            window    (inc (* 2 context))
            pad       (repeat context nil)
            sliding   (partition window 1 (concat pad (line-seq reader) pad))
            line-maps (find-line-maps-with-context sliding context pattern path)]
        (when (seq line-maps)
          (let [uniques    (dedupe-line-maps line-maps)
                max-line-# (reduce max (map :line-# uniques))]
            (doseq [line-map (sort-by :line-# uniques)]
              (p/grep-match output max-line-# line-map opts))))))))

(defn- stream-line-matches? [output opts stream-factory pattern]
  (with-reader output opts stream-factory
    (fn [reader] (some #(re-find pattern %) (line-seq reader)))))

(defn- calculate-hashes [output path stream-factory hash-types opts]
  (doseq [hash-type hash-types]
    (when-let [{:keys [digest]} (get hash-algorithms hash-type)]
      (when-let [hash-value (with-stream output opts stream-factory digest)]
        (p/print-hash output path hash-type hash-value opts)))))

;;;; ---------------------------------------------------------------------------
;;;; Path / file helpers

(defn name-part
  "Return the final path segment of a forward-slash-delimited path."
  [^String path]
  (let [i (.lastIndexOf path (int \/))]
    (if (neg? i) path (subs path (inc i)))))

(defn file-ext
  "Lowercase extension (without the dot) of a File, or nil if it has none."
  [^File f]
  (let [n (.getName f)
        i (.lastIndexOf n (int \.))]
    (when (and (pos? i) (not= (inc i) (count n)))
      (str/lower-case (subs n (inc i))))))

;;;; ---------------------------------------------------------------------------
;;;; Cat materialization is delegated to a render fn passed in from main.
;;;; The render fn signature is: (fn [path stream-factory opts] -> String)
;;;; This lets buffering outputs materialize content while the source jar is
;;;; still open, while keeping ANSI/formatting concerns out of core.

(defn- handle-match
  "Common dispatcher: given a match candidate (file or jar entry), apply name/
  path/apath filters and then run the requested operation."
  [output opts file-name file-path stream-factory render-cat]
  (let [{:keys [name grep path apath cat hash]} opts
        macro-op (or cat hash)]
    (cond
      (and name  (not (re-find name  file-name))) nil
      (and path  (not (re-find path  file-path))) nil
      (and apath (not (re-find apath file-path))) nil
      (not (or macro-op grep))
      (p/match output file-path opts)

      (and grep macro-op
           (not (stream-line-matches? output opts stream-factory grep))) nil

      hash (calculate-hashes output file-path stream-factory hash opts)
      cat  (when-let [s (render-cat file-path stream-factory opts)]
             (p/dump-stream output file-path s opts))
      grep (grep-stream output file-path stream-factory opts))))

;;;; ---------------------------------------------------------------------------
;;;; File-type registry — replaces defmulti file-finder

(declare scan-jar scan-disk-file)

(def file-finders
  "Registry of file-type handlers. Keys are file extensions (lowercased) or
  :default for normal disk files. Each entry has:
    :scan    (fn [^File f rel-path opts output render-cat] -> nil)
    :desc    String shown in CLI help
    :default Whether this type is searched when -t is not given
    :char    Single-char selector used by -t"
  {:default {:scan    (fn [f path opts output render-cat]
                        (scan-disk-file f path opts output render-cat))
             :desc    "normal files"
             :default true
             :char    \n}
   "jar"    {:scan    (fn [f path opts output render-cat]
                        (scan-jar f path opts output render-cat))
             :desc    "files in jar files"
             :default true
             :char    \j}
   "zip"    {:scan    (fn [f path opts output render-cat]
                        (scan-jar f path opts output render-cat))
             :desc    "files in zip files"
             :default false
             :char    \z}})

(defn- finder-for
  "Pick a file-finder entry for f, given the active set of file types."
  [^File f types]
  (let [ext (file-ext f)]
    (cond
      (and ext (contains? file-finders ext) (contains? types ext))
      (get file-finders ext)

      :else
      (get file-finders :default))))

;;;; ---------------------------------------------------------------------------
;;;; Scanners

(defn- scan-disk-file [^File f path opts output render-cat]
  (let [stream-factory #(jio/input-stream f)]
    (handle-match output opts (.getName f) path stream-factory render-cat)))

(defn- scan-jar [^File jar ^String jar-path opts output render-cat]
  (when (pos? (.length jar))
    (let [prefix (str (str/trim jar-path) \@)]
      (try
        (with-open [^ZipFile zip (ZipFile. ^File jar)]
          (doseq [^ZipEntry entry (enumeration-seq (.entries zip))]
            (let [entry-path     (.getName entry)
                  entry-name     (name-part entry-path)
                  full-path      (str prefix entry-path)
                  stream-factory #(.getInputStream zip entry)]
              (handle-match output opts entry-name full-path stream-factory render-cat))))
        (catch Exception e
          (p/warn output
                  (str (.getSimpleName (class e)) " opening " (.getPath jar)
                       " - " (.getMessage e))
                  e
                  opts))))))

;;;; ---------------------------------------------------------------------------
;;;; Active-types filter — only candidates whose extension is in the active
;;;; set, plus normal disk files when :default is active.

(defn- valid-file-fn
  "Returns a predicate over java.io.File that decides whether the file should
  even be considered for scanning."
  [opts]
  (let [active-types (:types opts)
        active-exts  (set (remove #{:default} active-types))]
    (fn [^File f]
      (and (.isFile f)
           (boolean
             (or (active-types :default)
                 (contains? active-exts (file-ext f))))))))

;;;; ---------------------------------------------------------------------------
;;;; Top-level scan

(defn- relative-path
  "Convert an absolute File path to one relative to search-root. Tolerates
  trailing separators on search-root."
  [^File search-root]
  (let [root (.getPath search-root)
        len  (cond-> (count root)
               (not (str/ends-with? root File/separator)) inc)]
    (fn [^File f] (subs (.getPath f) len))))

(defn path-fn
  "Return a fn File -> String producing either the canonical absolute path
  or a path relative to search-root, depending on (:apath opts)."
  [^File search-root opts]
  (if (:apath opts)
    (fn [^File f] (.getCanonicalPath f))
    (relative-path search-root)))

(defn candidate-files
  "The lazy seq of files (under search-root) whose extension is permitted by
  the active --types set. Pre-munge opts before calling."
  [^File search-root opts]
  (filter (valid-file-fn opts) (file-seq search-root)))

(defn scan-file
  "Scan a single File against output/render-cat with already-munged opts.
  display-path is the path string (relative or absolute) to surface to the
  user."
  [output render-cat opts ^File f display-path]
  (let [{:keys [scan]} (finder-for f (:types opts))]
    (scan f display-path opts output render-cat)))

(defn perform-scan
  "Serial scanner. Walks search-root and dispatches each candidate file to its
  registered finder. output is a FindJarOutput sink. render-cat is the
  cat-rendering fn supplied by main (so core stays free of ANSI/formatting
  concerns)."
  [^File search-root output render-cat opts]
  (let [opts    (munge-regexes opts)
        to-path (path-fn search-root opts)]
    (doseq [f (candidate-files search-root opts)]
      (scan-file output render-cat opts f (to-path f)))))
