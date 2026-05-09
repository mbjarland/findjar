(ns findjar.core
  (:require [clojure.java.io :as jio]
            [clojure.set :as set]
            [clojure.string :as str]
            [findjar.hash :as hash]
            [findjar.protocols :as p])
  (:import [java.io File InputStream]
           [java.nio.file Files LinkOption]
           [java.util.regex Pattern]
           [java.util.zip ZipEntry ZipFile ZipInputStream]))

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

(defn unknown-flag-chars
  "Return a sorted set of characters in flags that are not recognized
  Pattern flags. Empty when all chars are known."
  [^String flags]
  (->> flags
       (remove #(contains? flag-bits %))
       (into (sorted-set))))

(defn- compile-with-flags ^Pattern [^Pattern pattern flags]
  (let [bits (reduce (fn [acc c] (bit-or acc (get flag-bits c 0))) 0 flags)]
    (Pattern/compile (.pattern pattern) bits)))

(defn munge-regexes
  "Apply the user-supplied regex flags (string of single-char flags) to all
  pattern opts in opts."
  [opts]
  (if-let [flags (not-empty (:flags opts))]
    (reduce (fn [acc k]
              (if-let [^Pattern v (get acc k)]
                (assoc acc k (compile-with-flags v flags))
                acc))
            opts
            [:name :grep :path :apath])
    opts))

;;;; ---------------------------------------------------------------------------
;;;; Grep — sliding-window context

(defn- window->matching-lines
  "Build the line-maps for a single hit at match-line-#. before/after specify
  the asymmetric context width. center-idx points at the hit within `lines`."
  [path match-line-# before lines center-idx match-idxs]
  (keep
    (fn [[cn line]]
      (when line
        (let [hit? (= cn match-line-#)
              m    {:path path :line-# cn :hit? hit? :line line}]
          (cond-> m hit? (assoc :match-idxs match-idxs)))))
    (map-indexed #(vector (+ (- match-line-# before) %1) %2) lines)))

(defn- dedupe-line-maps
  "Multiple context windows may emit the same :line-#. Fold duplicates,
  preferring the one with :hit? true."
  [matches]
  (mapv (fn [group]
          (or (first (filter :hit? group)) (first group)))
        (vals (group-by :line-# matches))))

(defn- find-line-maps-with-context [sliding before pattern path]
  (reduce
    (fn [a [window-# lines]]
      (let [idxs (match-idxs pattern (nth lines before))]
        (if (seq idxs)
          (into a (window->matching-lines path window-# before lines before idxs))
          a)))
    []
    (map-indexed vector sliding)))

(defn- effective-context
  "Resolve -A/-B/-x into [before after]. Explicit -A or -B win; if only -x
  is given, both before and after default to it."
  [opts]
  (let [x (or (:context opts) 0)
        a (:after opts)
        b (:before opts)]
    [(or b x) (or a x)]))

(defn grep-stream
  "Read file-content via stream-factory, slide a (1+before+after) window
  over its lines, emit p/grep-match for every matching line + context."
  [output path stream-factory opts]
  (with-reader output opts stream-factory
    (fn [reader]
      (let [pattern         (:grep opts)
            [before after]  (effective-context opts)
            window          (+ 1 before after)
            head-pad        (repeat before nil)
            tail-pad        (repeat after  nil)
            sliding         (partition window 1
                                       (concat head-pad (line-seq reader) tail-pad))
            line-maps       (find-line-maps-with-context sliding before pattern path)]
        (when (seq line-maps)
          (let [uniques    (dedupe-line-maps line-maps)
                max-line-# (reduce max (map :line-# uniques))]
            (doseq [line-map (sort-by :line-# uniques)]
              (p/grep-match output max-line-# line-map opts))))))))

(defn- stream-line-matches? [output opts stream-factory pattern]
  (with-reader output opts stream-factory
    (fn [reader] (some #(re-find pattern %) (line-seq reader)))))

(defn binary-stream?
  "Return true if the first 8KB of the stream contains a NUL byte. Mirrors
  the heuristic used by GNU grep / ripgrep / git for detecting binary data.
  Reads up to 8192 bytes."
  [^InputStream s]
  (let [buf (byte-array 8192)
        n   (.read s buf)]
    (loop [i 0]
      (cond
        (>= i n)                  false
        (zero? (aget buf i))      true
        :else                     (recur (inc i))))))

(defn- looks-binary?
  "Best-effort binary detection for a stream-factory. Returns true if the
  file's first 8KB contain a NUL byte. Errors during the peek count as
  not-binary so we don't lose searchable files to flaky reads."
  [output opts stream-factory]
  (boolean
    (with-stream output opts stream-factory binary-stream?)))

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

(defn- handle-find-by-hash
  "If --find-by-hash specs are set, hash the file and emit a :match for
  every spec that matches. Returns true if find-by-hash was attempted (i.e.
  caller should not fall through to other actions)."
  [output opts file-path stream-factory specs]
  (let [algos (set (map :algo specs))
        ;; Compute every requested algo once, even if multiple specs use it.
        digests (reduce
                  (fn [acc algo]
                    (if-let [{:keys [digest]} (get hash-algorithms algo)]
                      (assoc acc algo (with-stream output opts stream-factory digest))
                      acc))
                  {} algos)]
    (doseq [{:keys [algo hex]} specs]
      (when (= hex (get digests algo))
        (p/match output file-path opts)))
    true))

(defn- handle-match
  "Common dispatcher: given a match candidate (file or jar entry), apply name/
  path/apath filters and then run the requested operation. Locals are renamed
  away from clojure.core fns (name, hash) so the body stays readable."
  [output opts file-name file-path stream-factory render-cat]
  (let [name-pat    (:name opts)
        grep-pat    (:grep opts)
        path-pat    (:path opts)
        apath-pat   (:apath opts)
        cat?        (:cat opts)
        files-only? (:files-only opts)
        hash-types  (:hash opts)
        find-hash   (:find-by-hash opts)
        text?       (:text opts)
        macro-op    (or cat? hash-types find-hash)]
    (cond
      (and name-pat  (not (re-find name-pat  file-name))) nil
      (and path-pat  (not (re-find path-pat  file-path))) nil
      (and apath-pat (not (re-find apath-pat file-path))) nil

      ;; --find-by-hash short-circuits everything else: hash and emit on match.
      find-hash (handle-find-by-hash output opts file-path stream-factory find-hash)

      (not (or macro-op grep-pat))
      (p/match output file-path opts)

      ;; Skip binary files when grepping (unless --text is set, or we're
      ;; about to do something other than grep that needs the bytes).
      (and grep-pat (not text?) (not macro-op)
           (looks-binary? output opts stream-factory)) nil

      (and grep-pat macro-op
           (not (stream-line-matches? output opts stream-factory grep-pat))) nil

      hash-types (calculate-hashes output file-path stream-factory hash-types opts)
      cat?       (when-let [s (render-cat output file-path stream-factory opts)]
                   (p/dump-stream output file-path s opts))
      ;; -l / --files-only: collapse grep to a single path emission per
      ;; matching file. Cheap-exits on first match via line-seq + some.
      (and grep-pat files-only?)
      (when (stream-line-matches? output opts stream-factory grep-pat)
        (p/match output file-path opts))
      grep-pat   (grep-stream output file-path stream-factory opts))))

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
  "Pick a file-finder entry for f given the active set of types. Assumes f
  has already passed valid-file-fn — by construction either the ext is in
  types and registered, or :default is in types and we fall through."
  [^File f types]
  (or (when-let [ext (file-ext f)]
        (when (contains? types ext)
          (file-finders ext)))
      (file-finders :default)))

;;;; ---------------------------------------------------------------------------
;;;; Scanners

(defn- scan-disk-file [^File f path opts output render-cat]
  (let [stream-factory #(jio/input-stream f)]
    (handle-match output opts (.getName f) path stream-factory render-cat)))

(defn- nested-archive?
  "Should we recurse into this entry as if it were another jar/zip? Tied
  to (:nested opts) and the active --types set so users can opt out."
  [opts ^String entry-name]
  (and (:nested opts)
       (let [ext (when-let [i (.lastIndexOf entry-name (int \.))]
                   (when (pos? i) (str/lower-case (subs entry-name (inc i)))))]
         (and ext (contains? (:types opts) ext)))))

(declare scan-zip-stream)

(defn- scan-zip-stream
  "Iterate entries of an open ZipInputStream, treating each as a candidate.
  prefix is the path-string accumulated so far (e.g. 'outer.jar@inner.jar@')
  used to build full entry paths. Used for nested-archive recursion."
  [^ZipInputStream zis ^String prefix opts output render-cat]
  (loop []
    (when-let [^ZipEntry entry (.getNextEntry zis)]
      (let [entry-path (.getName entry)
            entry-name (name-part entry-path)
            full-path  (str prefix entry-path)
            ;; Cache the entry's bytes: ZipInputStream is sequential, but
            ;; multi-pass actions (--cat counts then formats; --find-by-hash
            ;; may digest the same stream) need a fresh stream each call.
            bytes      (let [baos (java.io.ByteArrayOutputStream.)]
                         (jio/copy zis baos)
                         (.toByteArray baos))
            stream-factory #(java.io.ByteArrayInputStream. bytes)]
        (cond
          (and (nested-archive? opts entry-name)
               (not (.isDirectory entry)))
          (with-open [inner (ZipInputStream.
                              (java.io.ByteArrayInputStream. bytes))]
            (scan-zip-stream inner (str full-path \@) opts output render-cat))

          :else
          (handle-match output opts entry-name full-path stream-factory render-cat)))
      (.closeEntry zis)
      (recur))))

(defn- scan-jar
  "Scan a jar/zip on disk. Empty jars produce a warn (silently skipping
  them was previously confusing). Nested archives are searched when
  --nested is set; directory entries are still emitted for path-list
  parity with master."
  [^File jar ^String jar-path opts output render-cat]
  (cond
    (zero? (.length jar))
    (p/warn output
            (str "skipping zero-byte archive " (.getPath jar))
            nil opts)

    :else
    (let [prefix (str (str/trim jar-path) \@)]
      (try
        ;; Top-level archive: use ZipFile (random access, faster for large
        ;; jars). Nested archives go through ZipInputStream via
        ;; scan-zip-stream above.
        (with-open [^ZipFile zip (ZipFile. ^File jar)]
          (doseq [^ZipEntry entry (enumeration-seq (.entries zip))
                  :let [entry-path     (.getName entry)
                        entry-name     (name-part entry-path)
                        full-path      (str prefix entry-path)
                        stream-factory #(.getInputStream zip entry)]]
            (cond
              (and (nested-archive? opts entry-name)
                   (not (.isDirectory entry)))
              (with-open [zis (ZipInputStream. (.getInputStream zip entry))]
                (scan-zip-stream zis (str full-path \@) opts output render-cat))

              :else
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

(def default-excluded-dirs
  "Directory names skipped during traversal unless --all is set. Common
  build/VCS dirs that are almost never the target of a search and that
  dominate scan time on real projects."
  #{".git" ".svn" ".hg" ".bzr"
    "node_modules"
    "target" "build"
    ".gradle" ".cpcache"
    ".idea" ".vscode"})

(defn- relative-path
  "Convert an absolute File path to one relative to search-root. Tolerates
  trailing separators on search-root."
  [^File search-root]
  (let [root (.getPath search-root)
        len  (cond-> (count root)
               (not (str/ends-with? root File/separator)) inc)]
    (fn [^File f] (subs (.getPath f) len))))

(defn path-fn
  "Return a fn File -> String producing the path representation chosen by
  opts:
    :apath true        — canonical absolute path
    :include-root? true — File.getPath as-is (search-root prefix preserved)
    otherwise          — path relative to search-root
  include-root? is set by main when there's more than one search root, so
  paths are unambiguous between roots."
  [^File search-root opts]
  (cond
    (:apath opts)         (fn [^File f] (.getCanonicalPath f))
    (:include-root? opts) (fn [^File f] (.getPath f))
    :else                 (relative-path search-root)))

;;;; ---------------------------------------------------------------------------
;;;; .gitignore — minimal best-effort matcher.
;;;; Supports comments (#), blank lines, * and ? wildcards, and trailing-slash
;;;; directory hints. Patterns containing '/' are anchored to the search-root,
;;;; otherwise they match at any depth. Negation (!pattern) is intentionally
;;;; ignored; if you need full git fidelity, run findjar inside a directory
;;;; you've already pruned, or pass --no-gitignore.

(defn- gitignore-line->regex [^String pat]
  (when-not (or (str/blank? pat) (.startsWith pat "#") (.startsWith pat "!"))
    (let [pat        (cond-> pat (.endsWith pat "/")  (subs 0 (dec (count pat))))
          anchored?  (or (.startsWith pat "/") (.contains pat "/"))
          pat        (cond-> pat (.startsWith pat "/") (subs 1))
          sb         (StringBuilder. (if anchored? "^" "(?:^|.*/)"))]
      (loop [i 0]
        (when (< i (count pat))
          (let [c (.charAt pat i)]
            (case c
              \* (.append sb "[^/]*")
              \? (.append sb "[^/]")
              \. (.append sb "\\.")
              \\ (.append sb "\\\\")
              \( (.append sb "\\(")
              \) (.append sb "\\)")
              \+ (.append sb "\\+")
              \^ (.append sb "\\^")
              \$ (.append sb "\\$")
              \{ (.append sb "\\{")
              \} (.append sb "\\}")
              \| (.append sb "\\|")
              (.append sb c))
            (recur (inc i)))))
      (.append sb "(?:/.*)?$")
      (Pattern/compile (.toString sb)))))

(defn- gitignore-matcher
  "Return (fn [rel-path] -> bool) telling whether rel-path is ignored by
  the .gitignore at root, or nil if no .gitignore (or --no-gitignore)."
  [^File root opts]
  (when-not (:no-gitignore opts)
    (let [gi (jio/file root ".gitignore")]
      (when (.isFile gi)
        (let [patterns (->> (str/split-lines (slurp gi))
                            (map str/trim)
                            (keep gitignore-line->regex))]
          (when (seq patterns)
            (fn [rel-path]
              (boolean (some #(re-find % rel-path) patterns)))))))))

;;;; ---------------------------------------------------------------------------
;;;; Walker

(defn- symlink? [^File f]
  (Files/isSymbolicLink (.toPath f)))

(defn- walk-tree
  "Custom recursive walker producing a lazy seq of files under root, applying:
   - --max-depth (1 = direct children, omit/nil = unlimited)
   - --exclude + default-excluded-dirs (unless :all)
   - --follow (default: skip symlinks)
   - .gitignore matching (unless :no-gitignore)
  Only files are emitted; directories are pruned-or-descended. The root
  itself is always traversed even if its name would otherwise be excluded."
  [^File root opts]
  (let [follow?    (:follow opts)
        max-depth  (:max-depth opts)
        excluded   (cond-> (or (:exclude opts) #{})
                     (not (:all opts)) (set/union default-excluded-dirs))
        gi-match?  (or (gitignore-matcher root opts) (constantly false))
        to-rel     (relative-path root)
        within?    (fn [d] (or (nil? max-depth) (<= d max-depth)))
        descend?   (fn [d] (or (nil? max-depth) (< d max-depth)))
        skip?      (fn [^File f]
                     (or (and (not follow?) (symlink? f))
                         (gi-match? (to-rel f))))
        prune-dir? (fn [^File d]
                     (or (contains? excluded (.getName d))
                         (skip? d)))
        step (fn step [^File f depth]
               (lazy-seq
                 (cond
                   (.isFile f)
                   (when (and (within? depth) (not (skip? f))) [f])

                   (.isDirectory f)
                   (when (and (descend? depth) (not (prune-dir? f)))
                     (mapcat #(step % (inc depth)) (.listFiles f)))

                   :else nil)))]
    (if (.isDirectory root)
      (mapcat #(step % 1) (.listFiles root))
      [root])))

(defn candidate-files
  "The lazy seq of files (under search-root) whose extension is permitted by
  the active --types set, after walk-tree applies symlink/depth/exclude/
  gitignore pruning. Pre-munge opts before calling."
  [^File search-root opts]
  (filter (valid-file-fn opts) (walk-tree search-root opts)))

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
