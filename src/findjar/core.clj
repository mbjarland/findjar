(ns findjar.core
  (:require [clojure.java.io :as jio]
            [clojure.set :as set]
            [clojure.string :as str]
            [findjar.hash :as hash]
            [findjar.protocols :as p])
  (:import [java.io BufferedInputStream ByteArrayInputStream File InputStream]
           [java.nio.file Files LinkOption]
           [java.util.regex Pattern]
           [java.util.zip GZIPInputStream ZipEntry ZipFile ZipInputStream]))

;;;; ---------------------------------------------------------------------------
;;;; Stream helpers — replace the old FileContent protocol.
;;;; A "stream-factory" is a 0-arg fn that returns a fresh InputStream each
;;;; time it's called. Helpers manage with-open and warn-on-error so callers
;;;; don't repeat the boilerplate.

(defn with-stream
  "Open the stream produced by stream-factory, hand it to f, close it.
  On exception, call (warn output msg ex opts) and return nil. The message
  always includes the exception's simple class name — many JDK errors carry
  null or terse messages that are hard to act on otherwise."
  [output opts stream-factory f]
  (try
    (with-open [^InputStream s (stream-factory)]
      (f s))
    (catch Exception e
      (let [cls (.getSimpleName (class e))
            msg (.getMessage e)
            full (if msg (str cls ": " msg) cls)]
        (p/warn output full e opts))
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
  pattern opts in opts. -i / --ignore-case is the same as adding 'i' to
  the flag string, and merges in so '-i -f m' acts like '-f im'."
  [opts]
  (let [flags (cond-> (or (:flags opts) "")
                (and (:ignore-case opts)
                     (not (str/includes? (or (:flags opts) "") "i")))
                (str "i"))]
    (if (empty? flags)
      opts
      (reduce (fn [acc k]
                (if-let [^Pattern v (get acc k)]
                  (assoc acc k (compile-with-flags v flags))
                  acc))
              opts
              [:name :grep :path :apath]))))

;;;; ---------------------------------------------------------------------------
;;;; Grep — sliding-window context

(defn- window->matching-lines
  "Build the line-maps for a single hit at match-line-#. before specifies
  how many context lines precede the hit within `lines`. match-idxs may be
  empty (e.g. for --invert-match where there are no chunks to highlight)."
  [path match-line-# before lines match-idxs]
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

(defn- line-match-fn
  "Decide whether a center line is a hit and (for highlighting) which
  ranges within it to colour. Returns nil for non-hits, a (possibly empty)
  vector of {:start :end} for hits. Empty means hit-but-no-highlight, used
  by --invert-match where there are no matched chunks to colour."
  [pattern invert?]
  (if invert?
    (fn [^String s] (when (and s (not (re-find pattern s))) []))
    (fn [^String s] (when s (let [m (match-idxs pattern s)] (when (seq m) m))))))

(defn- find-line-maps-with-context
  "Walk the sliding-window view of the file. Emits hit + context line-maps
  for the first --max-count hits (or all if not set)."
  [sliding before pattern path opts]
  (let [match? (line-match-fn pattern (:invert opts))
        max-n  (:max-count opts)]
    (loop [items (map-indexed vector sliding)
           hits  0
           acc   []]
      (cond
        (empty? items)                acc
        (and max-n (= hits max-n))    acc
        :else
        (let [[w-num lines] (first items)
              idxs (match? (nth lines before))]
          (if (nil? idxs)
            (recur (rest items) hits acc)
            (recur (rest items)
                   (inc hits)
                   (into acc (window->matching-lines path w-num before lines idxs)))))))))

(defn- effective-context
  "Resolve -A/-B/-x into [before after]. Explicit -A or -B win; if only -x
  is given, both before and after default to it. With --count, context is
  meaningless (we emit only a number) so we collapse to zero."
  [opts]
  (if (:count opts)
    [0 0]
    (let [x (or (:context opts) 0)
          a (:after opts)
          b (:before opts)]
      [(or b x) (or a x)])))

(defn grep-stream
  "Read file-content via stream-factory, slide a (1+before+after) window
  over its lines, emit per-line p/grep-match calls (or a single
  p/grep-count if --count is set)."
  [output path stream-factory opts]
  (with-reader output opts stream-factory
    (fn [reader]
      (let [pattern         (:grep opts)
            count?          (:count opts)
            [before after]  (effective-context opts)
            window          (+ 1 before after)
            head-pad        (repeat before nil)
            tail-pad        (repeat after  nil)
            sliding         (partition window 1
                                       (concat head-pad (line-seq reader) tail-pad))
            line-maps       (find-line-maps-with-context sliding before pattern path opts)]
        (cond
          count?
          (let [n (count (filter :hit? line-maps))]
            (when (pos? n)
              (p/grep-count output path n opts)))

          (seq line-maps)
          (let [uniques    (dedupe-line-maps line-maps)
                max-line-# (reduce max (map :line-# uniques))]
            (doseq [line-map (sort-by :line-# uniques)]
              (p/grep-match output max-line-# line-map opts))))))))

(defn- stream-line-matches?
  "Cheap-exit any-line check used by the -l / macro-op gates. Honors
  --invert-match: with -v, returns true if any line does NOT match."
  [output opts stream-factory pattern]
  (let [hit? (if (:invert opts)
               (fn [s] (not (re-find pattern s)))
               (fn [s] (re-find pattern s)))]
    (with-reader output opts stream-factory
      (fn [reader] (some hit? (line-seq reader))))))

;; The same access bits mean different things on classes vs methods —
;; bit 0x0020 is ACC_SUPER on a class (legacy / universal, not useful
;; to surface) but ACC_SYNCHRONIZED on a method. Two separate maps:

(def ^:private class-access-flags
  {0x0001 :public      0x0010 :final      0x0200 :interface
   0x0400 :abstract    0x2000 :annotation 0x4000 :enum})

(def ^:private method-access-flags
  {0x0001 :public      0x0002 :private    0x0004 :protected
   0x0008 :static      0x0010 :final      0x0020 :synchronized
   0x0100 :native      0x0400 :abstract   0x0800 :strict})

(defn- bits->kws [flags ^long acc]
  (into (sorted-set)
        (keep (fn [[^long bit kw]] (when (pos? (bit-and acc bit)) kw)))
        flags))

(defn- class-access->kws  [^long acc] (bits->kws class-access-flags  acc))
(defn- method-access->kws [^long acc] (bits->kws method-access-flags acc))

(defn class-info-from-bytes
  "Parse .class bytes via ASM and return a map describing the class.
  Returns nil if the bytes don't look like a valid class file. Uses the
  org.objectweb.asm dep rather than clojure.asm because the bundled
  Clojure ASM lags behind newer Java class-file versions."
  [^bytes bs]
  (try
    (let [reader  (org.objectweb.asm.ClassReader. bs)
          class-name (atom nil)
          super      (atom nil)
          ifaces     (atom [])
          c-access   (atom 0)
          methods    (atom [])
          ;; ASM9 covers Java 21+ classfile features. We SKIP_CODE so
          ;; method bodies are never parsed, which keeps this fast even
          ;; on huge classes.
          visitor
          (proxy [org.objectweb.asm.ClassVisitor] [org.objectweb.asm.Opcodes/ASM9]
            (visit [_version access nm _sig superName interfaces]
              (reset! class-name nm)
              (reset! super     superName)
              (reset! ifaces    (vec interfaces))
              (reset! c-access  access))
            (visitMethod [access nm desc _sig _exceptions]
              (swap! methods conj
                     {:name nm :desc desc :access (method-access->kws access)})
              nil))]
      (.accept reader visitor (int (bit-or org.objectweb.asm.ClassReader/SKIP_CODE
                                            org.objectweb.asm.ClassReader/SKIP_DEBUG
                                            org.objectweb.asm.ClassReader/SKIP_FRAMES)))
      {:name       @class-name
       :super      @super
       :interfaces @ifaces
       :access     (class-access->kws @c-access)
       :methods    @methods})
    (catch Throwable _ nil)))

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

(defn- handle-class-info
  "Read the entry's bytes, parse via clojure.asm, and emit class-info."
  [output opts file-path stream-factory]
  (when (.endsWith ^String file-path ".class")
    (when-let [bytes (with-stream output opts stream-factory
                       (fn [^InputStream s]
                         (let [baos (java.io.ByteArrayOutputStream.)]
                           (jio/copy s baos)
                           (.toByteArray baos))))]
      (when-let [info (class-info-from-bytes bytes)]
        (p/class-info output file-path info opts)))))

(defn- manifest-entry?
  "True if file-path looks like a MANIFEST.MF or a Maven pom.properties."
  [^String file-path]
  (or (.endsWith file-path "/MANIFEST.MF")
      (.endsWith file-path "@MANIFEST.MF")
      (= file-path "MANIFEST.MF")
      (.endsWith file-path "pom.properties")))

(def manifest-summary-keys
  "Curated allow-list of MANIFEST.MF attributes most users actually care
  about. --manifest-summary filters the full manifest down to this set
  so the output of `findjar app.jar --manifest-summary --nested` reads
  like a one-line-per-dependency summary instead of pages of build
  metadata."
  #{"Main-Class"
    "Premain-Class" "Agent-Class"
    "Implementation-Title" "Implementation-Version" "Implementation-Vendor"
    "Specification-Title" "Specification-Version" "Specification-Vendor"
    "Bundle-Name" "Bundle-SymbolicName" "Bundle-Version" "Bundle-Description"
    "Created-By" "Built-By" "Build-Jdk" "Build-Date" "Build-Time"
    "Class-Path"
    "Automatic-Module-Name"})

(defn- handle-explode
  "Extract a matched entry to disk under (:explode opts). The output
  filesystem path mirrors the archive layout: '@' separators in the
  source path become directory boundaries. Existing files are
  overwritten silently. A successful extraction emits a :match event
  for the on-disk file so the user sees the destination path (and
  the call participates in exit-code semantics)."
  [output opts ^String file-path stream-factory]
  (let [^File base (:explode opts)
        rel        (-> file-path
                       (str/replace "@" "/")
                       (str/replace #"^/+" ""))
        out-file   (jio/file base rel)]
    (try
      (jio/make-parents out-file)
      (with-open [is (stream-factory)
                  os (jio/output-stream out-file)]
        (jio/copy is os))
      (p/match output (.getPath out-file) opts)
      (catch Exception e
        (p/warn output
                (str "exploding " file-path " → " (.getPath out-file)
                     ": " (.getMessage e))
                e opts)))))

(defn- handle-manifest-summary
  "Like --manifest --cat but filtered: parses MANIFEST.MF via
  java.util.jar.Manifest and keeps only manifest-summary-keys; passes
  pom.properties through verbatim (they're already a 3-line summary)."
  [output opts file-path stream-factory]
  (cond
    (or (.endsWith ^String file-path "/MANIFEST.MF")
        (.endsWith ^String file-path "@MANIFEST.MF")
        (= ^String file-path "MANIFEST.MF"))
    (try
      (with-open [is (stream-factory)]
        (let [mf    (java.util.jar.Manifest. is)
              attrs (.getMainAttributes mf)
              rows  (->> attrs
                         (keep (fn [[k v]]
                                 (let [ks (str k)]
                                   (when (contains? manifest-summary-keys ks)
                                     [ks (str v)]))))
                         (sort-by first))]
          (when (seq rows)
            (let [block (str/join \newline
                          (concat [(str "<<<<<<< " file-path)]
                                  (for [[k v] rows]
                                    (str "  " k ": " v))
                                  [">>>>>>>" ""]))]
              (p/dump-stream output file-path block opts)))))
      (catch Exception e
        (p/warn output
                (str "manifest-summary parse failed on " file-path
                     " - " (.getMessage e))
                e opts)))

    (.endsWith ^String file-path "pom.properties")
    ;; pom.properties is already a 3-line summary; pass through with the
    ;; existing cat renderer if --manifest-summary is also catting them.
    nil))

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
        class-info? (:class-info opts)
        manifest?   (:manifest opts)
        manifest-s? (:manifest-summary opts)
        explode?    (:explode opts)
        files-only? (:files-only opts)
        hash-types  (:hash opts)
        find-hash   (:find-by-hash opts)
        text?       (:text opts)
        macro-op    (or cat? hash-types find-hash class-info? manifest? manifest-s? explode?)
        ;; ZIP / JAR directory entries are named with a trailing '/'.
        ;; They have no content — hashing them produces the sha1 of
        ;; the empty stream (da39a3ee... ) for every directory, which
        ;; is noise; cat / grep / class-info / manifest / find-by-hash
        ;; are similarly meaningless. Keep them visible only in the
        ;; default path-listing mode (where the directory entry's path
        ;; is itself useful information).
        dir?        (.endsWith ^String file-path "/")]
    (cond
      (and name-pat  (not (re-find name-pat  file-name))) nil
      (and path-pat  (not (re-find path-pat  file-path))) nil
      (and apath-pat (not (re-find apath-pat file-path))) nil

      ;; Directory entries: only meaningful in the default path-list mode.
      ;; Skip every action; emit the path when no action is in effect.
      dir?
      (when-not (or macro-op grep-pat)
        (p/match output file-path opts))

      ;; --class-info: parse .class entries, ignore other entries silently.
      class-info?
      (handle-class-info output opts file-path stream-factory)

      ;; --manifest: cat MANIFEST.MF / pom.properties entries; ignore
      ;; everything else. Only meaningful for jar/zip entries (paths
      ;; with @-separators), but also works on a bare META-INF/...
      ;; file on disk for the rare power-user invocation.
      manifest?
      (when (manifest-entry? file-path)
        (when-let [s (render-cat output file-path stream-factory opts)]
          (p/dump-stream output file-path s opts)))

      ;; --manifest-summary: like --manifest but filters MANIFEST.MF
      ;; through java.util.jar.Manifest and keeps only the curated
      ;; manifest-summary-keys set. Useful for auditing dependency
      ;; metadata across an uberjar without drowning in build logs.
      manifest-s?
      (when (manifest-entry? file-path)
        (handle-manifest-summary output opts file-path stream-factory))

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
      explode?   (handle-explode output opts file-path stream-factory)
      cat?       (when-let [s (render-cat output file-path stream-factory opts)]
                   (p/dump-stream output file-path s opts))
      ;; -l / --files-only: collapse grep to a single path emission per
      ;; matching file. Cheap-exits on first match via line-seq + some.
      (and grep-pat files-only?)
      (when (stream-line-matches? output opts stream-factory grep-pat)
        (p/match output file-path opts))
      grep-pat   (grep-stream output file-path stream-factory opts))))

;;;; ---------------------------------------------------------------------------
;;;; Tar reader — minimal (USTAR / GNU "L" long-name) header parser. We
;;;; don't pull in commons-compress; for findjar's read-only use case the
;;;; ~80 lines below cover the formats people actually encounter.

(defn- tar-string
  "Read a NUL-terminated ASCII string from a fixed-width header field."
  [^bytes hdr ^long off ^long len]
  (let [end (loop [i off]
              (cond
                (= i (+ off len))         (+ off len)
                (zero? (aget hdr i))      i
                :else                     (recur (inc i))))]
    (String. hdr (int off) (int (- end off)) "UTF-8")))

(defn- tar-octal ^long [^bytes hdr ^long off ^long len]
  (let [s (tar-string hdr off len)
        s (str/trim s)]
    (if (str/blank? s) 0 (Long/parseLong s 8))))

(defn- read-fully
  "Read exactly n bytes from in. Returns the byte[] or nil at EOF."
  [^InputStream in n]
  (let [buf (byte-array n)]
    (loop [off 0]
      (cond
        (= off n) buf
        :else
        (let [r (.read in buf off (- n off))]
          (cond
            (neg? r) (when (pos? off) buf)   ; partial: rare for tar headers
            :else    (recur (+ off r))))))))

(defn- skip-fully [^InputStream in ^long n]
  (loop [left n]
    (when (pos? left)
      (let [s (.skip in left)]
        (cond
          (zero? s) (when (neg? (.read in)) :eof)   ; EOF mid-skip
          :else     (recur (- left s)))))))

(defn- scan-tar-stream
  "Walk a tar archive's entries via InputStream. Each non-empty regular
   file produces a (handle-match ...) call with the content available via
   a stream-factory backed by an in-memory byte[]."
  [^InputStream in ^String prefix opts output render-cat]
  (let [pending-long-name (atom nil)]
    (loop [empty-blocks 0]
      (let [hdr (read-fully in 512)]
        (cond
          (nil? hdr) nil
          ;; Tar end-of-archive marker is two consecutive zero blocks.
          (every? zero? hdr)
          (when (zero? empty-blocks) (recur 1))

          :else
          (let [name      (or @pending-long-name (tar-string hdr 0 100))
                size      (tar-octal hdr 124 12)
                typeflag  (char (aget hdr 156))
                blocks    (long (Math/ceil (/ (double size) 512.0)))
                pad-bytes (- (* blocks 512) size)]
            (reset! pending-long-name nil)
            (case typeflag
              ;; Regular file (or '\0' which is the legacy encoding).
              (\0 \space)
              (let [bytes (if (pos? size) (read-fully in size) (byte-array 0))
                    _     (when (pos? pad-bytes) (skip-fully in pad-bytes))
                    full-path (str prefix name)
                    entry-name (name-part name)
                    sf    #(ByteArrayInputStream. bytes)]
                (handle-match output opts entry-name full-path sf render-cat)
                (recur 0))

              ;; GNU long-name extension: payload is the next entry's
              ;; full name. Cache and keep going.
              \L
              (let [payload (read-fully in size)
                    _       (when (pos? pad-bytes) (skip-fully in pad-bytes))
                    n       (str/replace (String. ^bytes payload "UTF-8")
                                          #" +$" "")]
                (reset! pending-long-name n)
                (recur 0))

              ;; Anything else (directory, symlink, etc.) — skip the
              ;; payload and move on.
              (do
                (when (pos? size) (skip-fully in (+ size pad-bytes)))
                (recur 0)))))))))

(defn- tar-input-stream ^InputStream [^File f gzipped?]
  (let [^InputStream raw (BufferedInputStream. (jio/input-stream f))]
    (if gzipped? (GZIPInputStream. raw) raw)))

(defn- scan-tar
  "Open a .tar / .tar.gz / .tgz file and walk its entries."
  [^File tar ^String tar-path opts output render-cat]
  (let [n        (str/lower-case (.getName tar))
        gzipped? (or (.endsWith n ".gz") (.endsWith n ".tgz"))
        prefix   (str (str/trim tar-path) \@)]
    (try
      (with-open [^InputStream in (tar-input-stream tar gzipped?)]
        (scan-tar-stream in prefix opts output render-cat))
      (catch Exception e
        (p/warn output
                (str (.getSimpleName (class e)) " opening " (.getPath tar)
                     " - " (.getMessage e))
                e
                opts)))))

;;;; ---------------------------------------------------------------------------
;;;; File-type registry — replaces defmulti file-finder

(declare scan-jar scan-disk-file)

(defn- scan-gz
  "Treat a single .gz file (e.g. a rotated log file) as a one-entry virtual
  archive. Unwraps with GZIPInputStream and feeds the resulting stream
  through handle-match. The synthetic entry name is the basename minus
  the trailing .gz so the @-separated path reads like other archives."
  [^File f path opts output render-cat]
  (let [name       (.getName f)
        lower      (str/lower-case name)
        entry-name (cond
                     (.endsWith lower ".gz") (subs name 0 (- (count name) 3))
                     :else                   name)
        full-path  (str (str/trim path) \@ entry-name)
        stream-factory
        #(java.util.zip.GZIPInputStream. (jio/input-stream f))]
    (try
      (handle-match output opts entry-name full-path stream-factory render-cat)
      (catch Exception e
        (p/warn output
                (str (.getSimpleName (class e)) " unwrapping " (.getPath f)
                     " - " (.getMessage e))
                e
                opts)))))

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
             :char    \z}
   "tar"    {:scan    (fn [f path opts output render-cat]
                        (scan-tar f path opts output render-cat))
             :desc    "files in tar / tar.gz / tgz archives"
             :default false
             :char    \t}
   "gz"     {:scan    (fn [f path opts output render-cat]
                        (scan-gz f path opts output render-cat))
             :desc    "contents of .gz files (e.g. rotated log files)"
             :default false
             :char    \g}})

(defn- tar-extension?
  "True if name has a multi-part tar extension (.tar.gz, .tar.bz2, etc.).
   The single-extension '.tar' is already covered by file-ext."
  [^String name]
  (let [n (str/lower-case name)]
    (or (.endsWith n ".tar.gz") (.endsWith n ".tgz"))))

(defn- effective-ext
  "Like file-ext but recognizes multi-part tar extensions, mapping them
   all to the canonical 'tar' file-finder key."
  [^File f]
  (let [n (.getName f)]
    (cond
      (tar-extension? n) "tar"
      :else              (file-ext f))))

(defn- finder-for
  "Pick a file-finder entry for f given the active set of types. Assumes f
  has already passed valid-file-fn — by construction either the ext is in
  types and registered, or :default is in types and we fall through."
  [^File f types]
  (or (when-let [ext (effective-ext f)]
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
                 (contains? active-exts (effective-ext f))))))))

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
  trailing separators on search-root. When f IS the search-root (e.g.
  'findjar app.jar --manifest' with a file-as-root), returns the
  basename."
  [^File search-root]
  (let [root (.getPath search-root)
        len  (cond-> (count root)
               (not (str/ends-with? root File/separator)) inc)]
    (fn [^File f]
      (let [p (.getPath f)]
        (cond
          (= p root)        (.getName f)
          (< (count p) len) (.getName f)
          :else             (subs p len))))))

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
;;;; .gitignore — best-effort matcher.
;;;;
;;;; Supports:
;;;;   - comments (#) and blank lines
;;;;   - * and ? wildcards (not globstar **)
;;;;   - trailing-slash directory hint (stripped; we ignore dir-only-ness)
;;;;   - negation (!pattern) with last-match-wins semantics
;;;;   - anchoring: patterns containing '/' anchor to the matcher's root
;;;;     directory; bare patterns match at any depth
;;;;
;;;; Patterns are kept in input order so a later !rule can re-include a
;;;; previously-excluded path, just like git.

(defn- compile-gitignore-pattern
  "Translate one non-comment, non-blank line into a regex matching paths
  relative to the .gitignore's directory."
  [^String pat]
  (let [pat       (cond-> pat (.endsWith pat "/")  (subs 0 (dec (count pat))))
        anchored? (or (.startsWith pat "/") (.contains pat "/"))
        pat       (cond-> pat (.startsWith pat "/") (subs 1))
        sb        (StringBuilder. (if anchored? "^" "(?:^|.*/)"))]
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
    (Pattern/compile (.toString sb))))

(defn- parse-gitignore-line
  "Parse one line of a .gitignore. Returns {:re Pattern :negate? bool} or
  nil for comments / blanks."
  [^String line]
  (let [line (str/trim line)]
    (cond
      (or (str/blank? line) (.startsWith line "#")) nil
      (.startsWith line "!") {:re (compile-gitignore-pattern (subs line 1))
                              :negate? true}
      :else                  {:re (compile-gitignore-pattern line)
                              :negate? false})))

(defn- gitignore-matcher
  "Return (fn [rel-path] -> bool) telling whether rel-path is ignored by
  the .gitignore at root, or nil if no .gitignore (or --no-gitignore).
  Implements last-match-wins so a later !rule re-includes the path."
  [^File root opts]
  (when-not (:no-gitignore opts)
    (let [gi (jio/file root ".gitignore")]
      (when (.isFile gi)
        (let [rules (->> (str/split-lines (slurp gi))
                         (keep parse-gitignore-line)
                         vec)]
          (when (seq rules)
            (fn [rel-path]
              (loop [i      0
                     ignore false]
                (if (>= i (count rules))
                  ignore
                  (let [{:keys [re negate?]} (rules i)
                        m (boolean (re-find re rel-path))]
                    (recur (inc i)
                           (cond
                             (and m negate?)       false
                             (and m (not negate?)) true
                             :else                 ignore))))))))))))

;;;; ---------------------------------------------------------------------------
;;;; Walker

(defn- symlink? [^File f]
  (Files/isSymbolicLink (.toPath f)))

(defn- subpath-from
  "Return the path of file relative to ancestor-dir, with forward slashes,
  or nil if file isn't under ancestor-dir."
  [^File ancestor-dir ^File file]
  (let [a (.getPath ancestor-dir)
        f (.getPath file)]
    (when (and (.startsWith f a) (> (count f) (count a)))
      (let [tail (subs f (count a))]
        (cond-> tail (.startsWith tail File/separator) (subs 1))))))

(defn- any-rule-ignores?
  "True if any (matcher, dir) pair in the stack would ignore the file
  under that dir. Each matcher tests a path relative to ITS dir."
  [stack ^File file]
  (boolean
    (some (fn [{:keys [matcher dir]}]
            (when-let [rel (subpath-from dir file)]
              (matcher rel)))
          stack)))

(defn- walk-tree
  "Custom recursive walker producing a lazy seq of files under root, applying:
   - --max-depth (1 = direct children, omit/nil = unlimited)
   - --exclude + default-excluded-dirs (unless :all)
   - --follow (default: skip symlinks)
   - .gitignore matching (unless :no-gitignore), recursively: a .gitignore
     in any descendant directory contributes patterns for that subtree
  Only files are emitted; directories are pruned-or-descended. The root
  itself is always traversed even if its name would otherwise be excluded."
  [^File root opts]
  (let [follow?    (:follow opts)
        max-depth  (:max-depth opts)
        excluded   (cond-> (or (:exclude opts) #{})
                     (not (:all opts)) (set/union default-excluded-dirs))
        within?    (fn [d] (or (nil? max-depth) (<= d max-depth)))
        descend?   (fn [d] (or (nil? max-depth) (< d max-depth)))
        ;; Each entry in the matcher stack is {:matcher fn :dir File}.
        ;; The matcher tests a path relative to its dir. Pushed on
        ;; descent, popped when the lazy seq leaves a subtree.
        push-here  (fn [stack ^File d]
                     (if-let [m (gitignore-matcher d opts)]
                       (conj stack {:matcher m :dir d})
                       stack))
        skip-file? (fn [stack ^File f]
                     (or (and (not follow?) (symlink? f))
                         (any-rule-ignores? stack f)))
        prune-dir? (fn [stack ^File d]
                     (or (contains? excluded (.getName d))
                         (and (not follow?) (symlink? d))
                         (any-rule-ignores? stack d)))
        step (fn step [stack ^File f depth]
               (lazy-seq
                 (cond
                   (.isFile f)
                   (when (and (within? depth) (not (skip-file? stack f))) [f])

                   (.isDirectory f)
                   (when (and (descend? depth) (not (prune-dir? stack f)))
                     (let [stack' (push-here stack f)]
                       (mapcat #(step stack' % (inc depth)) (.listFiles f))))

                   :else nil)))
        root-stack (push-here [] root)]
    (if (.isDirectory root)
      (mapcat #(step root-stack % 1) (.listFiles root))
      [root])))

(defn compile-glob ^java.util.regex.Pattern [^String pat]
  "Shell-style glob → anchored regex matching a forward-slash path.
    *   matches any chars except '/'
    **  matches any chars including '/'
    ?   matches a single char except '/'
  Everything else is escaped literally. No brace expansion or extglob."
  (let [sb (StringBuilder. "^")
        n  (count pat)]
    (loop [i 0]
      (cond
        (>= i n) nil
        (and (= \* (.charAt pat i))
             (< (inc i) n)
             (= \* (.charAt pat (inc i))))
        (do (.append sb ".*") (recur (+ i 2)))
        :else
        (let [c (.charAt pat i)]
          (case c
            \*  (.append sb "[^/]*")
            \?  (.append sb "[^/]")
            \.  (.append sb "\\.")
            \\  (.append sb "\\\\")
            \(  (.append sb "\\(")
            \)  (.append sb "\\)")
            \+  (.append sb "\\+")
            \^  (.append sb "\\^")
            \$  (.append sb "\\$")
            \{  (.append sb "\\{")
            \}  (.append sb "\\}")
            \|  (.append sb "\\|")
            (.append sb c))
          (recur (inc i)))))
    (.append sb "$")
    (java.util.regex.Pattern/compile (.toString sb))))

(defn- include-exclude-fn
  "Predicate over a forward-slash path that honours --include-glob and
  --exclude-glob. Each accepts multiple patterns; include is ANY-match,
  exclude is NONE-match; both default to 'pass through' when absent."
  [opts]
  (let [includes (->> (:include-globs opts) (mapv compile-glob))
        excludes (->> (:exclude-globs opts) (mapv compile-glob))]
    (fn [^String rel-path]
      (and (or (empty? includes) (some #(re-find % rel-path) includes))
           (or (empty? excludes) (not-any? #(re-find % rel-path) excludes))))))

(defn candidate-files
  "The lazy seq of files (under search-root) whose extension is permitted by
  the active --types set, after walk-tree applies symlink/depth/exclude/
  gitignore pruning. Pre-munge opts before calling. --include-glob and
  --exclude-glob (compared against the path relative to search-root) act
  as an additional filter layer."
  [^File search-root opts]
  (let [type-ok? (valid-file-fn opts)
        path-ok? (include-exclude-fn opts)
        to-rel   (relative-path search-root)]
    (->> (walk-tree search-root opts)
         (filter type-ok?)
         (filter (fn [^File f]
                   (path-ok? (str/replace (to-rel f) java.io.File/separator "/")))))))

(defn- ancestors-up-to
  "Return f's directory ancestors (excluding f itself), stopping at root
  (inclusive). If f is not under root, walks all ancestors."
  [^File root ^File f]
  (let [root-path (when root (.getCanonicalPath root))]
    (->> (iterate (fn [^File x] (.getParentFile x)) (.getParentFile f))
         (take-while some?)
         (reduce (fn [acc ^File a]
                   (let [conj-acc (conj acc a)]
                     (if (and root-path (= (.getCanonicalPath a) root-path))
                       (reduced conj-acc)
                       conj-acc)))
                 []))))

(defn why-skipped
  "Return a human-readable explanation of why findjar would NOT scan the
  file at file-path, given a search-root and the parsed opts. Returns nil
  if the file would actually be scanned. Best-effort: doesn't enumerate
  every possible cause, but pinpoints the common ones (default excludes,
  --types filter, non-existence, symlink without --follow, max-depth) and
  falls back to a 'try --all / --no-gitignore' hint."
  [^File search-root ^String file-path opts]
  (let [;; Default :types matches what the CLI does so that test callers
        ;; passing bare opts maps don't NPE inside the type predicate.
        opts  (cond-> (munge-regexes opts)
                (nil? (:types opts)) (assoc :types #{:default "jar"}))
        f     (jio/file file-path)
        absf  (when (.exists f) (.getCanonicalFile f))]
    (cond
      (not (.exists f))
      (str "path does not exist: " file-path)

      (.isDirectory f)
      (str "path is a directory; findjar scans files only")

      :else
      (let [excludes      (into (or (:exclude opts) #{})
                                (when-not (:all opts) default-excluded-dirs))
            ancestors     (ancestors-up-to search-root absf)
            in-cand?      (->> (candidate-files search-root opts)
                               (some #(= (.getCanonicalPath ^File %) (.getCanonicalPath absf)))
                               boolean)
            blocked-by    (some (fn [^File a]
                                  (when (contains? excludes (.getName a))
                                    (.getName a)))
                                ancestors)
            ext           (effective-ext f)
            type-allowed? (boolean
                            (or ((:types opts) :default)
                                (contains? (:types opts) ext)))
            symlink-anc   (when-not (:follow opts)
                            (some (fn [^File a]
                                    (when (symlink? a) (.getName a)))
                                  ancestors))
            depth-of      (count ancestors)
            max-depth     (:max-depth opts)]
        (cond
          in-cand?
          nil

          (and (not type-allowed?) (not ext))
          "extension-less file is not picked up by any --types entry (use 'n' to include normal disk files)"

          (not type-allowed?)
          (str "file extension '" ext "' is not in active --types " (sort (:types opts))
               "; pass -t to widen")

          blocked-by
          (str "ancestor directory '" blocked-by "' is excluded "
               (if (contains? default-excluded-dirs blocked-by)
                 "by the default exclude list (--all to bypass)"
                 "by --exclude"))

          symlink-anc
          (str "ancestor '" symlink-anc "' is a symlink and --follow is not set")

          (and max-depth (< max-depth depth-of))
          (str "depth " depth-of " exceeds --max-depth " max-depth)

          :else
          "excluded by .gitignore or a per-subdir .gitignore in an ancestor — try --all or --no-gitignore to confirm")))))

(defn scan-file
  "Scan a single File against output/render-cat with already-munged opts.
  display-path is the path string (relative or absolute) to surface to the
  user. Bumps the :examined-counter atom in opts if present (used by
  --stats)."
  [output render-cat opts ^File f display-path]
  (when-let [c (:examined-counter opts)]
    (swap! c inc))
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
