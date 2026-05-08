(ns findjar.cli
  (:require [clojure.edn :as edn]
            [clojure.java.io :as jio]
            [clojure.pprint :as cpp]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [findjar.core :as c]
            [jansi-clj.auto]
            [jansi-clj.core :as ansi])
  (:import [java.io PushbackReader]
           [java.text SimpleDateFormat]
           [java.util Date]
           [org.fusesource.jansi Ansi]))

(def max-width 78)

;;;; ---------------------------------------------------------------------------
;;;; Registry-driven CLI metadata. Reads from c/hash-algorithms and
;;;; c/file-finders so adding a new algorithm or file type updates --help.

(defn hash-selectors
  "Comma-separated list of registered hash algorithms (for help text)."
  []
  (str/join ", " (map :desc (vals c/hash-algorithms))))

(defn parse-hash-selector
  "Map a CLI hash name (\"sha1\") to the registry keyword (:sha1)."
  [s]
  (c/hash-by-desc s))

(defn file-types
  "Invert c/file-finders into a map keyed by single-char selector:
     {\\n {:desc \"normal files\" :default true  :ext :default}
      \\j {:desc \"files in jars\" :default true  :ext \"jar\"}
      ...}"
  []
  (reduce-kv
    (fn [a ext {:keys [desc default char]}]
      (assoc a char {:desc desc :default default :ext ext}))
    {}
    c/file-finders))

(defn file-type-selectors []
  (str/join "|" (keys (file-types))))

(defn file-type-descriptions []
  (str/join ", "
            (for [[k {:keys [desc]}] (file-types)]
              (str k " - " desc))))

(defn default-file-types []
  (filter (fn [[_ v]] (:default v)) (file-types)))

(defn default-file-type-exts []
  (set (map (comp :ext val) (default-file-types))))

(defn wrap-line [width line]
  (let [words (str/split line #" ")]
    (cpp/cl-format nil (str "~{~<~%~1," (dec width) ":;~A~> ~}") words)))

(defn un-whitespace [line]
  (if (.endsWith line "=")
    (subs line 0 (dec (count line)))
    (str/replace line #"\s+" " ")))

(defn wrap-desc [width margin desc]
  (let [line    (un-whitespace desc)
        wrapped (wrap-line width line)
        lines   (str/split wrapped #"\n")]
    (str/join (str \newline margin) lines)))

(defn wrap-opts
  "apply a function to the descriptions of the command line opts,
  returning a new set of opts with the altered descriptions"
  [width margin opts]
  (reduce
    (fn [c [short long desc & rest]]
      (let [modded (wrap-desc width margin desc)]
        (conj c (into [short long modded] rest))))
    []
    opts))

;TODO: move this formatting into summarize
(defn reformat-options
  "reformat the command line params for a clean output when printing usage"
  [max-width opts]
  (let [max-long-desc  (apply max (map (comp count second) opts))
        margin         (apply str (repeat (+ 2 3 1 max-long-desc 2) " "))
        max-desc-width (- max-width (+ max-long-desc 8))]
    (wrap-opts max-desc-width margin opts)))

(defn parse-types
  "Map a string of single-char type selectors (e.g. \"jz\") to the set of
  registered extensions (e.g. #{\"jar\" \"zip\"}). Returns nil if any char is
  not a registered selector; the :validate clause then surfaces an error."
  [types]
  (let [m (file-types)
        exts (map #(:ext (get m %)) types)]
    (when (every? some? exts)
      (set exts))))

(defn version-string []
  (let [resource (or (jio/resource "build/version.edn")
                     (let [f (jio/file "gen-resources/build/version.edn")]
                       (when (.exists f) f)))]
    (if-not resource
      "dev"
      (with-open [io-reader (jio/reader resource)
                  pb-reader (PushbackReader. io-reader)]
        (let [{:keys [timestamp ref-short version dirty?]} (edn/read pb-reader)
              ts     (or timestamp (quot (System/currentTimeMillis) 1000))
              fmt    (SimpleDateFormat. "yyyy.MM.dd HH:mm:ss")
              date   (.format fmt (Date. (long (* ts 1000))))]
          (str version " - " ref-short " - " date (when dirty? " +")))))))

;; TODO: add search-by-hash param
;; TODO: add -d output directory when using c

(defn cli-options []
  (reformat-options
    max-width
    [["-n" "--name <regex>"   "match against file name"
      :parse-fn re-pattern]
     ["-p" "--path <regex>"   "match against path relative to search-root"
      :parse-fn re-pattern]
     ["-a" "--apath <regex>"  "match against absolute path"
      :parse-fn re-pattern]
     ["-g" "--grep <regex>"   "match against file content lines"
      :parse-fn re-pattern]
     ["-f" "--flags <flags>"
      "regex flags applied to every pattern. Combine any of: i (case-insensitive), m (multiline), s (dotall), u (unicode-case), x (comments), d (unix-lines)."]
     ["-t" (str "--types <" (file-type-selectors) ">")
      (str "restrict file types: " (file-type-descriptions) ". Default: "
           (str/join (map first (default-file-types))))
      :default (default-file-type-exts)
      :parse-fn parse-types
      :validate [some? (str "type must be a non-empty combination of "
                            (file-type-selectors))]]

     ["-c" "--cat"
      "print the entire contents of matching files (with line numbers)"]
     ["-l" "--files-only"
      "with -g, print one path per matching file instead of every matching line"]
     ["-s" "--hash <algo>"
      (str "print file hash(es). Algorithms: " (hash-selectors)
           ". Repeat -s to print multiple.")
      :parse-fn parse-hash-selector
      :assoc-fn (fn [m k v] (update m k (fnil conj []) v))
      :validate [some? (str "hash must be one of " (hash-selectors))]]

     ["-x" "--context <#>"
      "with -g, show <#> lines of context around each match (default 0)"
      :parse-fn #(Integer/parseInt %)]
     ["-o" "--out-file <path>"
      "with -c, append output to file instead of stdout"
      :parse-fn jio/as-file]
     ["-m" "--monochrome"
      "disable ANSI coloring of matches"]

     [nil "--all"
      "traverse every directory. By default findjar skips .git, .svn, .hg, node_modules, target, build, .gradle, .cpcache, .idea, .vscode."]
     [nil "--no-parallel"
      "scan files serially (default is parallel using ~ cores+2 workers)"
      :id :no-parallel]
     [nil "--parallel-jobs <n>"
      "cap the number of concurrent scan workers. Useful on HDD or networked filesystems."
      :id :parallel-jobs
      :parse-fn #(Integer/parseInt %)
      :validate [pos? "must be a positive integer"]]

     [nil "--examples"  "print usage examples and exit"]
     [nil "--profile"   "enable tufte profiling (developer)"]
     ["-h" "--help"     "show this help and exit"]]))

;; Option groups for the help summary. Each group is rendered with its own
;; heading; ids must match the auto-derived ids in cli-options above.

(def option-groups
  [["Filtering"  [:name :path :apath :grep :flags :types]]
   ["Action"     [:cat :files-only :hash]]
   ["Output"     [:context :out-file :monochrome]]
   ["Scanning"   [:all :no-parallel :parallel-jobs]]
   ["Misc"       [:examples :profile :help]]])

(def usage-text
  ["findjar — search files and the contents of jar/zip archives"
   ""
   "Usage:  findjar [<search-root>...] [options]"
   ""
   "With no <search-root>, searches the current directory. Multiple roots are"
   "scanned in order; each result's path is prefixed with the root it came"
   "from (override with -a)."
   ""
   "Path patterns (-p, -a) match against:"
   "  - <relative-or-absolute-path>          for files on disk"
   "  - <path-to-jar>@<path-within-jar>      for entries inside jar/zip archives"
   ""
   "Without -c / -s / -g, findjar prints the path of each matching file."
   "With -g, prints matching content lines (or paths only when combined with -l)."
   "With -c, prints file contents. With -s, prints file hashes."
   ""
   "Run 'findjar --examples' for usage examples."])

(defn- summarize-group
  "Render one option group: heading + a column-aligned block of its options."
  [global-lens header parts-by-id ids]
  (let [parts (keep parts-by-id ids)]
    (when (seq parts)
      (str/join "\n"
                (cons (str header ":")
                      (cli/format-lines global-lens parts))))))

(defn summarize
  "Render the option summary, grouped per option-groups. Column widths are
  computed across all options so groups stay aligned with each other."
  [specs]
  (if (seq specs)
    (let [parts        (mapv (partial cli/make-summary-part false) specs)
          lens         (apply map (fn [& cols] (apply max (map count cols))) parts)
          parts-by-id  (zipmap (map :id specs) parts)]
      (->> option-groups
           (keep (fn [[header ids]] (summarize-group lens header parts-by-id ids)))
           (str/join "\n\n")))
    ""))

(defn usage [summary]
  (str (str/join \newline usage-text)
       "\n\n"
       summary
       "\n\n"
       "findjar " (version-string)
       "\n"))


(defn colorize
  "Render a line of text, optionally using ANSI colors when :monochrome is
  unset in opts."
  [{:keys [monochrome]} line]
  (let [old-value (Ansi/isEnabled)]
    (try
      (Ansi/setEnabled (not monochrome))
      (ansi/render line)
      (finally
        (Ansi/setEnabled old-value)))))

(def examples-text
  [""
   "Examples:"
   ""
   "(paths abbreviated with '...' for readability)"
   ""
   "  1. List every file under the current directory (and inside any .jar"
   "     entries). With no search-root, findjar defaults to '.':"
   ""
   "     @|bold ~> findjar|@"
   ""
   "     src/foo.clj"
   "     src/bar/baz.clj"
   "     .../some-lib.jar@META-INF/MANIFEST.MF"
   "     ..."
   ""
   "  2. Find files by name across a maven cache. -n matches the file"
   "     name, -p matches the (relative) path, -a matches the absolute"
   "     path:"
   ""
   "     @|bold ~> findjar ~/.m2 -n \"string.clj\"|@"
   ""
   "     .../clojure-1.9.0.jar@clojure/string.clj"
   "     .../clojure-1.7.0.jar@clojure/string.clj"
   "     .../octet-1.1.0.jar@octet/spec/string.cljc"
   "     ..."
   ""
   "  3. Grep file contents with a regex. -t j restricts to entries inside"
   "     jar files; -x N adds N lines of context around each match:"
   ""
   "     @|bold ~> findjar ~/.m2 -n clj -g \"Rich Hickey\" -t j -x 1|@"
   ""
   "     .../clojure-1.9.0.jar@clojure/set.clj 9  (ns ^{:doc \"Set ops..."
   "     .../clojure-1.9.0.jar@clojure/set.clj:10       :@|red author \"Rich Hickey|@\"}"
   "     .../clojure-1.9.0.jar@clojure/set.clj 11       clojure.set)"
   "     ..."
   ""
   "  4. Print only the paths of files that contain a match. Pipe to your"
   "     editor or to xargs:"
   ""
   "     @|bold ~> findjar . -g \"TODO\" -l|@"
   ""
   "     src/foo.clj"
   "     src/bar/baz.clj"
   ""
   "     @|bold ~> findjar . -g \"TODO\" -l | xargs $EDITOR|@"
   ""
   "  5. Cat an entry inside a jar (with line numbers and intra-line"
   "     highlighting when combined with -g):"
   ""
   "     @|bold ~> findjar ~/.m2 -n MANIFEST.MF -c -t j|@"
   ""
   "     @|red <<<<<<<|@ .../clojure-1.9.0.jar@META-INF/MANIFEST.MF"
   "     @|green 1|@ Manifest-Version: 1.0"
   "     @|green 2|@ Created-By: Apache Maven"
   "     @|green 3|@ Main-Class: clojure.main"
   "     @|red >>>>>>>|@"
   ""
   "  6. Compute one or more file hashes. Each line includes the algorithm"
   "     name so multiple -s flags are unambiguous:"
   ""
   "     @|bold ~> findjar ~/.m2 -n string.clj -t j -s sha1 -s md5|@"
   ""
   "     94a86681b58d556f1eb13a... sha1 .../clojure-1.9.0.jar@clojure/string.clj"
   "     f05f65fa44628a954970...   md5  .../clojure-1.9.0.jar@clojure/string.clj"
   "     7444756fa91b65...         sha1 .../clojure-1.7.0.jar@clojure/string.clj"
   "     ..."
   ""
   "  7. Search several roots in one invocation. Result paths include the"
   "     root prefix so they're unambiguous:"
   ""
   "     @|bold ~> findjar ~/.m2 ~/.gradle -n core.clj -t j|@"
   ""
   "     /Users/me/.m2/.../clojure-1.11.1.jar@clojure/core.clj"
   "     /Users/me/.gradle/caches/.../core.clj"
   "     ..."
   ""
   "  8. By default findjar skips .git, node_modules, target, build,"
   "     .gradle, .cpcache, .idea, .vscode, .svn, .hg. Use --all to"
   "     traverse everything:"
   ""
   "     @|bold ~> findjar . -g build-version --all|@"
   ""
   "  9. Case-insensitive matching: -f i applies the regex CASE_INSENSITIVE"
   "     flag to every pattern. Other flags: m s u x d (see --help):"
   ""
   "     @|bold ~> findjar ~/.m2 -g \"rich hickey\" -fi -t j|@"
   ""
   " 10. For very large scans (HDD or networked filesystems), bound the"
   "     parallel worker count or scan serially:"
   ""
   "     @|bold ~> findjar /mnt/slow-disk -g 'TODO' --parallel-jobs 4|@"
   "     @|bold ~> findjar /mnt/slow-disk -g 'TODO' --no-parallel|@"
   ""])

(defn examples
  "return a display string with the example usages"
  [opts]
  (->> examples-text
       ;(map #(wrap-line MAX_WIDTH %) lines)
       (map #(colorize opts %))
       (str/join \newline)))

(defn error-msg
  "Render a display string for one or more errors. The errors come first so
  they're visible without scrolling; a hint follows. Full usage is one
  --help away, no need to dump it on every typo."
  [errors _summary]
  (str "findjar: error" (when (< 1 (count errors)) "s") ":\n"
       (str/join \newline (map #(str "  " %) errors))
       "\n\nTry 'findjar --help' for more information.\n"))



(defn english-list [args]
  (condp = (count args)
    0 ""
    1 (first args)
    2 (str (first args) " and " (last args))
    (let [xs (interpose ", " args)]
      (apply str (concat (butlast xs) ["and " (last xs)])))))

(defn validate-args
  "Parse and validate command line arguments."
  [args]
  (let [parsed (cli/parse-opts args (cli-options)
                               :strict true
                               :summary-fn summarize)
        {:keys [options arguments errors summary]} parsed
        fail   (fn [msg] {:exit-message (error-msg [msg] summary)})
        opts   (-> options
                   (assoc :parallel (not (:no-parallel options)))
                   (dissoc :no-parallel))
        ;; No positional => search current directory.
        roots-strs  (if (empty? arguments) ["."] arguments)
        search-roots (mapv jio/file roots-strs)
        bad-roots    (remove #(.isDirectory ^java.io.File %) search-roots)]
    (cond
      (:examples options)
      {:exit-message (examples options) :ok? true}

      (:help options)
      {:exit-message (usage summary) :ok? true}

      (and (:apath options) (:path options))
      (fail "can not use path (-p) and apath (-a) together")

      errors
      {:exit-message (error-msg errors summary)}

      (seq bad-roots)
      (fail (str "non-directory search root"
                 (when (< 1 (count bad-roots)) "s") ": "
                 (english-list (mapv str bad-roots))))

      :else
      {:search-roots search-roots :opts opts})))

(defn exit [status msg]
  ;; --help / --examples (status 0) go to stdout; errors (non-zero) go to
  ;; stderr so callers piping stdout to other tools don't see them.
  (binding [*out* (if (zero? status) *out* *err*)]
    (println msg))
  (System/exit status))

