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
   "(some paths etc have been omitted/abbreviated for brevity)"
   ""
   "  1. list all files in maven cache (~/.m2), both directly on disk and "
   "     within jar files:"
   ""
   "     @|bold ~> findjar ~/.m2|@"
   ""
   "     .../1.6.1/nightlight-1.6.1.pom"
   "     .../1.6.1/nightlight-1.6.1.jar.sha1"
   "     .../1.6.1/nightlight-1.6.1.jar@META-INF/.../nightlight/pom.properties"
   "     ..."
   ""
   "  2. list all files where file name (-n) matches pattern, both directly on disk"
   "     and within jar files:"
   ""
   "     @|bold ~> findjar ~/.m2 -n \"string.clj\"|@"
   ""
   "     .../clojure-1.9.0.jar@clojure/string.clj"
   "     .../clojure-1.7.0.jar@clojure/string.clj"
   "     .../clojure-1.8.0.jar@clojure/string.clj"
   "     ...octet-1.1.0.jar@octet/spec/string.cljc"
   "     ..."
   ""
   "  3. list all files where both file name (-n) and a line in the file "
   "     content (-g) matches pattern. Print out matching lines with line "
   "     numbers, highlight intra-line content matches. Search only files "
   "     within jar files (-t) (ignore normal, directly on disk files):"
   ""
   "     @|bold ~> findjar clojure/1.9.0 -n \"clj\" -g \"author.*Rich Hickey\" -t j|@"
   ""
   "     .../clojure-1.9.0.jar@clojure/set.clj:10       :@|red author \"Rich Hickey|@\"}"
   "     .../clojure-1.9.0.jar@clojure/zip.clj:14        :@|red author \"Rich Hickey|@\"}"
   "     .../clojure-1.9.0.jar@clojure/inspector.clj:10  :@|red author \"Rich Hickey|@\"}"
   "     .../clojure-1.9.0.jar@clojure/xml.clj:10       :@|red author \"Rich Hickey|@\"}"
   "     ..."
   ""
   "    (with the matched string highlighted)"
   ""
   "  4. same as above, but include one surrounding line of \"context\" (-x)"
   "     when printing the matching lines:"
   ""
   "     @|bold ~> findjar clojure/1.9.0 -n \"clj\" -g \"Rich Hickey\" -t j -x 1|@"
   ""
   "     .../clojure-1.9.0.jar@clojure/set.clj 9  (ns ^{:doc \"Set operations..."
   "     .../clojure-1.9.0.jar@clojure/set.clj:10        :@|red author \"Rich Hickey|@\"}"
   "     .../clojure-1.9.0.jar@clojure/set.clj 11        clojure.set)"
   "     .../clojure-1.9.0.jar@clojure/zip.clj 13   and enumeration.  See Huet"
   "     .../clojure-1.9.0.jar@clojure/zip.clj:14        :@|red author \"Rich Hickey|@\"}"
   "     .../clojure-1.9.0.jar@clojure/zip.clj 15   clojure.zip"
   "     ..."
   ""
   "    (with the matched string highlighted)"
   ""
   "  5. find all files where name (-n) matches pattern and dump (-c) them on"
   "     stdout, search both directly on disk and within jar files:"
   ""
   "     @|bold ~> findjar clojure/1.9.0 -n \"MANIFEST.MF\" -c|@"
   ""
   "     @|red <<<<<<<|@ clojure-1.9.0.jar@META-INF/MANIFEST.MF"
   "     @|green 1|@ Manifest-Version: 1.0"
   "     @|green 2|@ Archiver-Version: Plexus Archiver"
   "     @|green 3|@ Created-By: Apache Maven"
   "     @|green 4|@ Built-By: jenkins"
   "     @|green 5|@ Build-Jdk: 1.7.0"
   "     @|green 6|@ Main-Class: clojure.main"
   "     @|green 7|@"
   "     @|red >>>>>>>|@"
   ""
   "  6. find all files where content (-g) matches pattern and dump (-c) them "
   "     on stdout, intra-line highlight the matching content lines, search "
   "     both directly on disk and within jar files:"
   ""
   "     @|bold ~> findjar clojure/1.9.0 -g \"Plexus\" -c|@"
   ""
   "     @|red <<<<<<<|@ clojure-1.9.0.jar@META-INF/MANIFEST.MF"
   "     @|green 1|@ Manifest-Version: 1.0"
   "     @|green 2|@ Archiver-Version: @|red Plexus|@ Archiver"
   "     @|green 3|@ Created-By: Apache Maven"
   "     @|green 4|@ Built-By: jenkins"
   "     @|green 5|@ Build-Jdk: 1.7.0"
   "     @|green 6|@ Main-Class: clojure.main"
   "     @|green 7|@"
   "     @|red >>>>>>>|@"
   ""
   "    (with the word \"Plexus\" on line 2 highlighted)"
   ""
   "  7. find all files where both file name (-n) and content (-g) matches pattern"
   "     and dump them on stdout:"
   ""
   "     @|bold ~> findjar clojure/1.9.0 -n \"properties\" -g \"groupId\" -c|@"
   ""
   "     @|red <<<<<<<|@ clojure-1.9.0.jar@META-INF/...clojure/pom.properties"
   "     @|green 1|@ #Generated by Maven"
   "     @|green 2|@ #Fri Dec 08 08:01:54 CST 2017"
   "     @|green 3|@ version=1.9.0"
   "     @|green 4|@ @|green groupId|@=org.clojure"
   "     @|green 5|@ artifactId=clojure"
   "     @|red >>>>>>>|@"
   ""
   "    (with the word \"groupId\" on line 4 highlighted)"
   ""
   "  8. find all files where name (-n) matches pattern and calculate "
   "     sha1 and md5 hash (-s) for them:"
   ""
   "     @|bold ~> findjar clojure/1.9.0 -n \"clj\" -s sha1 -s md5|@"
   ""
   "     ...94a86681b58d556f1eb13a clojure-1.9.0.jar@clojure/string.clj"
   "     ...f05f65fa44628a95497032 clojure-1.9.0.jar@clojure/set.clj"
   "     ...7444756fa91b65 clojure-1.9.0.jar@clojure/string.clj"
   "     ...95aaa8c6e9af74 clojure-1.9.0.jar@clojure/set.clj"
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

