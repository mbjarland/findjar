(ns findjar.cli
  (:require [clojure.edn :as edn]
            [clojure.java.io :as jio]
            [clojure.pprint :as cpp]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [findjar.core :as c]
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

(defn un-whitespace [^String line]
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
     ["-w" "--word-regexp"
      "with -g, match only at word boundaries (wraps the pattern in \\b…\\b)"
      :id :word]
     ["-v" "--invert-match"
      "with -g, emit lines that do NOT match the pattern"
      :id :invert]
     ["-f" "--flags <flags>"
      "regex flags applied to every pattern. Combine any of: i (case-insensitive), m (multiline), s (dotall), u (unicode-case), x (comments), d (unix-lines)."
      :validate [#(empty? (c/unknown-flag-chars %))
                 "must be a combination of i, m, s, u, x, d"]]
     ["-i" "--ignore-case"
      "shortcut for adding 'i' to --flags (case-insensitive match)"
      :id :ignore-case]
     ["-t" (str "--types <" (file-type-selectors) ">")
      (str "restrict file types: " (file-type-descriptions) ". Default: "
           (str/join (map first (default-file-types))))
      :default (default-file-type-exts)
      :parse-fn parse-types
      :validate [some? (str "type must be a non-empty combination of "
                            (file-type-selectors))]]

     ["-c" "--cat"
      "print the entire contents of matching files (with line numbers)"]
     [nil "--manifest"
      "for each matched .jar/.zip, print its META-INF/MANIFEST.MF (and any pom.properties). Implies -t j."
      :id :manifest]
     [nil "--class-info"
      "for each matched .class entry, print class name, super, interfaces, and method signatures (parsed via ASM)"
      :id :class-info]
     ["-l" "--files-only"
      "with -g, print one path per matching file instead of every matching line"]
     [nil "--count"
      "with -g, print only the count of matching lines per file ('<path>:<n>')"
      :id :count]
     [nil "--max-count <n>"
      "with -g, stop after <n> matching lines per file"
      :id :max-count
      :parse-fn #(Integer/parseInt %)
      :validate [pos? "must be a positive integer"]]
     ["-s" "--hash <algo>"
      (str "print file hash. Output format: '<hex> <algo> <path>'. Algorithms: "
           (hash-selectors) ". Repeat -s to print several.")
      :parse-fn parse-hash-selector
      :assoc-fn (fn [m k v] (update m k (fnil conj []) v))
      :validate [some? (str "hash must be one of " (hash-selectors))]]
     [nil "--find-by-hash <algo:hex>"
      "find every file whose hash equals <hex>, e.g. sha1:da39a3ee.... Repeatable."
      :id :find-by-hash
      :assoc-fn (fn [m k v] (update m k (fnil conj []) v))]
     ["-q" "--quiet"
      "suppress all output. Exit status is always grep-compatible: 0 if any match was emitted, 1 if not, 2 on bad args"]

     ["-x" "--context <#>"
      "with -g, show <#> lines of symmetric context around each match"
      :parse-fn #(Integer/parseInt %)
      :validate [#(<= 0 %) "must be >= 0"]]
     ["-A" "--after <#>"
      "with -g, show <#> lines of context after each match"
      :id :after
      :parse-fn #(Integer/parseInt %)
      :validate [#(<= 0 %) "must be >= 0"]]
     ["-B" "--before <#>"
      "with -g, show <#> lines of context before each match"
      :id :before
      :parse-fn #(Integer/parseInt %)
      :validate [#(<= 0 %) "must be >= 0"]]
     ["-G" "--glob <pattern>"
      "match against file name as a glob (e.g. '*.clj') instead of regex (-n)"]
     [nil "--output <fmt>"
      "output format: text (default), json or ndjson (one JSON object per line, the JSON Lines format), or json-array (single top-level JSON array, jq-friendly without -s)"
      :id :output
      :default :text
      :parse-fn keyword
      :validate [#{:text :json :ndjson :json-array}
                 "must be 'text', 'json', 'ndjson', or 'json-array'"]]
     ["-o" "--out-file <path>"
      "with -c, append output to file instead of stdout"
      :parse-fn jio/as-file]
     ["-m" "--monochrome"
      "disable ANSI coloring of matches (NO_COLOR env var also honored)"]
     ["-0" "--null"
      "terminate path-listing output with NUL instead of newline (for xargs -0)"
      :id :null]

     [nil "--all"
      "traverse every directory. By default findjar skips .git, .svn, .hg, node_modules, target, build, .gradle, .cpcache, .idea, .vscode, plus any paths matched by .gitignore at each search-root."]
     ["-L" "--follow"
      "follow symbolic links (default: don't follow)"]
     [nil "--max-depth <n>"
      "do not descend more than <n> directory levels below each search-root"
      :id :max-depth
      :parse-fn #(Integer/parseInt %)
      :validate [#(<= 0 %) "must be >= 0"]]
     [nil "--exclude <name>"
      "skip directories with this name (repeatable). Adds to the default exclude list."
      :id :exclude
      :assoc-fn (fn [m k v] (update m k (fnil conj #{}) v))]
     [nil "--no-gitignore"
      "do not honor .gitignore files in search-roots"
      :id :no-gitignore]
     [nil "--text"
      "with -g, do not skip files that look binary (NUL bytes in first 8KB)"
      :id :text]
     [nil "--no-parallel"
      "scan files serially (default is parallel using ~ cores+2 workers)"
      :id :no-parallel]
     [nil "--parallel-jobs <n>"
      "cap the number of concurrent scan workers. Useful on HDD or networked filesystems."
      :id :parallel-jobs
      :parse-fn #(Integer/parseInt %)
      :validate [pos? "must be a positive integer"]]
     [nil "--nested"
      "recurse into jars/zips that appear as entries inside other jars/zips. Path takes the form outer.jar@inner.jar@entry."
      :id :nested]

     [nil "--stats"
      "after the scan, print a one-line summary (archives + entries + hits + elapsed) to stderr"
      :id :stats]
     [nil "--examples"  "print usage examples and exit"]
     [nil "--completions <shell>"
      "print shell completion script (zsh|bash|fish) and exit"
      :id :completions
      :parse-fn keyword
      :validate [#{:zsh :bash :fish} "must be 'zsh', 'bash', or 'fish'"]]
     [nil "--profile"   "enable tufte profiling (developer)"]
     ["-V" "--version"  "print version and exit"]
     ["-h" "--help"     "show this help and exit"]]))

;; Option groups for the help summary. Each group is rendered with its own
;; heading; ids must match the auto-derived ids in cli-options above.

(def option-groups
  [["Filtering"  [:name :path :apath :glob :grep :word :invert :flags :ignore-case :types]]
   ["Action"     [:cat :manifest :class-info :files-only :count :max-count
                  :hash :find-by-hash :quiet]]
   ["Output"     [:context :after :before :output :out-file :monochrome :null]]
   ["Scanning"   [:all :follow :max-depth :exclude :no-gitignore :text
                  :no-parallel :parallel-jobs :nested]]
   ["Misc"       [:stats :examples :completions :profile :version :help]]])

(defn- load-resource
  "Slurp a packaged text resource. Used for help / examples text so cli.clj
  stays small and non-coders can edit them directly."
  [path]
  (or (some-> (jio/resource path) slurp)
      ;; Fallback for repl runs from the project root (no uberjar yet).
      (let [f (jio/file "resources" path)]
        (when (.exists f) (slurp f)))
      ""))

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
  (str (load-resource "findjar/usage.txt")
       "\n"
       summary
       "\n\n"
       "findjar " (version-string)
       "\n"))


(defn- no-color-env? []
  (let [v (System/getenv "NO_COLOR")]
    (and (some? v) (not= "" v))))

(defn colorize
  "Render a line of text, optionally using ANSI colors. Disabled when
  --monochrome is set or NO_COLOR is in the environment."
  [{:keys [monochrome]} line]
  (let [old-value (Ansi/isEnabled)
        on?       (and (not monochrome) (not (no-color-env?)))]
    (try
      (Ansi/setEnabled on?)
      (ansi/render line)
      (finally
        (Ansi/setEnabled old-value)))))

(defn examples
  "Render the packaged examples resource, applying jansi colour markers
  unless --monochrome / NO_COLOR disable them."
  [opts]
  (->> (load-resource "findjar/examples.txt")
       str/split-lines
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

(defn- parse-find-by-hash
  "Parse --find-by-hash values like 'sha1:da39a3ee...'. Returns a map
  {:algo :sha1 :hex \"da39a3ee...\"} or :error with a reason."
  [s]
  (if-let [[_ algo-str hex] (re-matches #"([^:]+):([0-9a-fA-F]+)" s)]
    (if-let [algo (c/hash-by-desc algo-str)]
      {:algo algo :hex (str/lower-case hex)}
      {:error (str "unknown algorithm '" algo-str "' in --find-by-hash; "
                   "must be one of " (str/join ", "
                                                (map :desc (vals c/hash-algorithms))))})
    {:error (str "--find-by-hash must be of the form <algo>:<hex>, got '" s "'")}))

(defn- compile-glob ^java.util.regex.Pattern [^String pat]
  ;; Translate a simple shell-style glob into a regex that matches the whole
  ;; file name. Supports * (anything-except-/), ? (any single char), and
  ;; [abc] character classes; everything else is escaped literally. This is
  ;; intentionally minimal — no extglob, no globstar, no brace expansion.
  (let [sb (StringBuilder. "^")]
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
    (.append sb "$")
    (java.util.regex.Pattern/compile (.toString sb))))

(defn validate-args
  "Parse and validate command line arguments."
  [args]
  (let [parsed (cli/parse-opts args (cli-options)
                               :strict true
                               :summary-fn summarize)
        {:keys [options arguments errors summary]} parsed
        fail   (fn [msg] {:exit-message (error-msg [msg] summary)})
        ;; Promote --no-parallel into a positive :parallel boolean and merge
        ;; -G glob into the :name regex slot (failing if both are given).
        glob   (:glob options)
        glob-pat (when glob (compile-glob glob))
        fbh-raw  (:find-by-hash options)
        fbh-parsed (when fbh-raw (mapv parse-find-by-hash fbh-raw))
        fbh-err  (some :error fbh-parsed)
        opts   (cond-> options
                 true             (assoc :parallel (not (:no-parallel options)))
                 true             (dissoc :no-parallel)
                 glob-pat         (assoc :name glob-pat)
                 fbh-parsed       (assoc :find-by-hash fbh-parsed)
                 ;; -w wraps the grep pattern with \b boundaries. Done
                 ;; before -f flags are applied (in core/munge-regexes), so
                 ;; the wrapped form picks up the user's flags too.
                 (and (:word options) (:grep options))
                 (update :grep (fn [^java.util.regex.Pattern p]
                                 (re-pattern (str "\\b(?:" (.pattern p) ")\\b")))))
        ;; No positional => search current directory.
        roots-strs   (if (empty? arguments) ["."] arguments)
        search-roots (mapv jio/file roots-strs)
        ;; A search-root can be a directory (recursive walk) or a file
        ;; (treated as a one-element seq — useful for 'findjar app.jar
        ;; --manifest' or 'findjar foo.jar -t j -g pat'). Anything that
        ;; doesn't exist is rejected.
        bad-roots    (remove (fn [^java.io.File f] (.exists f)) search-roots)]
    (cond
      (:version options)
      {:exit-message (str "findjar " (version-string)) :ok? true}

      (:examples options)
      {:exit-message (examples options) :ok? true}

      (:completions options)
      ;; Each script lives at resources/findjar/completions/<shell>.
      ;; Pipe to the right place per your shell:
      ;;   zsh:  > ${fpath[1]}/_findjar
      ;;   bash: > /etc/bash_completion.d/findjar
      ;;   fish: > ~/.config/fish/completions/findjar.fish
      {:exit-message (str/trimr
                       (load-resource (str "findjar/completions/"
                                           (name (:completions options)))))
       :ok? true}

      (:help options)
      {:exit-message (usage summary) :ok? true}

      (and (:apath options) (:path options))
      (fail "can not use path (-p) and apath (-a) together")

      (and glob (:name options))
      (fail "can not use --glob (-G) and --name (-n) together")

      fbh-err
      (fail fbh-err)

      errors
      {:exit-message (error-msg errors summary)}

      (seq bad-roots)
      (fail (str "search root not found"
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

