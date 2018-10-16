(ns findjar.cli
  (:require [findjar.core :as c]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [clojure.java.io :refer [file]])
  (:gen-class)
  (:import [java.nio.file Paths]))

(def MAX_WIDTH 78)

(defn multimethod-meta [multi]
  (reduce
   (fn [a [k v]]
     (assoc a k (v k)))
   {}
   (methods multi)))

(defn hash-selectors []
  (str/join
   ", "
   (map (fn [[k v]] (:desc v))
        (multimethod-meta c/calculate-hash))))

(defn hash-keys []
  (keys (multimethod-meta c/calculate-hash)))

(defn parse-hash-desc [desc]
  (let [m (reduce
           (fn [a [k v]] (assoc a (:desc v) k))
           {}
           (multimethod-meta c/calculate-hash))]
    (m desc)))

(defn file-types []
  (reduce
   (fn [a [k v]]
     (assoc a (:char v)
              {:desc    (:desc v)
               :default (:default v)
               :ext     k}))
   {}
   (multimethod-meta c/file-finder)))

(defn file-type-selectors []
  (str/join "|" (keys (file-types))))

(defn file-type-descriptions []
  (str/join ", "
            (map
             (fn [[k v]] (str k " - " (:desc v)))
             (file-types))))

(defn default-file-types []
  (filter
   (fn [[_ v]] (:default v))
   (file-types)))

(defn default-file-type-exts []
  (mapv (fn [[k v]] (:ext v)) (default-file-types)))

(defn wrap-line [width line]
  (let [words (str/split line #" ")]
    (clojure.pprint/cl-format nil (str "~{~<~%~1," (dec width) ":;~A~> ~}") words)))

(defn un-whitespace [str]
  (str/replace str #"\s+" " "))

(defn wrap-desc [width margin desc]
  (let [line    (un-whitespace desc)
        wrapped (wrap-line width line)
        lines   (str/split wrapped #"\n")]
    (str/join (str \newline margin) lines)))

(defn wrap-opts [width margin opts]
  "apply a function to the descriptions of the command line opts,
  returning a new set of opts with the altered descriptions"
  (reduce
   (fn [c [short long desc & rest]]
     (let [modded (wrap-desc width margin desc)]
       (conj c (into [short long modded] rest))))
   []
   opts))

;TODO: move this formatting into summarize
(defn reformat-options [max-width opts]
  "reformat the command line params for a clean output when printing usage"
  (let [max-long-desc  (apply max (map (comp count second) opts))
        margin         (apply str (repeat (+ 2 3 1 max-long-desc 2) " "))
        max-desc-width (- max-width (+ max-long-desc 8))]
    (wrap-opts max-desc-width margin opts)))

(defn parse-types [types]
  (let [m (file-types)]
    (map #(:ext (get m %)) types)))

(defn cli-options []
  (reformat-options
   MAX_WIDTH
   [;; First three strings describe a short-option, long-option with optional
    ;; example argument description, and a description. All three are optional
    ;; and positional.
    ["-p"
     "--path <regex>"
     "a pattern to match against the relative path (including file name) starting from search-root"
     :parse-fn #(re-pattern %)]
    ["-a"
     "--apath <regex>"
     "a pattern to match against the absolute path (including file name)"
     :parse-fn #(re-pattern %)]
    ["-n"
     "--name <regex>"
     "a pattern to match against file names"
     :parse-fn #(re-pattern %)]
    ["-g"
     "--grep <regex>"
     "a pattern to match against file content lines"
     :parse-fn #(re-pattern %)]
    ["-t"
     (str "--types <" (file-type-selectors) ">")
     (str "restrict the files searched to only the type(s) specified. "
          "The list of supported file types is extensible. Available file types: "
          (file-type-descriptions))
     :default (default-file-type-exts)
     :parse-fn parse-types
     :validate [#(every? (comp not nil?) %) (str "type must be one of " (file-type-selectors))]]
    ["-x"
     "--context <#>"
     "If -c is given, show <# of lines> lines of context around the match, defaults to 0"
     :parse-fn #(Integer/parseInt %)]
    ["-o"
     "--out-file <path>"
     "when using -c (cat file), write the contents of the located file(s) to the output file"
     :parse-fn #(clojure.java.io/as-file %)]
    ["-f"
     "--flags <flags>"
     "turns on regex flags for all matches used. Example: -f i turns on case insensitive matching for both file names and content. See oracle javadocs on
      Pattern.html#special for details on java regex flags"]

    ["-c"
     "--cat"
     "cat file. For matching files, print the entire file contents on the console"]

    ["-m"
     "--monochrome"
     "turn off ansi-coloring of matching content lines"]

    ["-s" "--hash <algo>"
     (str "calculate file hash(es) for matched files. Available algorithms: "
          (hash-selectors))
     :parse-fn parse-hash-desc
     :assoc-fn (fn [m k v] (update-in m [k] #(into [] (conj % v))))
     :validate [#(boolean %) (str "hash must be one of " (hash-selectors) "!")]]

    [nil "--profile"
     "internal developer option - enable profiling"]

    [nil "--examples"
     "print out usage examples"]
    ;["-n"
    ; "--no-color"
    ; "turn off ansi coloring of matches"]

    ;["-m" "--md5" "print md5 hash of matched files"]
    ;["-s" "--sha1" "print sha1 hash of matched files"]
    ["-h"
     "--help"
     "show usage information"]]))

(defn usage [summary]
  (as-> [""
         "findjar - a tool for searching through files, including files inside jars"
         ""
         "usage: findjar <search-root> [-p <path-pattern>] [-g <content-pattern>]  [...]"
         ""
         "findjar searches for files in any disk structure. It is capable of
          looking both for/in normal files and also for/in files inside
          zip/jar files. It is capable of regex matching both for file name/path
          and for content within the files."
         ""
         "This tool is in essence an improvement of the unix find command
          geared towards solving a common problem for programmers on the JVM:
          finding that specific file or class in your maven repo, classpath, etc
          when that file can reside either directly on disk or inside a jar archive."
         ""
         "Note that this tool is capable of a few extra tricks such as writing
          out the contents of matched files inside jar files and calculating
          md5 or sha1 hashes of matched files inside jar files."
         ""
         "For regular files the path pattern (-p) matches against the entire path,"
         "including the file name, i.e.:"
         ""
         "   ~> findjar ~/.m2 -p '.*asm/asm/3.2.*pom'"
         ""
         "   repository/asm/asm/3.2/asm-3.2.pom"
         ""
         "whereas for files within jar files, the path pattern matches the string:"
         ""
         "   <path-to-jar-file>@<path-within-jar-file>"
         ""
         "i.e:"
         ""
         "   ~> findjar ~/.m2 -p '.*asm/asm.*Edge.class'"
         ""
         "   repository/asm/asm/3.2/asm-3.2.jar@org/objectweb/asm/Edge.class"
         ""
         "Command line switches can be provided either using short form i.e. '-t j'
          or long form i.e. '--type j'."
         ""
         "For usage examples, run: ~> findjar --examples"
         ""
         "Options:"]
        lines
        (map un-whitespace lines)
        (map #(wrap-line MAX_WIDTH %) lines)
        (str/join \newline lines)
        (str lines "\n" summary "\n")))

(defn examples []
  ;;TODO: conditionally jansi color code example results
  (as-> ["Examples:"
         "(some paths etc have been omitted/abbreviated for brevity)"
         ""
         "  1. list all files in maven cache (~/.m2), both directly on disk and "
         "     within jar files:"
         ""
         "     ~> findjar ~/.m2"
         ""
         "     .../1.6.1/nightlight-1.6.1.pom"
         "     .../1.6.1/nightlight-1.6.1.jar.sha1"
         "     .../1.6.1/nightlight-1.6.1.jar@META-INF/.../nightlight/pom.properties"
         "     ..."
         ""
         "  2. list all files where file name matches pattern, both directly on disk"
         "     and within jar files:"
         ""
         "     ~> findjar ~/.m2 -n \"string.clj\""
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
         "     within jar files (ignore normal, directly on disk files):"
         ""
         "     ~> findjar clojure/1.9.0 -n \"clj\" -g \"author.*Rich Hickey\""
         ""
         "     .../clojure-1.9.0.jar@clojure/set.clj:10       :author \"Rich Hickey\"}"
         "     .../clojure-1.9.0.jar@clojure/zip.clj:14        :author \"Rich Hickey\"}"
         "     .../clojure-1.9.0.jar@clojure/inspector.clj:10  :author \"Rich Hickey\"}"
         "     .../clojure-1.9.0.jar@clojure/xml.clj:10       :author \"Rich Hickey\"}"
         "     ..."
         "    (with the matched string highlighted)"
         ""
         "  4. same as above, but include one surrounding line of \"context\""
         "     when printing the matching lines:"
         ""
         "     ~> findjar clojure/1.9.0 -n \"clj\" -g \"Rich Hickey\" -t j -x 1"
         ""
         "     .../clojure-1.9.0.jar@clojure/set.clj 9  (ns ^{:doc \"Set operations..."
         "     .../clojure-1.9.0.jar@clojure/set.clj:10        :author \"Rich Hickey\"}"
         "     .../clojure-1.9.0.jar@clojure/set.clj 11        clojure.set)"
         "     .../clojure-1.9.0.jar@clojure/zip.clj 13   and enumeration.  See Huet"
         "     .../clojure-1.9.0.jar@clojure/zip.clj:14        :author \"Rich Hickey\"}"
         "     .../clojure-1.9.0.jar@clojure/zip.clj 15   clojure.zip"
         "     ..."
         "    (with the matched string highlighted)"
         ""
         "  5. find all files where name matches pattern and dump them on stdout, "
         "     search both directly on disk and within jar files:"
         ""
         "     ~> findjar clojure/1.9.0 -n \"MANIFEST.MF\" -c"
         ""
         "     <<<<<<< clojure-1.9.0.jar@META-INF/MANIFEST.MF"
         "     1 Manifest-Version: 1.0"
         "     2 Archiver-Version: Plexus Archiver"
         "     3 Created-By: Apache Maven"
         "     4 Built-By: jenkins"
         "     5 Build-Jdk: 1.7.0"
         "     6 Main-Class: clojure.main"
         "     7"
         "     >>>>>>>"
         ""
         "  6. find all files where content matches pattern and dump them on stdout, "
         "     intra-line highlight the matching content lines, search both directly "
         "     on disk and within jar files:"
         ""
         "     ~> findjar clojure/1.9.0 -g \"Plexus\" -c"
         ""
         "     <<<<<<< clojure-1.9.0.jar@META-INF/MANIFEST.MF"
         "     1 Manifest-Version: 1.0"
         "     2 Archiver-Version: Plexus Archiver"
         "     3 Created-By: Apache Maven"
         "     4 Built-By: jenkins"
         "     5 Build-Jdk: 1.7.0"
         "     6 Main-Class: clojure.main"
         "     7"
         "     >>>>>>>"
         "    (with the word \"Plexus\" on line 2 highlighted)"
         ""
         "  7. find all files both file name and content matches pattern and dump, "
         "     them on stdout:"
         ""
         "     ~> findjar clojure/1.9.0 -n \"properties\" -g \"groupId\" -c"
         ""
         "     <<<<<<< clojure-1.9.0.jar@META-INF/...clojure/pom.properties"
         "     1 #Generated by Maven"
         "     2 #Fri Dec 08 08:01:54 CST 2017"
         "     3 version=1.9.0"
         "     4 groupId=org.clojure"
         "     5 artifactId=clojure"
         "     >>>>>>>"
         "    (with the word \"groupId\" on line 4 highlighted)"
         ""
         "  8. find all files where name matches pattern and calculate both "
         "     an sha1 and an md5 hash them:"
         ""
         "     ~> findjar clojure/1.9.0 -n \"clj\" -s sha1 -s md5"
         ""
         "     ...94a86681b58d556f1eb13a clojure-1.9.0.jar@clojure/string.clj"
         "     ...f05f65fa44628a95497032 clojure-1.9.0.jar@clojure/set.clj"
         "     ...7444756fa91b65 clojure-1.9.0.jar@clojure/string.clj"
         "     ...95aaa8c6e9af74 clojure-1.9.0.jar@clojure/set.clj"
         ""]
        lines
        ;(map #(wrap-line MAX_WIDTH %) lines)
        (str/join \newline lines)))

(defn error-msg [errors summary]
  (str (usage summary)
       "\n"
       "ERROR" (when (> (count errors) 1) "S") ":\n\n"
       (str "  " (str/join (str \newline "  ") errors) \newline)))

(defn valid-search-root? [arguments]
  (and (= 1 (count arguments))
       (.exists (file (first arguments)))))

(defn summarize
  "Reduce options specs into a options summary for printing at a terminal.
  Note that the specs argument should be the compiled version. That effectively
  means that you shouldn't call summarize directly. When you call parse-opts
  you get back a :summary key which is the result of calling summarize (or
  your user-supplied :summary-fn option) on the compiled option specs."
  [specs]
  (if (seq specs)
    (let [show-defaults? false                              ;(some #(and (:required %) (contains? % :default)) specs)
          parts          (map (partial cli/make-summary-part show-defaults?) specs)
          lens           (apply map (fn [& cols] (apply max (map count cols))) parts)
          lines          (cli/format-lines lens parts)]
      (str/join \newline lines))
    ""))


(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [parsed (cli/parse-opts args (cli-options)
                               :strict true
                               :summary-fn summarize)
        {:keys [options arguments errors summary]} parsed
        fail   (fn [msg] {:exit-message (error-msg [msg] summary)})]
    (cond
      (:examples options) {:exit-message (examples) :ok? true} ; examples => exit OK with examples
      (:help options) {:exit-message (usage summary) :ok? true} ; help => exit OK with usage summary
      (and (:out-file options)
           (:grep options)) (fail "can not use out-file (-o) and grep (-g) together")
      (and (:apath options)
           (:path options)) (fail "can not use path (-p) and apath (-a) together")
      errors {:exit-message (error-msg errors summary)}     ; errors => exit with description of errors
      (valid-search-root? arguments) {:search-root (file (first arguments))
                                      :opts        options}
      :else (fail "invalid search-root"))))                 ; failed custom validation => exit with usage summary

(defn exit [status msg]
  (println msg)
  (System/exit status))

(comment
 ;; print opts in repl
 (cli/parse-opts ["-h"]
                 (cli-options)
                 :strict true
                 :summary-fn summarize)

 ;; provide multiple hash algs
 (cli/parse-opts ["-s" "sha1" "-s" "md5"]
                 (cli-options)
                 :strict true
                 :summary-fn summarize)

 ;; parse a real set of opts
 (cli/parse-opts ["." "-n" ".clj" "-t" "d"]
                 (cli-options)
                 :strict true
                 :summary-fn summarize)

 )