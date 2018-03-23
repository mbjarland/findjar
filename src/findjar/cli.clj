(ns findjar.cli
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :refer [file]])
  (:gen-class))

(def MAX_WIDTH 80)

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

(defn reformat-options [max-width opts]
  "reformat the command line params for a clean output when printing usage"
  (let [max-long-desc  (apply max (map (comp count second) opts))
        margin         (apply str (repeat (+ 2 3 1 max-long-desc 2) " "))
        max-desc-width (- max-width (+ max-long-desc 8))]
    (wrap-opts max-desc-width margin opts)))

(defn parse-type [type]
  (if (= 1 (count type))
    (->> type
         (.toLowerCase)
         (first))
    (throw (proxy [Exception] []                            ; override toString to clean up error display
             (toString [] (str "type has to be either j (Jar) or f (disk File)"))))))

(def cli-options
  (->> [;; First three strings describe a short-option, long-option with optional
        ;; example argument description, and a description. All three are optional
        ;; and positional.
        ["-n" "--name <regex>"
         "a pattern to match against file names"
         :parse-fn #(re-pattern %)]
        ["-g" "--grep <regex>"
         "a pattern to match against file content lines"
         :parse-fn #(re-pattern %)]
        ["-t" "--type <j|d>"
         "restrict the files searched to only jar files or only disk
         files.
          Note that the default behavior is to search both plain files on
          disk and files inside jar files"
         :parse-fn parse-type
         :validate [#{\j \f} "type must be one of 'j' and 'f'"]]
        ["-p" "--path <regex>"
         "a pattern to match against the relative path starting from search-root"
         :parse-fn #(re-pattern %)]
        ["-a" "--apath <regex>"
         "a pattern to match against the absolute path"
         :parse-fn #(re-pattern %)]
        ["-x" "--context <# of lines>"
         "If -c is given, show <# of lines> lines of context around the match, defaults to 0"
         :parse-fn #(Integer/parseInt %)]
        ["-o" "--out-file <path>"
         "when using -c (cat file), write the contents of the located file(s) to the output file"
         :parse-fn #(clojure.java.io/as-file %)]
        ["-f" "--flags <flags>"
         "turns on regex flags for all matches used. Example: -f i turns on case insensitive matching for both file names and content. See oracle javadocs on
          Pattern.html#special for details on java regex flags"]
        ["-c" "--cat" "cat file. For matching files, print the entire file contents on the console"]
        ["-m" "--md5" "print md5 hash of matched files"]
        ["-s" "--sha1" "print sha1 hash of matched files"]
        ["-h" "--help" "show usage information"]]
       (reformat-options MAX_WIDTH)))

(defn usage [summary]
  (as-> [""
         "findjar - a tool for searching through files, including files inside jars"
         ""
         "usage: findjar <search-root> [-n <name-pattern>] [-g <content-pattern>]  [...]"
         ""
         "This script searches for files in any disk structure. It is capable of
          looking inside jar/zip files for file names and also capable of looking for
          file content inside the files which reside in the jar/zip files."
         ""
         "This tool is in essence an improvement of the unix find command
          geared towards solving a common problem for programmers on the JVM:
          finding that specific file or class in your maven repo, classpath, etc
          when that file resides inside a jar archive."
         ""
         "Note that this tool is capable of a few extra tricks such as writing
          out the contents of matched files inside jar files and calculating
          md5 or sha1 hashes of matched files inside jar files."
         ""
         "Command line switches can be provided either using short form i.e. '-t j'
          or long form i.e. '--type j'."
         ""
         "Options:"] lines
        (map un-whitespace lines)
        (map #(wrap-line MAX_WIDTH %) lines)
        (str/join \newline lines)
        (str lines "\n" summary "\n")))

(defn error-msg [errors summary]
  (str (usage summary)
       "\n"
       "ERROR" (when (> (count errors) 1) "S") ":\n\n"
       (str "  " (str/join (str \newline "  ") errors) \newline)))

(defn valid-search-root? [arguments]
  (and (= 1 (count arguments))
       (.exists (file (first arguments)))))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options :strict true)
        fail (fn [msg] {:exit-message (error-msg [msg] summary)})]
    (cond
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
