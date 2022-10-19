(ns findjar.main
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [findjar.cli :as cli]
            [findjar.core :as c]
            [jansi-clj.auto]
            [jansi-clj.core :refer [green red]]
            [taoensso.tufte :refer [profile]])
  (:import [java.io File LineNumberReader])
  (:gen-class))

(defn split-at-idxs
  "takes a string and a seq of indicies and splits
  the string at those indicies"
  [str idxs]
  (let [[r l _] (reduce
                  (fn [[a s i] idx]
                    (let [[t r] (split-at (- idx i) s)]
                      [(conj a (str/join t))
                       (str/join r)
                       idx]))
                  [[] str 0]
                  idxs)]
    (conj r l)))

; a global
(def ^:dynamic *use-colors* true)

(defn use-colors?
  [opts]
  (not (:monochrome opts)))

(defn style
  "utility function to optionally ansi color output, first
  argument is a jansi-clj color function such as ansi/red"
  [color-fn str]
  (if *use-colors* (color-fn str) str))

(defn highlight-matches
  "ansi color highlight intra-line matches in content, first argument
  is whether this line contains a hit or not, second the line of content,
  third the match indicies, fourth the ansi color function and the last one
  the options sent into this program. Returns colored string"
  [hit? line match-idxs hit-color-fn opts]
  (if (not hit?)
    line
    (let [tokens (split-at-idxs line (mapcat vals match-idxs))]
      (first
        (reduce
          (fn [[a h?] token]
            [(str a (if h? (style hit-color-fn token) token)) (not h?)])
          ["" false]
          tokens)))))

(defn content-line-count
  "returns number of lines in a content item"
  [content-provider]
  (content-provider
    nil
    (fn [reader]
      (with-open [r (LineNumberReader. reader)]
        (.skip r Long/MAX_VALUE)
        (.getLineNumber r)))))

(defn content-string
  "creates a string 'dump' of a content item, first arg is a 'content provider'
  capable of providing a reader or stream to the content, second is a file path
  (possily within a jar file), third is a boolean indicating whether the output
  of this function is intended to be written to a file (no ansi coloring to files)
  and the last argument is the map of options sent into this program"
  [content-provider path to-file? opts]
  (content-provider
    nil
    (fn [reader]
      (with-out-str
        (binding [*use-colors* (and (not to-file?) (use-colors? opts))]
                 (println (style red "<<<<<<<") path)
                 (let [max-n-len (count (str (content-line-count content-provider)))
                       lines     (line-seq reader)
                       grep      (:grep opts)]
                   (doseq [[n line] (map-indexed vector lines)]
                     (let [match-idxs (when grep (c/match-idxs grep line))
                           line       (if match-idxs (highlight-matches true line match-idxs red opts) line)
                           prefix     (if to-file?
                                        ""
                                        (let [n-len (count (str (inc n)))
                                              p     (str/join (repeat (- max-n-len n-len) \space))]
                                          (str p (inc n) " ")))]
                       (println (str (style green prefix) line)))))
                 (println (style red ">>>>>>>")))))))

(defn default-handler
  "returns an implementation of the FindJarHandler protocol. This moves all
  side-effecting things out of the rest of the code and into this single place.
  It also makes the rest of the code more testable and makes it possible for
  users of this code to modify the behavior by supplying their own handler"
  []
  (reify c/FindJarHandler
    (warn [_ msg ex opts]
      (binding [*use-colors* (use-colors? opts)]
               ;(.printStackTrace ex)
               (println (style red (str "WARN:" msg)))))

    (match [_ path opts]
      (println path))

    ;{:path 'path', :line-# 1,  :hit? true,  :line '2222', :match-idxs [{:start 0, :end 2} {:start 2, :end 4}]}
    (grep-match
      [_ max-line-# {:keys [path line-# hit? line match-idxs]} opts]
      (let [context?  (< 0 (or (:context opts) 0))
            display-# (inc line-#)
            max       (count (str max-line-#))
            len       (count (str display-#))
            pad       (str/join (repeat (inc (- max len)) \space))]
        (binding [*use-colors* (use-colors? opts)]
                 (println (str (str/trim path)
                               (if (and (not hit?) context?) " " ":")
                               display-#
                               pad
                               (highlight-matches hit? line match-idxs red opts))))))

    (dump-stream
      [_ path content-provider opts]
      (let [^File of (:out-file opts)
            to-file? (not (nil? of))
            str      (content-string content-provider path to-file? opts)]
        ;(binding [*out* *err*]
        ;  (prn :strlen (count str) :f path))
        (when str                                           ;str is nil if there was an issue reading file
          (if to-file?
            (with-open [w (jio/writer of :append true)]
              (.write w str)
              (println path ">>" (.getPath of)))
            (println str)))))

    (print-hash [_ path hash-type hash-value opts]
      (println hash-value path))))

(taoensso.tufte/add-basic-println-handler! {})

(defn main-entrypoint
  "the main entrypoint function for the findjar program. This function is used
  by both the -main and repl-main functions. First argument is whether or not
  command line errors should lead to a System/exit"
  [hard-exit-on-errors? [& args]]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        profile? (:profile opts)
        handler  (default-handler)]
    (when profile? (prn :do-exit? hard-exit-on-errors? :opts opts))
    (if exit-message
      (if hard-exit-on-errors?
        (cli/exit (if ok? 0 1) exit-message)
        (println "would exit with code " (if ok? 0 1) "msg," exit-message))
      (profile {:when profile? :nmax 10000000} (c/perform-scan search-root handler opts)))))

(defn -main [& args]
  (main-entrypoint true args))

(defn repl-main [& args]
  (main-entrypoint false args))

(comment
  "" ""

  ;; findjar . -p "ATGDBSetup.*ModuleManager.properties" -g '\{[^}]+\}' | grep -vE "Resource|Message"

  (repl-main "/home/mbjarland/projects/kpna/packages/ATG10.2/"
             "-p" "ATGDBSetup.*ModuleManager.properties"
             "-g" "[\\{]"
             "-t" "z"
             ;"--profile")
             )



  (repl-main "/Users/mbjarland/projects/kpna/packages/ATG10.2/"
             "-n" "GLOBAL.properties"
             "-g" "logging"
             "-t" "z"
             "--profile")

  (repl-main "/Users/mbjarland/projects/kpna/packages/ATG10.2/"
             "-n" "GLOBAL.properties"
             "-c"
             "--profile")


  (repl-main "."
             "-n" "main.clj"
             "-g" "main.clj")

  ;; parse a real set of opts
  (cli/parse-opts ["." "-n" ".clj" "-t" "d"]
                  (cli-options)
                  :strict true
                  :summary-fn summarize)

  ;; profile a run
  (profiled
    {}
    (repl-main
      "/home/mbjarland/projects/kpna/packages/ATG10.2/"
      "-t" "j"
      "-n" "xml$"
      "-g" "login"
      "-fi"
      "-x" "2"))

  (format-duration [duration]
                   (let [periods [:year 365 :day 24 :hour 60 :minute 60 :second 60]
                         total   (reduce (fn [a [_ p]] (* s p)) 1 periods)]
                     (if (< duration 1)
                       "now"
                       (let [cf     (fn [[d p cs] [pn pc]] [(mod d p) (conj cs (/ p pc))])
                             folder (fn [[s0 s1 r] p]
                                      (match p
                                             [_ 0] [s0 s1 r]
                                             [n 1] [s1 ", " (str "1 " n s0 r)]
                                             [n c] [s1 ", " (str c " " n s0 r)]))
                             [_ _ components] (reduce cf [duration total '()] periods)

                             [_ _ r] (reduce folder ["" " and " ""] components)]
                         r))))

  )
