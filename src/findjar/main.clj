(ns findjar.main
  (:require [findjar.core :as c]
            [clojure.string :as str]
            [findjar.cli :as cli]
            [jansi-clj.auto :as ansi]
            [taoensso.tufte :refer [profile]]
            [clojure.java.io :as jio])
  (:gen-class)
  (:import (java.io LineNumberReader File BufferedWriter)))

(defn split-at-idxs [str idxs]
  (let [[r l _] (reduce
                 (fn [[a s i] idx]
                   (let [[t r] (split-at (- idx i) s)]
                     [(conj a (str/join t))
                      (str/join r)
                      idx]))
                 [[] str 0]
                 idxs)]
    (conj r l)))

(defn highlight-matches [hit? line match-idxs hit-color-fn]
  (if (not hit?)
    line
    (let [tokens (split-at-idxs line (mapcat vals match-idxs))]
      (first
       (reduce
        (fn [[a h?] token]
          [(str a (if h? (hit-color-fn token) token)) (not h?)])
        ["" false]
        tokens)))))

(defn stream-line-count [content-provider]
  (content-provider
   nil
   (fn [reader]
     (with-open [r (LineNumberReader. reader)]
       (.skip r Long/MAX_VALUE)
       (.getLineNumber r)))))

(defn style [color-fn])
(defn content-string [content-provider path to-file?]
  (content-provider
   nil
   (fn [reader]
     (with-out-str
       ;(when to-file? (jansi-clj.core/disable!))

       (println (ansi/red "<<<<<<<") path)
       (let [max-n-len (count (str (stream-line-count content-provider)))
             lines     (line-seq reader)]
         (doseq [[n line] (map-indexed vector lines)]
           (let [prefix (if to-file?
                          ""
                          (let [n-len (count (str (inc n)))
                                p     (str/join (repeat (- max-n-len n-len) \space))]
                            (str p (inc n))))]
             (println (str (ansi/green prefix) (str/trim line))))))
       (println (ansi/red ">>>>>>>"))

       ;(when to-file? (jansi-clj.core/enable!))
       ))))


(defn default-handler
  "returns an implementation of the FindJarHandler protocol. This moves all
  side-effecting things out of the rest of the code and into this single place.
  It also makes the rest of the code more testable and makes it possible for
  users of this code to modify the behavior by supplying their own handler"
  []
  (reify c/FindJarHandler
    (warn [_ msg ex]
      (println (ansi/red (str "WARN:" msg))))

    (match [_ path]
      (println path))

    ;{:path 'path', :line-# 1,  :hit? true,  :line '2222', :match-idxs [{:start 0, :end 2} {:start 2, :end 4}]}
    (grep-match
      [_ max-line-# {:keys [path line-# hit? line match-idxs]} opts]
      (let [context?  (< 0 (or (:context opts) 0))
            display-# (inc line-#)
            max       (count (str max-line-#))
            len       (count (str display-#))
            pad       (str/join (repeat (inc (- max len)) \space))]
        (println (str (str/trim path)
                      (if (and (not hit?) context?) " " ":")
                      display-#
                      pad
                      (highlight-matches hit? line match-idxs ansi/red)))))

    (dump-stream
      [_ path content-provider opts]
      (let [^File of (:out-file opts)
            to-file? (not (nil? of))
            str      (content-string content-provider path to-file?)]
        (when str                                           ;str is nil if there was an issue reading file
          (if to-file?
            (with-open [w (jio/writer of :append true)]
              (.write w str)
              (println path ">>" (.getPath of)))
            (print str)))))

    (print-hash [_ path hash-type hash-value opts]
      (println hash-value path))))

(taoensso.tufte/add-basic-println-handler! {})

(defn main-handler [do-exit? [& args]]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        handler  (default-handler)
        profile? (:profile opts)]
    (when profile? (prn :do-exit? do-exit? :opts opts))
    (if exit-message
      (if do-exit?
        (cli/exit (if ok? 0 1) exit-message)
        (println "would exit with code " (if ok? 0 1) "msg," exit-message))
      (profile {:when profile?} (c/perform-scan search-root handler opts)))))

(defn -main [& args]
  (main-handler true args))

(defn repl-main [& args]
  (main-handler false args))

(comment
 "" ""

 (repl-main "/Users/mbjarland/projects/kpna/packages/ATG10.2/"
            "-n" "GLOBAL.properties"
            ;"-s" "sha1"
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
