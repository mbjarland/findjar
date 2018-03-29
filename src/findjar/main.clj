(ns findjar.main
  (:require [findjar.core :as c]
            [clojure.string :as str]
            [findjar.cli :as cli]
            [jansi-clj.core :as ansi]
            ;[taoensso.tufte :as tufte :refer [p profiled profile defnp]]
            )
  (:gen-class))

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

(defn highlight-matches [hit? line match-idxs color-fn]
  (if (not hit?)
    line
    (let [tokens (split-at-idxs line (mapcat vals match-idxs))]
      (first
        (reduce
          (fn [[a h?] token]
            [(str a (if h? (color-fn token) token)) (not h?)])
          ["" false]
          tokens)))))

(defn default-handler
  "returns an implementation of the FindJarHandler protocol. This moves all
  side-effecting things out of the rest of the code and into this single place.
  It also makes the rest of the code more testable and makes it possible for
  users of this code to modify the behavior by supplying their own handler"
  []
  (reify c/FindJarHandler
    (warn [_ file type msg ex opts]
      (println "WARN:" msg))

    (match [_ path]
      (println path))

    ;{:path s :line-nr n :max-line-n :hit? b :start-col ns :end-col ne}"
    (grep-match
      [_ max-line-# {:keys [path line-# hit? match-idxs line]} opts]
      (let [context?  (< 0 (or (:context opts) 0))
            display-# (inc line-#)
            max       (count (str max-line-#))
            len       (count (str display-#))
            pad       (str/join (repeat (inc (- max len)) \space))]
        (println (str (str/trim path)
                      ":"
                      (if (and hit? context?) (ansi/red display-#) display-#)
                      pad
                      (highlight-matches hit? line match-idxs ansi/red)))))
    

    (dump-stream [_ path stream-factory opts]
      (c/default-dump-stream stream-factory path opts))     ;;TODO: switch arg order

    (print-hash [_ path hash-type hash-value opts]
      (println hash-value path))))

(defn -main [& args]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        handler (default-handler)]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (c/perform-file-scan search-root handler opts))))

(defn repl-main [& args]
  (let [{:keys [search-root opts exit-message ok?]} (cli/validate-args args)
        handler (default-handler)]
    (prn :opts opts)
    (if exit-message
      (println "would exit with code " (if ok? 0 1) "msg," exit-message)
      (c/perform-file-scan search-root handler opts))))

(comment

  (repl-main "/home/mbjarland/projects/kpna/packages/ATG10.2/"
             "-n" "GLOBAL.properties"
             "-g" "logging")

  (repl-main "."
             "-n" "test.txt"
             "-g" "222")

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

  )
