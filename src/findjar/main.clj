(ns findjar.main
  (:require [findjar.core :as c]
            [clojure.string :as str]
            [findjar.cli :as cli]
            [jansi-clj.core :as ansi])
  (:gen-class))


(defn highlight-match [line start end color]
  (if (or (nil? start) (nil? end))
    line
    (let [[h m t] (map str/join [(take start line)
                                 (take (- end start) (drop start line))
                                 (drop end line)])]
      (str/trim (str h (color m) t)))))

(defn default-handler
  "returns an implementation of the FindJarHandler protocol. This moves all
  side-effecting things out of the rest of the code and into this single place.
  It also makes the rest of the code more testable and makes it possible for
  users of this code to modify the behavior by supplying their own handler"
  []
  (reify c/FindJarHandler
    (warn [_ file type msg e opts]
      (.printStackTrace e)
      (println "WARN:" msg))

    (match [_ path]
      (println path))

    ;{:path s :line-nr n :max-line-n :hit? b :start-col ns :end-col ne}"
    (grep-match
      [_ max-line-# {:keys [path line-# hit? start-col end-col line]} opts]
      (let [context?  (< 0 (or (:context opts) 0))
            display-# (inc line-#)
            max       (count (str max-line-#))
            len       (count (str display-#))
            pad       (str/join (repeat (inc (- max len)) \space))]
        (println (str (str/trim path)
                      ":"
                      display-#
                      (if context?
                        (if hit? ":" " ")
                        (if hit? ":" " "))
                      pad
                      (highlight-match line start-col end-col ansi/red)))))

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

  ;; parse a real set of opts
  (cli/parse-opts ["." "-n" ".clj" "-t" "d"]
                  (cli-options)
                  :strict true
                  :summary-fn summarize)

  )
