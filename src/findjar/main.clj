(ns findjar.main
  (:require [findjar.core :as c]
            [clojure.string :as str]
            [findjar.cli :as cli])
  (:gen-class))


(defn default-handler
  "returns an implementation of the FindJarHandler protocol. This moves all
  side-effecting things out of the rest of the code and into this single place.
  It also makes the rest of the code more testable and makes it possible for
  users of this code to modify the behavior by supplying their own handler"
  []
  (reify c/FindJarHandler
    (warn [_ file type msg opts]
      (println "WARN: can not read file -" (.getPath file)))

    (match [_ display-name]
      (println display-name))

    (grep-match [_ display-name line-nr hit? line opts]
      (println (str (str/trim display-name)
                    ":" (inc line-nr) (if hit? ">" ":")
                    (str/trim line))))

    (dump-stream [_ display-name stream-factory opts]
      (c/default-dump-stream stream-factory display-name opts)) ;;TODO: switch arg order

    (print-hash [_ display-name hash-type hash-value opts]
      (println hash-value display-name))))

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
    (if (or exit-message true)
      (println "would exit with code " (if ok? 0 1) "msg," exit-message)
      (c/perform-file-scan search-root handler opts))))

(comment

  (repl-main "/home/mbjarland/projects/kpna/packages/ATG10.2/"
             "-n" "GLOBAL.properties"
             "-g" "logging")

  )
