(ns build
  (:require [clojure.tools.build.api :as b]))


(def lib 'mbjarland/findjar)
(def version "1.0.4")                                       ;(format "1.0.4.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn gen-version-file [_]
  (let [hash      (b/git-process {:git-args ["rev-parse" "HEAD"]})
        short     (apply str (take 7 hash))
        rev-count (b/git-count-revs nil)]
    (b/write-file {:path    "gen-resources/build/version.edn"
                   :content {:ref       hash
                             :ref-short short
                             :version   version
                             :rev-count rev-count
                             :dirty?    true}})))

(b/git-count-revs nil)


(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis     basis
                  :src-dirs  ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis     basis
           :main      'findjar.main}))