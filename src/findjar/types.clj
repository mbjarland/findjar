(ns findjar.types
  (:require [clojure.java.io :as jio])
  (:import (java.io File)))

(defn file-ext [^File f]
  (let [p (.getCanonicalPath f)
        i (.lastIndexOf p ".")]
    (if (and (pos? i) (not (= (inc i) (count p))))
      (subs p (inc i))
      nil)))

(defn valid-file? [^File f opts handler]                    ;;TODO: use multimethod file type
  (let [active-types (:enabled-file-types opts)
        ext          (.toLowerCase (file-ext f))
        warn         (:warning handler)]
    (and
      (.isFile f)
      (or (.canRead f)
          (do (warn f :can-not-read "WARN: can not read file" opts)
              false))
      (get active-types ext))))

(defmulti file-type-scanner
          (fn [^File f] (file-ext f)))

(defmethod file-type-scanner "jar"
  [^File f]
  {:fn      (fn [display-name opts handler]
              (find-in-jar f display-name opts handler))
   :desc    "files in jar files"
   :default true
   :char    \j})

(defmethod file-type-scanner "zip"
  [^File f]
  {:fn      (fn [display-name opts handler]
              (find-in-jar f display-name opts handler))
   :desc    "files in zip files"
   :default false
   :char    \z})

(defmethod file-type-scanner :default
  [^File f]
  {:fn      (fn [display-name opts handler]
              (let [file-name      (.getName f)
                    stream-factory #(jio/input-stream f)]
                (print-stream-matches opts file-name display-name stream-factory handler)))
   :desc    "files on disk"
   :default true
   :char    \d})

