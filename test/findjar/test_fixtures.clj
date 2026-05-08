(ns findjar.test-fixtures
  "Builds a deterministic fixture tree under a temp directory:

      <root>/
         alpha.txt          (\"hello\\nworld\\nclojure rocks\\n\")
         beta.clj           (clojure source containing 'Rich Hickey')
         empty.txt          (zero bytes)
         nested/
            gamma.txt       (\"nested file\\n\")
         lib.jar            -> contains:
                              clojure/string.clj  (with 'Rich Hickey')
                              META-INF/MANIFEST.MF
         data.zip           -> contains:
                              data/numbers.txt    (1..5 lines)
         empty.jar          (zero-byte file with .jar extension)

  All paths use forward slashes inside archives."
  (:require [clojure.java.io :as jio])
  (:import [java.io ByteArrayOutputStream File]
           [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.util.zip ZipEntry ZipOutputStream]))

(defn- write-text! [^File f ^String content]
  (jio/make-parents f)
  (spit f content))

(defn- write-zip! [^File f entries]
  (jio/make-parents f)
  (with-open [zos (ZipOutputStream. (jio/output-stream f))]
    (doseq [[name content] entries]
      (.putNextEntry zos (ZipEntry. name))
      (let [bytes (cond
                    (bytes? content) content
                    (string? content) (.getBytes ^String content "UTF-8"))]
        (.write zos ^bytes bytes 0 (alength ^bytes bytes)))
      (.closeEntry zos))))

(def ^:private alpha-content "hello\nworld\nclojure rocks\n")
(def ^:private beta-content
  (str "(ns example\n"
       "  ^{:author \"Rich Hickey\"})\n"
       "(defn hello [] :ok)\n"))
(def ^:private clj-string-source
  (str ";; pretend clojure/string.clj\n"
       "(ns ^{:author \"Rich Hickey\"} clojure.string)\n"
       "(defn upper [s] (.toUpperCase s))\n"))
(def ^:private manifest-mf
  "Manifest-Version: 1.0\nCreated-By: test\nMain-Class: example.Main\n")
(def ^:private numbers-txt "1\n2\n3\n4\n5\n")

(defn build-fixture-root
  "Create a fresh fixture tree under a unique temp directory. Returns the
  root File. Caller is responsible for cleanup (or just leave it for the OS
  to GC the system temp dir)."
  ^File []
  (let [tmp ^Path (Files/createTempDirectory
                    "findjar-fixture-"
                    (into-array FileAttribute []))
        root (.toFile tmp)]
    (write-text! (jio/file root "alpha.txt") alpha-content)
    (write-text! (jio/file root "beta.clj") beta-content)
    (write-text! (jio/file root "empty.txt") "")
    (write-text! (jio/file root "nested" "gamma.txt") "nested file\n")
    (write-zip! (jio/file root "lib.jar")
                [["clojure/string.clj" clj-string-source]
                 ["META-INF/MANIFEST.MF" manifest-mf]])
    (write-zip! (jio/file root "data.zip")
                [["data/numbers.txt" numbers-txt]])
    (jio/make-parents (jio/file root "empty.jar"))
    (.createNewFile (jio/file root "empty.jar"))
    root))

(defn delete-recursively! [^File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-recursively! c)))
  (.delete f))
