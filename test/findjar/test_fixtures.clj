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
         target/junk.txt    (default-excluded dir contents)
         .git/HEAD          (default-excluded dir contents)

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
    (write-text! (jio/file root "target" "junk.txt") "build artifact\n")
    (write-text! (jio/file root ".git" "HEAD") "ref: refs/heads/main\n")
    ;; A binary-looking file (NUL byte in first 8KB) for binary-skip tests.
    (let [^File f (jio/file root "binary.dat")]
      (jio/make-parents f)
      (with-open [out (jio/output-stream f)]
        (.write out (byte-array [(byte 0x48) (byte 0x00) (byte 0x65) (byte 0x6c)
                                  (byte 0x6c) (byte 0x6f)]))))
    ;; A nested jar (uberjar shape): outer.jar contains inner.jar which
    ;; contains a token file with a unique grep target.
    (let [inner-bytes (let [baos (java.io.ByteArrayOutputStream.)]
                        (with-open [zos (java.util.zip.ZipOutputStream. baos)]
                          (.putNextEntry zos
                            (java.util.zip.ZipEntry. "deep/token.txt"))
                          (let [b (.getBytes "NESTED-MARKER\n" "UTF-8")]
                            (.write zos b 0 (alength b)))
                          (.closeEntry zos))
                        (.toByteArray baos))]
      (write-zip! (jio/file root "outer.jar")
                  [["inner.jar" inner-bytes]
                   ["plain.txt" "outer plain content\n"]]))
    ;; A .gitignore at the root excluding 'ignored/' and '*.log', and
    ;; testing negation: !keep.log re-includes one specific log.
    (write-text! (jio/file root ".gitignore")
                 "# fixture gitignore\nignored/\n*.log\n!keep.log\n")
    (write-text! (jio/file root "ignored" "secret.clj") "should be ignored\n")
    (write-text! (jio/file root "trace.log") "log content\n")
    (write-text! (jio/file root "keep.log") "kept by negation rule\n")
    ;; Nested directory with its own .gitignore — patterns there only
    ;; apply to the subtree.
    (write-text! (jio/file root "sub-with-gi" "should-stay.txt") "stays\n")
    (write-text! (jio/file root "sub-with-gi" "local.skip") "subtree-only ignore\n")
    (write-text! (jio/file root "sub-with-gi" ".gitignore")
                 "*.skip\n")
    ;; A small tar.gz archive for tar tests. Header layout: 512-byte
    ;; ustar header + content (padded to 512), terminated by two zero
    ;; blocks.
    (let [tar-bytes
          (let [baos (java.io.ByteArrayOutputStream.)]
            (letfn [(pad [^java.io.ByteArrayOutputStream b len]
                      (let [n (mod len 512)]
                        (when (pos? n)
                          (.write b (byte-array (- 512 n))))))
                    (write-tar-entry [^java.io.ByteArrayOutputStream b name content]
                      (let [hdr (byte-array 512)
                            name-bs (.getBytes ^String name "UTF-8")
                            size (count content)
                            size-oct (format "%011o" size)
                            mode "0000644"
                            mtime (format "%011o" (long (/ (System/currentTimeMillis) 1000)))
                            magic "ustar"
                            put! (fn [^bytes h ^long off ^bytes src]
                                   (System/arraycopy src 0 h off (min (alength src) (- 512 off))))]
                        (put! hdr 0   name-bs)
                        (put! hdr 100 (.getBytes mode "UTF-8"))
                        (put! hdr 124 (.getBytes (str size-oct \space) "UTF-8"))
                        (put! hdr 136 (.getBytes (str mtime \space) "UTF-8"))
                        (put! hdr 156 (byte-array [(byte (int \0))]))
                        (put! hdr 257 (.getBytes magic "UTF-8"))
                        ;; checksum: spaces in field, then sum
                        (let [chk-area-init (byte-array 8 (byte 32))]
                          (put! hdr 148 chk-area-init))
                        (let [sum (reduce + (map #(bit-and % 0xFF) hdr))
                              chk-str (str (format "%06o" sum) "\0 ")]
                          (put! hdr 148 (.getBytes chk-str "UTF-8")))
                        (.write b hdr)
                        (.write b (.getBytes ^String content "UTF-8"))
                        (pad b size)))]
              (write-tar-entry baos "hello.txt"  "tar greeting\n")
              (write-tar-entry baos "deep/x.txt" "deep tar entry\n")
              ;; Two zero blocks → end of archive.
              (.write baos (byte-array 1024)))
            (.toByteArray baos))]
      (let [out (jio/file root "demo.tar")]
        (with-open [os (jio/output-stream out)] (.write os tar-bytes)))
      (let [out (jio/file root "demo.tar.gz")]
        (with-open [os (java.util.zip.GZIPOutputStream. (jio/output-stream out))]
          (.write os tar-bytes))))
    ;; A .gz "log file": access.log.gz containing a few lines for the
    ;; -t g (gzip log) type tests.
    (let [content "INFO 2026-05-12 ok\nERROR 2026-05-12 bad\nINFO 2026-05-12 done\n"
          out     (jio/file root "access.log.gz")]
      (jio/make-parents out)
      (with-open [os (java.util.zip.GZIPOutputStream. (jio/output-stream out))]
        (.write os (.getBytes content "UTF-8"))))
    ;; A real .class file for --class-info tests. Generate via ASM so
    ;; the bytes are guaranteed valid for the parser we use in core.
    (let [cw (org.objectweb.asm.ClassWriter. 0)]
      (.visit cw 52   ; Java 8 classfile major
              (bit-or 0x0001 0x0020) ; ACC_PUBLIC | ACC_SUPER
              "fixture/Greeter"
              nil
              "java/lang/Object"
              (into-array String ["java/io/Serializable"]))
      (let [mv (.visitMethod cw 0x0001 "<init>" "()V" nil nil)]
        (.visitCode mv)
        (.visitVarInsn mv org.objectweb.asm.Opcodes/ALOAD 0)
        (.visitMethodInsn mv org.objectweb.asm.Opcodes/INVOKESPECIAL
                          "java/lang/Object" "<init>" "()V" false)
        (.visitInsn mv org.objectweb.asm.Opcodes/RETURN)
        (.visitMaxs mv 1 1)
        (.visitEnd mv))
      (let [mv (.visitMethod cw 0x0009 "hello" "(I)Ljava/lang/String;" nil nil)]
        (.visitCode mv)
        (.visitInsn mv org.objectweb.asm.Opcodes/ACONST_NULL)
        (.visitInsn mv org.objectweb.asm.Opcodes/ARETURN)
        (.visitMaxs mv 1 1)
        (.visitEnd mv))
      (.visitEnd cw)
      (let [bytes (.toByteArray cw)
            f (jio/file root "Greeter.class")]
        (with-open [os (jio/output-stream f)] (.write os bytes))))
    root))

(defn delete-recursively! [^File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-recursively! c)))
  (.delete f))
