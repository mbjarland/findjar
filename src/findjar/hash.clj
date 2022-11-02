(ns findjar.hash
  (:import [java.io InputStream]
           [java.security DigestInputStream MessageDigest]
           [java.util Locale]
           [java.util.zip CRC32]))


(def hex-chars ^bytes
  (byte-array (.getBytes "0123456789abcdef" "UTF-8")))

(defn bytes->hex
  "Convert Byte Array to Hex String"
  ^String
  [^bytes data]
  (let [len           (alength data)
        ^bytes buffer (byte-array (* 2 len))]
    (loop [i 0]
      (when (< i len)
        (let [b (aget data i)]
          (aset buffer (* 2 i) (aget hex-chars (bit-shift-right (bit-and b 0xF0) 4)))
          (aset buffer (inc (* 2 i)) (aget hex-chars (bit-and b 0x0F))))
        (recur (inc i))))
    (String. buffer "UTF-8")))

; TODO: test with BufferedInputStream(is, buf-size) for
; performance
(defn read-is
  "reads inputstream using a fixed byte buffer. Calls
  (update-fn byte-buffer n-bytes-read) for each buffered read.
  Returns total bytes read."
  ^long
  [^InputStream stream buf-size update-fn]
  (let [buf (byte-array buf-size)]
    (loop [total-len 0]
      (let [n (.read stream buf)]
        (if (pos? n)
          (do (when update-fn (update-fn buf n))
              (recur (+ total-len n)))
          total-len)))))

(defn digest [^String algo ^InputStream stream]
  (let [digest (MessageDigest/getInstance algo)]
    (with-open [dis (DigestInputStream. stream digest)]
      (read-is dis 256 nil))
    (bytes->hex (.digest digest))))

(defn crc-32 ^String [^InputStream stream]
  (let [crc (CRC32.)]
    (read-is stream 256 (fn [^bytes buf ^long n] (.update crc buf 0 n)))
    (String/format Locale/US "%08x" (to-array [(.getValue crc)]))))

