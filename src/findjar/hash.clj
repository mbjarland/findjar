(ns findjar.hash
  (:import [java.security MessageDigest]
           [java.util Locale]
           [java.util.zip CRC32]))


(def  hex-chars ^bytes
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

(defn digest [algo]
  (fn [^bytes data]
    (-> (MessageDigest/getInstance algo)
        (.digest data)
        bytes->hex)))

(defn crc-32 ^String [^bytes data]
  (let [crc (CRC32.)]
    (.update crc data)
    (String/format Locale/US "%08x" (to-array [(.getValue crc)]))))

(def md5 (digest "MD5"))
(def sha-1 (digest "SHA-1"))
(def sha-256 (digest "SHA-256"))
(def sha-512 (digest "SHA-512"))
