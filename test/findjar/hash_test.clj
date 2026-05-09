(ns findjar.hash-test
  (:require [clojure.test :refer [deftest is testing]]
            [findjar.hash :as h])
  (:import [java.io ByteArrayInputStream]))

(defn- bytes-of [^String s] (.getBytes s "UTF-8"))
(defn- stream-of [^String s] (ByteArrayInputStream. (bytes-of s)))

(deftest digest-known-values
  (testing "sha1 of empty string"
    (is (= "da39a3ee5e6b4b0d3255bfef95601890afd80709"
           (h/digest "SHA-1" (stream-of "")))))
  (testing "md5 of \"hello\\n\""
    (is (= "b1946ac92492d2347c6235b4d2611184"
           (h/digest "MD5" (stream-of "hello\n")))))
  (testing "sha256 of \"abc\""
    (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
           (h/digest "SHA-256" (stream-of "abc"))))))

(deftest crc32-known-value
  ;; CRC-32 of the ASCII string "123456789" is 0xCBF43926
  (is (= "cbf43926" (h/crc-32 (stream-of "123456789")))))

(deftest read-is-handles-empty
  (is (= 0 (h/read-is (stream-of "") 32 nil))))

(deftest read-is-counts-bytes
  (is (= 11 (h/read-is (stream-of "hello world") 4 nil))))

(deftest bytes-hex-roundtrip
  (is (= "00ff10" (h/bytes->hex (byte-array [0 -1 16])))))
