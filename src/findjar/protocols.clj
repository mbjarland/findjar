(ns findjar.protocols)

(defprotocol FindJarOutput
  "Sink for everything findjar wants to surface to the user. Implementations
  can render to stdout (default), capture into a buffer (for parallel scans),
  or collect calls in tests."
  (warn [this msg ex opts]
    "Called when a file or archive can't be read. msg is a human-readable
    description, ex is the underlying Throwable (may be nil), opts is the
    full parsed-CLI map. Implementations should not throw — the caller will
    continue with the next file.")
  (match [this path opts]
    "Called for a file/path match (no -g, -c, -s).")
  (grep-match [this max-line-# line-map opts]
    "Called for a single grep result line. line-map is:
       {:path p :line-# n :hit? bool :line s
        :match-idxs [{:start i :end j} ...]   ;; only when :hit? true
       }
     max-line-# is the largest :line-# in this file's match set, used for
     padding column widths.")
  (grep-count [this path n opts]
    "Called for --count. n is the number of matching lines in the file.
     Suppresses any grep-match calls for the same file.")
  (class-info [this path info opts]
    "Called for --class-info on a .class entry. info is:
       {:name 'java/lang/String'
        :super 'java/lang/Object'
        :interfaces ['java/io/Serializable' ...]
        :access #{:public :final ...}
        :methods [{:name '<init>' :desc '()V' :access #{:public}} ...]}")
  (dump-stream [this path materialized opts]
    "Called for -c (cat). materialized is the already-rendered string
     (including ANSI / line-number formatting) ready to be emitted. The
     stream is read once at call time so implementations can buffer freely.")
  (print-hash [this path hash-type hash-value opts]
    "Called for -s. hash-type is the algorithm keyword (:md5 :sha1 ...);
     hash-value is the hex/CRC string."))
