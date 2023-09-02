(ns findjar.protocols)

(defprotocol FileContent
  "a protocol definition to represent file content from either normal disk files
  or files within zip/jar archives. As we use the protocol FindJarOutput to push
  the output to stdout to the edges of the system and stdout out of this namespace,
  we similarly need an abstraction for retrieving the contents of a file to be
  able to hash/display lines/dump etc file contents.

  This protocol allows us to create a 'content provider' which implements this
  protocol and assuming you get handed a content provider (file-content in this
  case) to a specific file, it allows you to do the following:

    (as-stream file-content #(do-something-with-file-content-stream %))
    (as-reader file-content #(do-something-with-file-content-reader %))

  where the stream handed to the anonymous function is an InputStream to the
  file contents (either on disk or inside a zip/jar archive) and the reader
  is similar but for receiving the content in text format."
  (as-stream [this stream-handler]
    "called when you want to read the file contents as a binary stream.
    stream-handler is a one argument function which will receive an open InputStream
    to the file contents as the argument. It is not the responsibility of the
    stream-handler to close the stream.")
  (as-reader [this reader-handler]
    "called when you want to read the file contents as a reader of text data.
    reader-handler is a one argument function which will receive an open java.io.Reader
    to the file contents as the argument. It is not the responsibility of the
    stream-handler to close the reader."))

(defprotocol FindJarOutput
  "a protocol definition to move all side effecting things (mainly io to stdout)
  to the edges of the findjar code. We send in an output handler implementation
  into the findjar entry point and all side effecting things are then handled by
  the output handler leaving the code in this namespace less polluted by
  side effects."
  (warn [this msg ex opts]
    "called on errors during file scan. First argument is a java
    file object, second a keyword indicating error type, third a
    message describing the issue, and fourth the full set of options
    used to start the file scan")
  (match [this path opts]
    "called when a normal (as in a non-grep) file name/path match
    is encountered")
  (grep-match [this max-line-# line-map opts]
    "called when a matching line is found in the content of a file
    (as opposed to matching only on file name/path). See docstring for
    'match' for description of path. The second argument
    is the max line number which will need to be displayed in
    the containing file (for padding the output), the third argument
    is a a map on the format:

    {:path p :line-# n :hit? true :line data
     :match-idxs [{:start 14, :end 16}
                  {:start 16, :end 18}
                  {:start 18, :end 20}]}]

    where the match-idxs is only present if the line is a hit.

    The fourth argument is the full set of options used to start the
    file scan")
  (dump-stream [this path file-content opts]
    "called to dump the entire contents of a file (when using the -c
    option) to some target location as specified by the opts. By default
    this will be either to stdout or to a target file (with the -o option),
    but protocol implementations can choose to do something else. See
    the docstring for 'match' for a description of the first argument. The
    second argument is a no-arguments function which will return an input
    stream to the matching file when called, the third argument is
    the full set of options used to start the file scan")
  (print-hash [this path hash-type hash-value opts]
    "called to calculate and output a hash for a file. See the docstring
    for 'match' for a description of the first argument. See docstring
    for 'dump-stream' for a description of the second argument. The third
    argument is a map where the keys are keywords indicating the user
    requested hash operations (:md5 :sha1) and the values are the
    meta-data for these hash operations. The last argument is a the full
    set of options used to start the file scan"))

