(defpackage :thnappy
  (:documentation
   "Common Lisp bindings to Google's Snappy compression library.")
  (:use :cl :cffi)
  (:export

   :compress
   :uncompress
   :valid-compressed-p

   :compress-string
   :compress-byte-vector
   :uncompress-byte-vector
   :valid-compressed-byte-vector-p

   ))
