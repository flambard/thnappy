(defpackage :thnappy
  (:documentation
   "Common Lisp bindings to Google's Snappy compression library.")
  (:nicknames :snappy)
  (:use :cl :cffi)
  (:export

   ;; High level functions
   :compress
   :uncompress
   :valid-compressed-p

   ;; Low level functions
   :compress-string
   :compress-byte-vector
   :uncompress-byte-vector
   :valid-compressed-byte-vector-p

   ;; Conditions
   :invalid-input
   :buffer-too-small

   ))
