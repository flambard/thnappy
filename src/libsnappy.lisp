(in-package :thnappy)

(define-foreign-library libsnappy
  (t (:default "libsnappy")))

(use-foreign-library libsnappy)


(defcenum snappy-status
  (:ok 0)
  (:invalid-input 1)
  (:buffer-too-small 2))

;;; size_t = unsigned int?
(defctype size-t :unsigned-int)

(defcfun "snappy_compress" snappy-status
  (input (:pointer :char))
  (input-length size-t)
  (compressed (:pointer :char))
  (compressed-length (:pointer size-t)))

(defcfun "snappy_uncompress" snappy-status
  (compressed (:pointer :char))
  (compressed-length size-t)
  (uncompressed (:pointer :char))
  (uncompressed-length (:pointer size-t)))

(defcfun "snappy_max_compressed_length" size-t
  (source-length size-t))

(defcfun "snappy_uncompressed_length" snappy-status
  (compressed (:pointer :char))
  (compressed-length size-t)
  (result (:pointer size-t)))

(defcfun "snappy_validate_compressed_buffer" snappy-status
  (compressed (:pointer :char))
  (compressed-length size-t))
