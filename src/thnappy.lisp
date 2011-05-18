(in-package :thnappy)

(defgeneric compress (bytes)
  (:documentation "Compresses BYTES and returns a vector."))

(defmethod compress ((bytes vector))
  (compress-byte-vector bytes))

(defmethod compress ((bytes string))
  (compress-string bytes))

(defmethod compress ((bytes sequence))
  (compress-byte-vector (coerce bytes '(vector (unsigned-byte 8)))))


(defgeneric uncompress (bytes &key to-string)
  (:documentation "Uncompresses BYTES and returns a vector."))

(defmethod uncompress :around (bytes &key to-string)
  (let ((uncompressed (call-next-method)))
    (if to-string
        (map 'string #'code-char uncompressed)
        uncompressed)))

(defmethod uncompress ((bytes vector) &key &allow-other-keys)
  (uncompress-byte-vector bytes))

(defmethod uncompress ((bytes string) &key &allow-other-keys)
  (uncompress-byte-vector (map '(vector (unsigned-byte 8)) #'char-code bytes)))

(defmethod uncompress ((bytes sequence) &key &allow-other-keys)
  (uncompress-byte-vector (coerce bytes '(vector (unsigned-byte 8)))))


(defgeneric valid-compressed-p (bytes)
  (:documentation "Returns true if BYTES is a valid for uncompression."))

(defmethod valid-compressed-p ((bytes vector))
  (valid-compressed-byte-vector-p bytes))

(defmethod valid-compressed-p ((bytes string))
  (valid-compressed-byte-vector-p
   (map '(vector (unsigned-byte 8)) #'char-code bytes)))

(defmethod valid-compressed-p ((bytes sequence))
  (valid-compressed-byte-vector-p (coerce bytes '(vector (unsigned-byte 8)))))


(defun compress-string (string)
  (with-foreign-pointer (compr (snappy-max-compressed-length (length string)))
    (with-foreign-object (compr-size 'size-t)
      (with-foreign-string (cstring string)
        (ecase (snappy-compress cstring (length string) compr compr-size)
          (:ok t)
          (:invalid-input (error "Invalid input!"))
          (:buffer-too-small (error "Buffer too small!")))
        (let* ((size (cffi:mem-ref compr-size 'size-t))
               (result (make-byte-vector size)))
          (mem-read-vector result compr :uchar size)
          result)))))

(defun compress-byte-vector (bytes)
  (let ((bytes-length (length bytes)))
    (with-foreign-pointer (compr (snappy-max-compressed-length bytes-length))
      (with-foreign-objects ((compr-size 'size-t)
                             (c-bytes :uchar bytes-length))
        (mem-write-vector bytes c-bytes :uchar)
        (ecase (snappy-compress c-bytes bytes-length compr compr-size)
          (:ok t)
          (:invalid-input (error "Invalid input!"))
          (:buffer-too-small (error "Buffer to small!")))
        (let* ((size (cffi:mem-ref compr-size 'size-t))
               (result (make-byte-vector size)))
          (mem-read-vector result compr :uchar size)
          result)) )))


(defun uncompress-byte-vector (compressed-bytes)
  (let ((byte-count (length compressed-bytes)))
    (with-foreign-objects ((uncompr-size 'size-t)
                           (c-compr-bytes :uchar byte-count))
      (mem-write-vector compressed-bytes c-compr-bytes :uchar)
      (ecase (snappy-uncompressed-length c-compr-bytes byte-count uncompr-size)
        (:ok t)
        (:invalid-input (error "Invalid input!"))
        (:buffer-too-small (error "Buffer to small!")))
      (with-foreign-pointer
          (uncompr (mem-ref uncompr-size 'size-t) uncompressed-size)
        (ecase (snappy-uncompress c-compr-bytes byte-count uncompr uncompr-size)
          (:ok t)
          (:invalid-input (error "Invalid input!"))
          (:buffer-too-small (error "Buffer to small!")))
        (let* ((size (cffi:mem-ref uncompr-size 'size-t))
               (result (make-byte-vector size)))
          (assert (= size uncompressed-size))
          (mem-read-vector result uncompr :uchar size)
          result)) )))


(defun valid-compressed-byte-vector-p (bytes)
  (with-foreign-pointer (compressed-bytes (length bytes) byte-count)
    (mem-write-vector bytes compressed-bytes :uchar)
    (ecase (snappy-validate-compressed-buffer compressed-bytes (length bytes))
      (:ok t)
      (:invalid-input (error "Invalid input!"))
      (:buffer-too-small (error "Buffer too small!")))))


(defun make-byte-vector (size)
  (make-array size :element-type '(unsigned-byte 8)))
