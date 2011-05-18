(in-package :thnappy)

(define-condition invalid-input (error)
  ()
  (:documentation "Snappy says this is invalid input."))

(define-condition buffer-too-small (error)
  ()
  (:documentation "Snappy says that the buffer is too small."))
