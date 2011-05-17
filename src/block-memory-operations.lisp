(in-package :thnappy)

;;;
;;; CFFI Block Memory Operations
;;;

(defun mem-fill (ptr type count value &optional (offset 0))
  (loop
     for i below count
     for off from offset by (foreign-type-size type)
     do (setf (mem-ref ptr type off) value)))

(defun mem-read-vector (vector ptr type count &optional (offset 0))
  (loop
     for i below (min count (length vector))
     for off from offset by (foreign-type-size type)
     do (setf (aref vector i) (mem-ref ptr type off))
     finally (return i)))

(defun mem-read-c-string (string ptr &optional (offset 0))
  (loop
     for i below (length string)
     for off from offset
     for char = (mem-ref ptr :char off)
     until (zerop char)
     do (setf (char string i) char)
     finally (return i)))

(defun mem-write-vector (vector ptr type &optional (count (length vector)) (offset 0))
  (loop
     for i below count
     for off from offset by (foreign-type-size type)
     do (setf (mem-ref ptr type off) (aref vector i))
     finally (return i)))

(defun mem-write-c-string (string ptr &optional (offset 0))
  (mem-write-vector string ptr :char (length string) offset))
