(defpackage :thnappy-system
  (:use :cl))

(in-package :thnappy-system)

(asdf:defsystem :thnappy
  :description "Common Lisp bindings to Google's Snappy compression library."
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.0.1"
  :license "BSD License"
  :depends-on (:cffi)
  :components
  ((:module :src
            :components
            ((:file "package")
             (:file "conditions"
                    :depends-on ("package"))
             (:file "libsnappy"
                    :depends-on ("package"))
             (:file "block-memory-operations"
                    :depends-on ("package"))
             (:file "thnappy"
                    :depends-on ("libsnappy"
                                 "block-memory-operations"))
             ))))
