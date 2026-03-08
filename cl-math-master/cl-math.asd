;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/01/29

(defpackage #:cl-math
  (:use :cl :asdf))

(in-package :cl-math)

(asdf:defsystem cl-math
  :name    "cl-math"
  :version "0.1"
  :author  "Martin Kersner, <m.kersner@gmail.com>"
  :long-description "Mathematical library for Common Lisp"
  :serial t
  :components ((:file "list")
               (:file "random")
               (:file "math")
               (:file "matrix")

               (:module unit-tests
                  :components ((:file "unit-test")
                               (:file "unit-test-list")
                               (:file "unit-test-math")
                               (:file "unit-test-matrix")))
               )
  )
