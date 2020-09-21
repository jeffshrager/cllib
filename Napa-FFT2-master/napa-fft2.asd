(asdf:defsystem "napa-fft2"
  :version "0.0.1"
  :licence "BSD"
  :description "Fast Fourier Transforms in Common Lisp based on a code generator"
  :serial t
  :components ((:file "types")
               (:file "runtime")
               (:file "indexing")
               (:file "transpose")
               (:file "small-fft")
               (:file "medium-fft")
               (:file "large-fft")
               (:file "tune")))
