(defun run-all-instances (function block-size instances)
  (declare (type simple-vector instances)
           (type function function)
           (type index block-size))
  (let* ((len   (length instances))
         (times (make-array (ceiling len block-size)))
         last-value)
    (loop for i below len by block-size
          for j upfrom 0
          for last of-type index = (min len (+ i block-size))
          do (multiple-value-bind (value time)
                 (sb-vm::with-cycle-counter
                   (let (dst)
                     (loop for i from i below last
                           do (setf dst (funcall function (aref instances j)))
                           finally (return dst))))
               (setf (aref times j) (/ (float time 1d0)
                                       (- last i))
                     last-value     value)))
    (sort times #'<)
    (values (aref times (truncate (length times) 20))
            last-value)))

(defun make-inputs (size)
  (let ((count (max 2 (truncate (ash 256 20) ;; at least 256 MB
                                (* size 16)))))
    (map-into (make-array count)
              (let ((count 0))
                (lambda ()
                  (incf count)
                  (map-into (make-array size :element-type 'complex-sample)
                            (lambda ()
                              (complex (random 1d0) count))))))))

(defun time-inputs (makers n &optional (block-size 16))
  (when (atom makers)
    (setf makers (list makers)))
  (let ((funs (let ((*error-output* (make-broadcast-stream)))
                (mapcar (lambda (maker)
                          (if (atom maker)
                              (funcall maker n)
                              (apply (first maker) n (rest maker))))
                        makers))))
    (flet ((run-it ()
             (let* ((outputs '())
                    (cycles
                      (loop with inputs = (make-inputs n)
                            for fun in funs
                            collect (multiple-value-bind (cycles dst)
                                        (run-all-instances fun
                                                           block-size
                                                           inputs)
                                      (push dst outputs)
                                      cycles))))
               #+nil
               (loop with base = (first outputs)
                     for maker in (rest makers)
                     for output in (rest outputs)
                     unless (and (= (length base)
                                    (length output))
                                 (every (lambda (x y)
                                          (< (/ (abs (- x y))
                                                (max (abs x) (abs y) 1d0))
                                             1d-5))
                                        (first outputs) output))
                       do (format t "Mismatch for ~A ~A ~A~%" maker
                                  (length base) (length output)))
               cycles)))
      (mapcar #'min (run-it) (run-it)))))

(defun make-bordeaux-fft (n)
  (let ((dst (make-array n :element-type 'complex-sample))
        (instance (bordeaux-fft::make-fft-instance n)))
    (lambda (src)
      (bordeaux-fft::fft-common instance src dst))))

(defun make-small-fft (n)
  (let* ((fft (compile nil `(lambda (dst src twiddle)
                              (declare (type complex-sample-array dst src
                                             twiddle)
                                       (ignorable twiddle)
                                       (optimize speed (safety 0)))
                              ,(gen-fft/small n :startd 0 :starts 0 :twiddle 'twiddle)
                              dst)))
         (twiddle (make-twiddle-factors n 1))
         (dst     (make-array n :element-type 'complex-sample)))
    (declare (type function fft))
    (lambda (src)
      (funcall fft dst src twiddle))))

(defun make-medium-fft (n)
  (let* ((fft (compile nil `(lambda (dst src tmp twiddle ck)
                              (declare (type complex-sample-array dst src
                                             tmp twiddle ck)
                                       (ignorable twiddle)
                                       (optimize speed (safety 0)))
                              ,(gen-fft/medium n :startd 0 :starts 0
                                                 :startt 0
                                                 :twiddle 'twiddle
                                                 :cooley-tukey 'ck)
                              dst)))
         (twiddle (make-twiddle-factors n 1)) ;; only need sqrt
         (ck      (make-all-factors (integer-length (1- n)) 1))
         (dst     (make-array n :element-type 'complex-sample))
         (tmp     (make-array n :element-type 'complex-sample)))
    (declare (type function fft))
    (lambda (src)
      (funcall fft dst src tmp twiddle ck))))

(defun make-large-fft (n &optional (lower 'gen-fft/medium))
  (let* ((size1 (ash 1 (truncate (integer-length (1- n))
                                2)))
         (size2 (/ n size1))
         (transpose (compile nil `(lambda (vec tmp)
                                    (declare (type complex-sample-array vec tmp)
                                             (optimize speed (safety 0)))
                                    ,(generate-transpose size2 size1 nil
                                                         :vecs 0 :tmps 0))))
         (fft (compile nil `(lambda (dst src tmp twiddle ck)
                              (declare (type complex-sample-array dst src
                                             tmp twiddle ck)
                                       (ignorable twiddle tmp)
                                       (optimize speed (safety 0)))
                              ,(gen-fft/large n
                                              :startd 0
                                              :starts 0
                                              :startt 0
                                              :twiddle 'twiddle
                                              :cooley-tukey 'ck
                                              :lower lower)
                              dst)))
         (twiddle (make-twiddle-factors n 1)) ;; only need sqrt
         (ck      (make-all-factors (integer-length (1- n)) 1))
         (dst     (make-array n :element-type 'complex-sample))
         (src2    (make-array n :element-type 'complex-sample))
         (tmp     (make-array n :element-type 'complex-sample)))
    (declare (type function fft)
             (ignorable src2))
    (lambda (src)
      (declare (type complex-sample-array src))
      #+nil(replace src2 src)
      #+nil
      (funcall transpose src tmp)
      (funcall fft dst src
               tmp twiddle ck)
      #+nil(funcall transpose dst tmp)
      dst)))

(defvar *runs* '(((2 5) make-bordeaux-fft make-small-fft)
                 ((6 7) make-bordeaux-fft make-small-fft
                    make-medium-fft
                    (make-large-fft gen-fft/small))
                 ((8 20) make-bordeaux-fft make-small-fft
                    make-medium-fft
                    (make-large-fft gen-fft/small)
                    make-large-fft)
                 ((21 25) make-bordeaux-fft
                    make-medium-fft
                    (make-large-fft gen-fft/small)
                    make-large-fft)))

(defun execute-runs (&optional (runs *runs*))
  (loop for (span . specs) in runs do
    (let ((first (if (atom span)
                     span
                     (first span)))
          (last   (if (atom span)
                      span
                      (second span)))
          (*print-pretty* nil))
      (loop for size from first upto last
            do (format t "~A \"~A\" ~{~10,1,F ~}~%" size specs
                       (time-inputs specs (ash 1 size)))
            do (gc :full t)))))
