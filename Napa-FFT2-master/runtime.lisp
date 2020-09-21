(defmacro define-inline-function (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args)
       ,@body)
     (define-compiler-macro ,name (&rest arg-forms)
       `((lambda (,@',args) ,@',body)
         ,@arg-forms))))

(define-inline-function mul+i (x)
  (declare (type complex-sample x))
  #+ (and sbcl complex-float-vops)
  (sb-vm::swap-complex (conjugate x))
  #- (and sbcl complex-float-vops)
  (* x #c(0 1d0)))

(define-inline-function mul-i (x)
  (declare (type complex-sample x))
  #+ (and sbcl complex-float-vops)
  (conjugate (sb-vm::swap-complex x))
  #- (and sbcl complex-float-vops)
  (* x #c(0 -1d0)))

(define-inline-function mul+/-sqrt+i (x scale)
  (declare (type complex-sample x)
           (type double-float scale))
  #+ (and sbcl complex-float-vops)
  (let ((x (* x scale)))
    (+ x (sb-vm::swap-complex (conjugate x))))
  #- (and sbcl complex-float-vops)
  (* x (complex scale scale)))

(define-inline-function mul+/-sqrt-i (x scale)
  (declare (type complex-sample x)
           (type double-float scale))
  #+ (and sbcl complex-float-vops)
  (let ((x (* x scale)))
    (- x (sb-vm::swap-complex (conjugate x))))
  #- (and sbcl complex-float-vops)
  (* x (complex scale (- scale))))

(defun mul-root (x root &optional default)
  (setf root (mod root 1))
  (case root
    (0 x)
    (1/2
     `(- ,x))
    (1/4
     `(mul+i ,x))
    (3/4
     `(mul-i ,x))
    ((1/8 5/8)
     `(mul+/-sqrt+i
       ,x
       ,(case root
          (1/8 (/ (sqrt 2d0) 2d0))
          (5/8 (- (/ (sqrt 2d0) 2d0))))))
    ((3/8 7/8)
     `(mul+/-sqrt-i
       ,x
       ,(case root
          (3/8 (- (/ (sqrt 2d0) 2d0)))
          (7/8 (/ (sqrt 2d0) 2d0)))))
    (t
     (assert default)
     `(* ,x ,default))))

(declaim (inline scale))
(defun scale (x y)
  (declare (type complex-sample x)
           (type double-float y))
  (* x y))

(define-compiler-macro scale (x y &environment env)
  (if (or (and (numberp y)
               (= y 1))
          #+sbcl
          (and (constantp y env)
               (let ((y (sb-int:constant-form-value y env)))
                 (and (numberp y)
                      (= 1 y)))))
      `(the complex-sample ,x)
      `((lambda (x y)
          (declare (type complex-sample x)
                   (type double-float y))
          (* x y))
        ,x ,y)))

(defmacro %twiddle (x twiddle index)
  (if twiddle
      `(* ,x (ref ,twiddle ,index))
      x))

(defmacro swapf (x y)
  `(let ((x ,x)
         (y ,y))
     (declare (type complex-sample-array x y))
     (setf ,x y
           ,y x)))

;; cough.
(defun make-twiddle-factors (size direction)
  (let ((coeffs (make-array size :element-type 'complex-sample))
        (coeff #c(1d0 0d0))
        (factor (exp (/ (* direction -2 pi #c(0 1)) size))))
    (loop for i of-type fixnum from (ash size -1) below size
          do (setf (aref coeffs i) coeff)
          do (setf coeff (* coeff factor)))
    (do ((src (ash size -1) (ash src -1))
         (src-size (ash size -1) (ash src-size -1))
         (dst (ash size -2) (ash dst -1)))
        ((< src-size 1))
      (loop for s from src below (+ src src-size) by 2
            for d upfrom dst
            do (setf (aref coeffs d) (aref coeffs s))))
    coeffs))

(defun make-cooley-tukey-factors (size1 size2 direction
                                  &optional (coeffs (make-array
                                                     (* size1 size2)
                                                     :element-type 'complex-sample))
                                    (offset 0))
  (declare (type half-index size1 size2)
           (type (member -1 1) direction)
           (type complex-sample-array coeffs)
           (type index offset))
  (let* ((size   (* size1 size2))
         (base   (/ (* direction -2 pi) size)))
    (declare (type double-float base)
             (optimize speed))
    (dotimes (i size2)
      (let ((base (* base i)))
        (dotimes (j size1)
          (let ((k (+ (* j size2) i))
                (factor (* base j)))
            (setf (aref coeffs (+ offset k)) (cis factor))))))
    coeffs))

(declaim (type (integer -1) +factor-bias+))
(defconstant +factor-bias+ -1)

(defun make-all-factors (log-max-size direction &optional previous)
  (declare (type (or null complex-sample-array) previous))
  (when previous
    (let ((length (- (length previous) +factor-bias+)))
      (assert (power-of-two-p length))
      (when (>= (integer-length (1- length)) (1+ log-max-size))
        (return-from make-all-factors previous))))
  (let ((all-factors (make-array (+ (* 2 (ash 1 log-max-size))
                                    +factor-bias+)
                                 :element-type 'complex-sample)))
    (cond (previous
           (replace all-factors previous))
          (t
           (fill all-factors #c(0d0 0d0) :end (1+ +factor-bias+))))
    (loop for i from (if previous
                         (integer-length (1- (length previous)))
                         0)
            upto log-max-size do
      (let* ((total-size (ash 1 i))
             (size1      (ash 1 (truncate i 2)))
             (size2      (ash 1 (truncate (1+ i) 2))))
        (make-cooley-tukey-factors size1 size2 direction all-factors
                                   (+ total-size +factor-bias+))))
    all-factors))

(defun power-of-two-p (x)
  (and (typep x 'index)
       (= 1 (logcount x))))
