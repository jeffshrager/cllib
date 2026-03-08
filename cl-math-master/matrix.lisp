;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/04/27 
;;;;
;;;; Marix 2.0
;;;; Perform mathematical operations, mainly with matrices (vector is considered
;;;; as matrix as well). Matrices are represented by lists. Most operations have
;;;; strict prototype in order to avoid running additional functions and don't
;;;; control underflow or overflow of dimension indices. Rows and cols are
;;;; counted from zero.

;;; MATRIX CREATION
;;; * (empty-matrix rows cols)

;;; * (empty shape)
;;; * (zeros shape)
;;; * (ones shape)

;;; * (empty-like mat)
;;; * (zeros-like mat)
;;; * (ones-like mat)

;;; * (initialize-matrix rows cols val)
;;; * (rand-norm-matrix rows cols)
;;; * (matrix-from-data data)

;;; * (matrix-from-data-peel data) TODO try to avoid
;;; * (matrix-data-peel data) TODO try to avoid
;;;
;;; SINGLE MATRIX OPERATIONS
;;; * (matrix-shape mat)
;;; * (transpose mat)
;;; * (nth-row mat row)
;;; * (nth-col col mat)
;;; * ([] from to mat)
;;; * ([][] row col mat)

;;; * (apply-mat mat fun)

;;; * (remove-col mat col-idx)
;;; * (remove-row mat row-idx)
;;; * (remove-row-list-matrix row list-mat)

;;; * (prepend-col-val mat val)
;;; * (append-col-val val mat)
;;; * (insert-col-val val mat idx)
;;; * (matrix-indices rows cols)

;;; * (sigmoid mat) ; TODO move to different package
;;; * (sigmoid-prime mat) ; TODO move to different package

;;; * (shuffle-rows mat)
;;; * (shuffle-rows-spec mat idx-list)

;;; * (det mat)
;;; * (inv mat)
;;;
;;; MATRIX MULTIPLICATION
;;; ** number of cols of mat1 has to be equal number of rows of mat2
;;; * (dot mat1 mat2)
;;;
;;; ELEMENT-WISE OPERATIONS
;;; ** requires the same dimensions of both matrices
;;; ** uniqueness of parameter order depends on commutative property of employed
;;;    mathematical function
;;; * (+mm m1 m2)
;;; * (-mm m1 m2)
;;; * (*mm m1 m2)
;;; * (/mm m1 m2)
;;;
;;; MATRIX & VALUE OPERATIONS
;;; * (+mv mat val)
;;; * (-mv mat val)
;;; * (*mv mat val)
;;; * (/mv mat val)
;;; * (power mat val)
;;;
;;; MATRIX-ROW OPERATIONS
;;; * (+mr mat row)
;;; * (-mr mat row)
;;; * (*mr mat row)
;;; * (/mr mat row)
;;;
;;; * MATRIX-COL OPERATIONS
;;; * (+mc mat col)
;;; * (-mc mat col)
;;; * (*mc mat col)
;;; * (/mc mat col)

;;; * (subtract-val-col val col mat)
;;; * TODO == subtract-val-col (-cv mat col-idx val)
;;; * (sum-rows mat)
;;; * (sum-cols mat)
;;; * (sum mat)

;;; * (mean-rows mat)
;;; * TODO (std-rows mat mean)
;;; * (mean-cols mat)
;;; * (std-cols mat mean)

;;; * (arg-sort-col-mat col-mat) TODO accept matrices with more than one column
;;; * TODO (arg-sort-row-mat row-mat)

;;; * (nth-col-max mat col-idx)
;;; * (nth-col-min mat col-idx)
;;; * (nth-row-max mat row-idx)
;;; * (nth-row-min mat row-idx)
;;;
;;; MATRIX to MATRIX OPERATIONS
;;; * (vstack mat-left mat-right)
;;; * (hstack mat-top mat-bottom)

;;; TODO 
;;; nth-row nth-col base functions for returning complete matrices
;;; unit test for EVERY FUNCTION
;;; more inspiration from numpy

(setf *matrix-namespace* '())

(defstruct matrix rows cols data)

(define-condition matrix-error (error)
  ((text :initarg :text :reader text)))

;;; Compare rows and columns of two matrices and their data.
(push 'compare-matrix *matrix-namespace*)
(defun compare-matrix (mat-a mat-b)
  (cond 
    ((not (equal (matrix-rows mat-a) (matrix-rows mat-b))) nil)
    ((not (equal (matrix-cols mat-a) (matrix-cols mat-b))) nil)
    ((not (equal (matrix-data mat-a) (matrix-data mat-b))) nil)
    (t t)))

;;; MATRIX CREATION

(defmacro empty-matrix-macro (rows cols &rest default)
  `(flet ((generate-empty-matrix (,rows ,cols)
       (loop for i from 1 to ,rows
          collect ,@default)))

    (make-matrix :rows ,rows
                 :cols ,cols
                 :data (generate-empty-matrix ,rows ,cols))))

;;; Create a matrix of size [rows; cols] filled with NIL values.
(push 'empty-matrix *matrix-namespace*)
(defun empty-matrix (rows cols &optional (default NIL))
  (empty-matrix-macro rows cols (make-list cols :initial-element default)))

;;; Auxiliary function for zeros and ones functions.
(push 'matrix-from-shape *matrix-namespace*)
(defun matrix-from-shape (shape value)
  (if (listp shape)
    (empty-matrix (car shape) (cadr shape) value)
    (empty-matrix 1           shape        value)))

;;; Create matrix of given shape filled with NIL.
;;; If shape is a defined as a single number, row matrix will be created.
(push 'empty *matrix-namespace*)
(defun empty (shape)
  (let ((value NIL))
    (matrix-from-shape shape value)))

;;; Create matrix of given shape filled with zeros.
;;; If shape is a defined as a single number, row matrix will be created.
(push 'zeros *matrix-namespace*)
(defun zeros (shape)
  (let ((value 0))
    (matrix-from-shape shape value)))

;;; Create matrix of given shape filled with ones.
;;; If shape is a defined as a single number, row matrix will be created.
(push 'ones *matrix-namespace*)
(defun ones (shape)
  (let ((value 1))
    (matrix-from-shape shape value)))

;;; Auxiliary function for zeros-like and ones-like.
(push 'matrix-like *matrix-namespace*)
(defun matrix-like (mat value)
  (empty-matrix
    (matrix-rows mat)
    (matrix-cols mat)
    value))

;;; Create an empty matrix of the same size as given matrix.
(push 'empty-like *matrix-namespace*)
(defun empty-like (mat)
  (let ((value NIL))
    (matrix-like mat value)))

;;; Create a matrix of the same size as given matrix with 0 values.
(push 'zeros-like *matrix-namespace*)
(defun zeros-like (mat)
  (let ((value 0))
    (matrix-like mat value)))

;;; Create a matrix of the same size as given matrix with 0 values.
(push 'ones-like *matrix-namespace*)
(defun ones-like (mat)
  (let ((value 1))
    (matrix-like mat value)))

;;; Generate and initialize matrix with a given value.
(push 'initialize-matrix *matrix-namespace*)
(defun initialize-matrix (rows cols val)
  (empty-matrix-macro rows cols (make-list cols :initial-element val)))

;;; Generate matrix filled with random normal values.
;;; mean 0
;;; std  1
;;; TODO: keep here?
(push 'rand-norm-matrix *matrix-namespace*)
(defun rand-norm-matrix (rows cols)
  (empty-matrix-macro rows cols (make-list-rand-normal cols)))

;;; Control if all rows have the same number of columns.
(push 'valid-matrix *matrix-namespace*)
(defun valid-matrix (data)
  (let ((exp-row-len
          (make-list (length data) :initial-element (length (car data)))))

    (if (equal (mapcar #'length data)
               exp-row-len)
      t
      nil)))

;;; Create a matrix structure from given data (list of lists).
(push 'matrix-from-data *matrix-namespace*)
(defun matrix-from-data (data)
  (if (not (valid-matrix data))
      (error 'matrix-error :text "Length of matrix rows is not consistent."))

  (let* ((c (length (car data)))
         (r (if (= c 0)
                 0
                 (length data)))
         (d (if (or (= r 0)
                       (= c 0))
                 nil
                 data)))

    (make-matrix :rows r
                 :cols c
                 :data d)))

;;; Adds additional layer (list) around given data in order to be able to create
;;; matrix from this data.
;;; TODO create abstraction for peel
(defun matrix-from-data-peel (data)
  (matrix-from-data (list data)))

;;; Removes layer (access the first item of list) from given matrix.
;;; TODO create abstraction for peel
(defun matrix-data-peel (data)
  (car (matrix-data data)))

;;; SINGLE MATRIX OPERATIONS

(push 'matrix-shape *matrix-namespace*)
(defun matrix-shape (mat)
  (list (matrix-rows mat)
          (matrix-cols mat)))

;;; Auxiliary function. Shouldn't be employed by itself? TODO move to somewhere else?
(push 'transpose-list *matrix-namespace*)
(defun transpose-list (lst)
  (apply #'mapcar (cons #'list lst)))

;;; Transpose matrix.
(push 'transpose *matrix-namespace*)
(defun transpose (mat)
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (data (matrix-data mat)))

    (make-matrix :rows cols
                 :cols rows
                 :data (transpose-list data))))

;;; Auxiliary function for NTH-ROW function.
(push 'nth-row-ret-lst *matrix-namespace*)
(defun nth-row-ret-lst (mat row-idx)
  (list (nth row-idx (matrix-data mat))))

;;; Return n-th row from given matrix.
(push 'nth-row *matrix-namespace*)
(defun nth-row (mat row-idx)
  (make-matrix :rows 1
               :cols (matrix-cols mat)
               :data (nth-row-ret-lst mat row-idx)))

;;; Auxiliary function for NTH-COL function.
(push 'nth-col-ret-lst *matrix-namespace*)
(defmacro nth-col-ret-lst (mat col-idx)
  `(mapcar #'(lambda (x) (list (nth ,col-idx x))) (matrix-data ,mat)))

;;; Return n-th column from given matrix.
(push 'nth-col *matrix-namespace*)
(defun nth-col (mat col-idx)
  (make-matrix :rows (matrix-rows mat)
               :cols 1
               :data (nth-col-ret-lst mat col-idx)))

(push 'matrix-smart-convert *matrix-namespace*)
(defun matrix-smart-convert (mat)
  (if (equal (matrix-shape mat) '(1 1))
    (caar (matrix-data mat))
    mat))

;;; Auxiliary function for [].
(defun []-op (mat-list row-idx col-from col-to)
  (let ((col-to (1+ col-to)))

    (mapcar #'(lambda (r)
                (subseq (nth r mat-list) col-from col-to))
            row-idx)))

(push '[] *matrix-namespace*)
(defun [] (mat &key (row nil) (col nil))
  (multiple-value-bind
    (mat-list row-idx col-from col-to) ([]-prep mat :row row :col col)

    (let ((mat-tmp (matrix-from-data
                     ([]-op mat-list row-idx col-from col-to))))

    (matrix-smart-convert mat-tmp))))

;;; Auxiliary function for [].
(defun setf-[] (mat-list val-list row-idx col-from col-to)
  (let ((col-to (1+ col-to)))

  (mapcar #'(lambda (r v)
              (setf
                (subseq (nth r mat-list) col-from col-to)
                v))
          row-idx val-list)))

(push 'setf-[] *matrix-namespace*)
(defun (setf []) (val mat &key (row nil) (col nil))
  (multiple-value-bind
    (mat-list row-idx col-from col-to) ([]-prep mat :row row :col col)

    (let* ((col-idx (list (first col-idx) (car (last col-idx))))
           (val (if (numberp val)
                  (list (list val))
                  val))
           (mat-tmp (matrix-from-data
                      (setf-[] mat-list val row-idx col-from col-to))))

    (matrix-smart-convert mat-tmp))
    ))

(defun []-prep (mat &key (row nil) (col nil))
  (flet ((gen-enum (borders boundary)
           (cond ((null borders)
                   (iota-range 0 (1- boundary)))

                  ((listp borders)
                  (iota-range (car borders) (cadr borders)))

                  (t
                    (list borders))))

        (gen-col-range (borders boundary)
          (cond ((null borders)
                 (values 0 (1- boundary)))

                ((listp borders)
                 (values (car borders) (cadr borders)))

                (t
                  (values borders borders)))))

    (multiple-value-bind (col-from col-to) (gen-col-range col (matrix-cols mat))

      (values
        (matrix-data mat)
        (gen-enum row (matrix-rows mat))
        col-from
        col-to))))

;;; Access value of 2D matrix.
;;; Does not control access outside of matrix.
;;; TODO replace with []?
(push '[][] *matrix-namespace*)
(defun [][] (row col mat)
  ([] mat :row row :col col))

;;; Remove column from given matrix.
;;; Create new matrix.
(push 'remove-col *matrix-namespace*)
(defun remove-col (mat col-idx)
  (matrix-from-data
    (mapcar #'(lambda (x) (remove-nth col-idx x)) (matrix-data mat))))

;;; Removes row from given matrix.
;;; Create a new matrix.
(push 'remove-row *matrix-namespace*)
(defun remove-row (mat row-idx)
  (matrix-from-data (remove-nth row-idx (matrix-data mat))))

;;; Remove row from matrix composed of lists.
(push 'remove-row-list-matrix *matrix-namespace*)
(defun remove-row-list-matrix (row list-mat)
  (remove-nth row list-mat))

;;; Append constant number at the beginning of each row of given matrix.
(push 'prepend-col-val *matrix-namespace*)
(defun prepend-col-val (mat val)
  (matrix-from-data (mapcar #'(lambda (x) (push val x)) (matrix-data mat))))

;;; Append constant number at the end of each row of given matrix.
(push 'append-col-val *matrix-namespace*)
(defun append-col-val (mat val)
  (matrix-from-data (mapcar #'(lambda (x) (append x (list val))) (matrix-data mat))))

;;; Insert constant value (whole column) at given position (column) in matrix.
;;; Does not control access out of boundaries.
(push 'insert-col-val *matrix-namespace*)
(defun insert-col-val (mat val &key idx)
  (let ((end (matrix-cols mat)))
    (matrix-from-data (mapcar #'(lambda (x) (append (subseq x 0 idx) (list val) (subseq x idx end)))
                              (matrix-data mat)))))

;;; Auxiliary function for MATRIX-INDICES.
(defun matrix-indices-rec (rows cols orig-cols)
  (if (eq rows 0)
    nil
    (cons (cons (1- rows) (1- cols))
          (if (eq (1- cols) 0)
            (matrix-indices-rec (1- rows) orig-cols orig-cols)
            (matrix-indices-rec rows (1- cols) orig-cols)))))

;;; Generate list of matrix indices from given matrix dimensions.
(push 'matrix-indices *matrix-namespace*)
(defun matrix-indices (rows cols)
  (matrix-indices-rec rows cols cols))

;;; Basic sigmoid function.
;;; Accept only single number.
;;; TODO move to only mathematical library for common functions?
(push 'sigmoid-base *matrix-namespace*)
(defun sigmoid-base (num)
    (handler-bind
      ((floating-point-underflow #'(lambda (x) (return-from sigmoid-base 1.0)))
       (floating-point-overflow  #'(lambda (x) (return-from sigmoid-base 0.0))))

      (/ 1 (+ 1 (exp (- num))))))

;;; Calculate sigmoid of each value in given matrix.
;;; TODO move to only mathematical library for common functions?
(push 'sigmoid *matrix-namespace*)
(defun sigmoid (mat)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (sigmoid-base y)) x))
            (matrix-data mat))))

;;; Derivation of sigmoid function.
;;; TODO move to only mathematical library for common functions?
(push 'sigmoid-prime *matrix-namespace*)
(defun sigmoid-prime (mat)
  (let ((s (sigmoid mat)))
    (*mm
      s
      (-mm (ones-like s) s))))

;;; Randomly shuffle rows of matrix.
;;; TODO unit test?
(push 'shuffle-rows *matrix-namespace*)
(defun shuffle-rows (mat)
  (let ((n-rows (matrix-rows mat))
        (mat-list (matrix-data mat))
        (mat-shuffled NIL)
        (rand-idx 0))

  (dotimes (i n-rows)
    (setf rand-idx (random (- n-rows i)))
    (push (nth rand-idx mat-list) mat-shuffled)
    (setf mat-list (remove-row-list-matrix rand-idx mat-list)))

  (matrix-from-data mat-shuffled)))

;;; Shuffle rows according to specification.
;;; Specification is list with index ordering.
;;; Function assumes number of indexes less than number of matrix rows.
(push 'shuffle-rows-spec *matrix-namespace*)
(defun shuffle-rows-spec (mat idx-list)
  (let ((mat-list (matrix-data mat)))

    (matrix-from-data
      (mapcar #'(lambda (idx) (nth idx mat-list)) idx-list))))

;;; Calculate determinant of given matrix.
;;; Matrix has to have square shape.
(push 'det *matrix-namespace*)
(defun det (mat)
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (first-row (nth 0 (nth-row-ret-lst mat 0))))

    (cond
      ((not (eq rows cols))
        (error 'matrix-error :text "Matrix must be square"))

      ((eq rows 2)
        (- (* ([][] 0 0 mat) ([][] 1 1 mat))
           (* ([][] 0 1 mat) ([][] 1 0 mat))))

      (t
        (apply #'+ (mapcar #'(lambda (idx val) (if (is-odd idx)
                                                  (apply #'* (list -1 val (det (det-submatrix idx mat))))
                                                  (apply #'* (list val (det (det-submatrix idx mat))))))
                                (iota cols) first-row)))
    )))

;;; Extract submatrix used in recursive determinant calculation.
;;; Auxiliary function for DET function.
(defun det-submatrix (col mat)
  (let ((last-row-idx (- (matrix-rows mat) 1)))
    (remove-col ([] mat :row (list 1 last-row-idx)) col)))

;;; Compute inverse of a given matrix.
(push 'inv *matrix-namespace*)
(defun inv (mat)
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (tmp-row-flag NIL)
        (tmp-col-flag NIL))

    (cond
      ((not (eq rows cols))
        (error 'matrix-error :text "Matrix must be square"))

      ((eq rows 2)
        (*mv (matrix-from-data (list (list    ([][] 1 1 mat)  (- ([][] 0 1 mat)))
                                          (list (- ([][] 1 0 mat))    ([][] 0 0 mat))))
             (/ 1 (det mat))))

      (t
        (*mv (transpose (matrix-from-data
             (mapcar #'(lambda (row) (progn (setf tmp-row-flag (if (is-odd row) -1 1))
                                            (mapcar #'(lambda (col) (progn (setf tmp-col-flag (if (is-odd col) -1 1))
                                                                           (apply #'* (list tmp-row-flag tmp-col-flag (det (remove-col (remove-row mat row) col))))))
                               (iota cols)))) (iota rows))))
             (/ 1 (det mat)))))))

;;; Apply lambda function to each value of matrix.
(push 'apply-mat *matrix-namespace*)
(defmacro apply-mat (mat lmbd-fun)
  `(let ((matrix-lst (matrix-data ,mat)))

     (matrix-from-data
       (mapcar #'(lambda (row)
                   (mapcar #',lmbd-fun row)) matrix-lst))))

;;; MATRIX MULTIPLICATION

;;; Element-wise product of two vectors.
;;; Assume correct vector dimensions.
;;; TODO move to some library with simplier functions?
(push 'vec-mult *matrix-namespace*)
(defun vec-mult (vec1 vec2)
  (apply #'+ (mapcar #'* vec1 vec2)))

;;; Calculate the new values for one cell of result matrix.
;;; Auxiliary function for DOT-REC.
(defun dot-cell-calc (mat-out row-idx col-idx row-vec col-vec)
  (setf (nth col-idx (nth row-idx (matrix-data mat-out)))
        (vec-mult (car row-vec)
                   (car (transpose-list col-vec))))

  mat-out)

;;; Auxiliary function for DOT function.
(defun dot-rec (mat-out mat-l mat-r mat-idxs)
  (if (eq (car mat-idxs) nil)
    mat-out
    (let* ((row-idx (caar mat-idxs))
           (col-idx (cdar mat-idxs))
           (row-vec (nth-row-ret-lst mat-l row-idx))
           (col-vec (nth-col-ret-lst mat-r col-idx)))

    (dot-rec (dot-cell-calc mat-out row-idx col-idx row-vec col-vec) mat-l mat-r (cdr mat-idxs)))))

;;; Control matrix dot product validity.
;;; Auxiliary function for DOT function.
(defun valid-dot-op (mat-l mat-r)
  (if (not (= (matrix-cols mat-l)
              (matrix-rows mat-r)))
    (error 'matrix-error :text "Matrices cannot be multiplied. Dimensions do not fit.")))

;;; Matrix multiplication of two matrices.
;;; If keep is T no optimizations on matrix size will be performed.
;;; TODO should we slow down performance with VALID-DOT-OP?
;;; TODO unit-test with keep parameter
(push 'dot *matrix-namespace*)
(defun dot (mat-l mat-r &key (keep nil))
  (valid-dot-op mat-l mat-r)

  (let* ((rows (matrix-rows mat-l))
         (cols (matrix-cols mat-r))
         (mat-out (empty-matrix rows cols))
         (mat-idxs (matrix-indices rows cols)))

    (setf mat-out (dot-rec mat-out mat-l mat-r mat-idxs))

    (if (and (= (matrix-rows mat-out) 1)
             (= (matrix-cols mat-out) 1)
             (not keep))
      (caar (matrix-data mat-out))
      mat-out)))

;;; ELEMENT-WISE OPERATIONS

;;; Auxiliary function for ADD, SUBTRACT and MATRIX-MULT.
(defun element-wise-op (lst-l lst-r op)
  (mapcar #'(lambda (x y) (mapcar op x y)) lst-l lst-r))

(defmacro mm-op (mat-l mat-r op)
  `(matrix-from-data
    (element-wise-op (matrix-data ,mat-l) (matrix-data ,mat-r) ,op)))

;;; Element-wise add for matrices.
(push '+mm *matrix-namespace*)
(defun +mm (mat-l mat-r)
  (mm-op mat-l mat-r #'+))

;;; Elementwise subtract for matrices.
(push '-mm *matrix-namespace*)
(defun -mm (mat-l mat-r)
  (mm-op mat-l mat-r #'-))

;;; Elementwise matrix multiplication.
(push '*mm *matrix-namespace*)
(defun *mm (mat-l mat-r)
  (mm-op mat-l mat-r #'*))

(push '/mm *matrix-namespace*)
(defun /mm (mat-l mat-r)
  (mm-op mat-l mat-r #'/))

;;; MATRIX & VALUE OPERATIONS

(defmacro mv-op (mat op val)
  `(matrix-from-data
    (mapcar #'(lambda (row)
                (mapcar #'(lambda (col) (funcall ,op col ,val))
                        row))
            (matrix-data ,mat))))

;;; Subtract matrix value from given matrix.
(push '-mv *matrix-namespace*)
(defun -mv (mat val)
  (mv-op mat #'- val))

;;; Add constant value to given matrix.
(push '+mv *matrix-namespace*)
(defun +mv (mat val)
  (mv-op mat #'+ val))

;;; Multiply matrix with a given value.
(push '*mv *matrix-namespace*)
(defun *mv (mat val)
  (mv-op mat #'* val))

(push '/mv *matrix-namespace*)
(defun /mv (mat val)
  (mv-op mat #'/ val))

;;; Compute power using given exponent at each cell of matrix.
(push 'power *matrix-namespace*)
(defun power (mat val)
  (matrix-from-data
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (expt y val)) x))
            (matrix-data mat))))

;;; MATRIX-ROW/COL OPERATIONS

;;; Auxiliary function for ELWISE-MAT-ROW-OP.
(defun elwise-row-row-op (lst-row-l lst-row-r op)
  (mapcar #'(lambda (x y) (apply op (list x y))) lst-row-l lst-row-r))

;;; Auxiliary function for -mr.
(defun elwise-mat-row-op (lst-mat lst-row op)
  (mapcar #'(lambda (x) (elwise-row-row-op x lst-row op)) lst-mat))

;;; Auxiliary function for matrix and row operations.
;;; Used by +mr, -mr, *mr and /mr.
(defun mr-op (mat row op)
  (matrix-from-data
    (elwise-mat-row-op (matrix-data mat) (car (matrix-data row)) op)))

;;; Element-wise subtract values of given row from all rows in matrix.
(push '-mr *matrix-namespace*)
(defun -mr (mat row)
  (mr-op mat row #'-))

(push '+mr *matrix-namespace*)
(defun +mr (mat row)
  (mr-op mat row #'+))

(push '*mr *matrix-namespace*)
(defun *mr (mat row)
  (mr-op mat row #'*))

(push '/mr *matrix-namespace*)
(defun /mr (mat row)
  (mr-op mat row #'/))

;;; Auxiliary function for matrix and column operations.
;;; Used by +mc, -mc, *mc and /mc.
(defun mc-op (mat col op)
  (let ((col-trans (transpose col))
        (mat-trans (transpose mat)))

    (transpose
      (mr-op col-trans mat-trans op))))

(push '+mc *matrix-namespace*)
(defun +mc (col mat)
  (mc-op mat col #'+))

;;; Element-wise subtract values of given column from all columns in matrix.
(push '-mc *matrix-namespace*)
(defun -mc (col mat)
  (mc-op mat col #'-))

(push '*mc *matrix-namespace*)
(defun *mc (col mat)
  (mc-op mat col #'*))

(push '/mc *matrix-namespace*)
(defun /mc (col mat)
  (mc-op mat col #'/))

;;; Subtract value from specified column in matrix.
(push 'subtract-val-col *matrix-namespace*)
(defun subtract-val-col (val col mat)
  (matrix-from-data
    (mapcar #'(lambda (row) (let ((row-val (nth col row)))
                                   (setf (nth col row) (- row-val val))
                                   row))
              (matrix-data mat))))

;;; Perform aggregating operation on each row of matrix.
;;; Auxiliary function for SUM-ROWS.
(defun rows-op (mat-lst op)
  (mapcar #'(lambda (x) (list (apply op x))) mat-lst))

;;; Sum values at each row of matrix.
(push 'sum-rows *matrix-namespace*)
(defun sum-rows (mat)
  (matrix-from-data
    (rows-op (matrix-data mat) #'+)))

;;; Sum values at each column of matrix.
(push 'sum-cols *matrix-namespace*)
(defun sum-cols (mat)
  (transpose (matrix-from-data
               (rows-op (matrix-data (transpose mat)) #'+))))

;;; Sum all values in a given matrix.
(push 'sum *matrix-namespace*)
(defun sum (mat)
  (let ((total-sum 0))
    (mapcar #'(lambda (x) (incf total-sum (reduce #'+ x)))
            (matrix-data mat))

    total-sum))

;;; Compute mean for all columns.
(push 'mean-cols *matrix-namespace*)
(defun mean-cols (mat)
  (let ((num-rows (matrix-rows mat)))

    (matrix-from-data (transpose-list
      (mapcar #'(lambda (column)
                  (list (/ (apply #'+ column) num-rows)))
              (transpose-list (matrix-data mat)))))))

;;; Compute mean for all rows
(push 'mean-rows *matrix-namespace*)
(defun mean-rows (mat)
  (let ((num-cols (matrix-cols mat)))
    
    (matrix-from-data 
      (mapcar #'(lambda (row) 
                  (list (/ (apply #'+ row) num-cols)) )
              (matrix-data mat)))))

;;; Compute standard deviation for all columns.
;;; TODO unit tests
(push 'std-cols *matrix-namespace*)
(defun std-cols (mat mean)
  (let ((mat-list (matrix-data mat))
        (mean-list (matrix-data-peel mean))
        (row-num (matrix-rows mat))
        (std-list (make-list (matrix-cols mat) :initial-element 0)))

    (mapcar #'(lambda (row)
      (setf std-list
        (mapcar #'+ std-list ;; add to temporary value
                (mapcar #'(lambda (val) (expt val 2)) ;; compute squares
                                     (mapcar #'- row mean-list)))))
      mat-list)

    (matrix-from-data-peel (mapcar #'sqrt
      (mapcar #'(lambda (val) (/ val row-num)) std-list)))))

;;; Normalize data.
;;; TODO move to different file/library
;(push 'normalize *matrix-namespace*)
;(defun normalize (mat mean std)
;  (let ((mean-list (matrix-data-peel mean))
;        (std-list (matrix-data-peel std)))
;
;    (matrix-from-data
;      (mapcar #'(lambda (row)
;                  (mapcar #'/ (mapcar #'- row mean-list) std-list))
;              (matrix-data mat)))))

;;; Sorts column vector and return indices of sorted vector.
(push 'arg-sort-col-mat *matrix-namespace*)
(defun arg-sort-col-mat (col-mat)
  (let* ((vec (matrix-data col-mat))
         (idxs (iota (length vec)))
         (join-vec-idxs (mapcar #'(lambda (x y) (cons (car x) y)) vec idxs)))

    (mapcar #'(lambda (x) (cdr x)) (stable-sort join-vec-idxs #'< :key #'car))))

;;; Auxiliary function for NTH-COL-MAX nd NTH-COL-MIN functions.
(defmacro nth-col-op (mat idx op)
  `(funcall ,op
           (car (transpose-list (nth-col-ret-lst ,mat ,idx)))))

;;; Find the largest value in specific column of a given matrix.
(push 'nth-col-max *matrix-namespace*)
(defun nth-col-max (mat col-idx)
  (nth-col-op mat col-idx #'maximum))

;;; Find the smallest value in specific column of a given matrix.
(push 'nth-col-min *matrix-namespace*)
(defun nth-col-min (mat col-idx)
  (nth-col-op mat col-idx #'minimum))

;;; Auxiliary function for NTH-ROW-MAX nd NTH-ROW-MIN functions.
(defmacro nth-row-op (mat idx op)
  `(funcall ,op (car (nth-row-ret-lst ,mat ,idx))))

;;; Find the largest value in specific row of a given matrix.
(push 'nth-row-max *matrix-namespace*)
(defun nth-row-max (mat row-idx)
  (nth-row-op mat row-idx #'maximum))

;;; Find the largest value in specific row of a given matrix.
(push 'nth-row-min *matrix-namespace*)
(defun nth-row-min (mat row-idx)
  (nth-row-op mat row-idx #'minimum))

;;; Concatenate matrices vertically.
(push 'vstack *matrix-namespace*)
(defun vstack (mat-left mat-right)
  (if (not (and
        (= (matrix-rows mat-left) (matrix-rows mat-right))))
    (error 'matrix-error :text "Number of matrix rows does not correspondent each other.")

    (matrix-from-data (mapcar #'(lambda (l r)
                                  (concatenate 'list l r))
                              (matrix-data mat-left)
                              (matrix-data mat-right)))))

;;; Concatenate matrices horizontally.
(push 'hstack *matrix-namespace*)
(defun hstack (mat-top mat-bottom)
  (if (not (and
        (= (matrix-cols mat-top) (matrix-cols mat-bottom))))
    (error 'matrix-error :text "Number of matrix columns does not correspondent each other.")

    (matrix-from-data
      (concatenate 'list (matrix-data mat-top) (matrix-data mat-bottom)))))
