;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 symmetric-matrix-p
;;
(defun symmetric-matrix-p(a)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          symmetric-matrix-p Doc:
;;
;; Author: Andy Long
;; Created: Wed Nov 26 22:55:42 1997
;;
;; Description/Arguments:

       (symmetric-matrix-p a)
  
where a is a matrix.

;; Returns:

;; Notes:

;; See Also:

;; Examples:
 (symmetric-matrix-p (id 3))				;; t
 (symmetric-matrix-p (matrix '(2 3) (iseq 6)))		;; nil
 (symmetric-matrix-p (matrix '(2 2) '(1 2 3 4)))	;; nil
 (symmetric-matrix-p (matrix '(2 2) '(1 2 2 1)))	;; t
"
  (if (square-matrix-p a)
      (matrix-equalp
       (extract-upper-triangular a)
       (extract-upper-triangular (transpose a))
       )
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Description for SVD:
;;
(defun svd (a)
  "
;; Description: function svd

     performs the Singular Value Decomposition of a matrix. The native svd code
 (sv-decomp) requires that the number of rows be >= the number of columns. This
 being unnecessary, svd handles either case.

			A = U L V'

 where U and V are orthogonal matrices, and L is a 'diagonal' matrix containing
 the singular values on the diagonal (largest to smallest). For an mxn matrix,
 the U returned is m by (min m,n), L  is (min m,n) by (min m,n), and V is
 m by (min m,n). 

     It also assures that the singular values are returned as real (if a
 complex matrix is given, they are returned as complex with zero imaginary part
 by sv-decomp).

     Returns a list of the matrix U, a list of the singular values, the matrix
 V, and a flag indicating either that the algorithm converged (T), or NIL
 otherwise.

;; See Also: sv-decomp, pseudo-inverse

;; Examples:
 (def a (matrix '(3 2) (list 1 -1 -1 2 1 2)))
 (def svd-a (svd a))

 ;; as promised works on matrices where the number of rows is less than the
 ;; number of columns:
 (def a-transpose (transpose a))
 (def svd-a-transpose (svd a-transpose))

 ;; Here's how to put the matrix a back together again:
 (def u (first svd-a))
 (def lambda (second svd-a))
 (def v (third svd-a))
 (mprint (matmult u (diagonal-to-full lambda) (transpose v))) ;; = a

 ;; works for complex matrices, too:
 (def a #2a((2 #c(0 1))(#c(0 1) 2)))
 (def svd-a (svd a))
 (def u (first svd-a))
 (def lambda (second svd-a))
 (def v (third svd-a))

 ;; Notice that to put a complex matrix back together you need to conjugate the
 ;; (possibly complex) matrix v :
 (mprint (matmult u (diagonal-to-full lambda) (transpose (conjugate v))))
" 
  (if (matrixp a)
      (let* (    ; true, so get the matrix dimensions:
	     (size (size a))
	     (swap (< (first size) (second size)))
	     ;; if swap is true, then transpose:
	     (svd (if swap (sv-decomp (transpose a))
		    (sv-decomp a)))
	     (u (if swap (third svd) (first svd)))
	     ;; make sure that the lambda are real numbers, and return them as
	     ;; a list rather than as a vector:
	     (lambda (mapcar #'realpart (coerce (second svd) 'list)))
	     (v (if swap (first svd) (third svd)))
	     (converged (fourth svd))
	     )
	(list u lambda v converged)
	)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 mprint
;;
(defun mprint(A &rest args)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          mprint Doc:
;;
;; Author: Andy Long
;; Created: Sun Feb 1 14:46:11 1998
;;
;; Description/Arguments:

   (mprint matrix &optional stream &key (float-digits 6))
  
	pretty-prints a matrix, or a vector/list as a column matrix.

;; Returns: nil, after pretty-printing the object.

;; Notes: used to be aliased to print-matrix, then I generalized it to handle
;; vectors/lists too.

;; See Also: print-matrix

;; Examples:
 (mprint (id 3))
 (def v (rand.u 3))
 (mprint v)
 (mprint (coerce v 'vector))
 (mprint v t :float-digits 10)
 (mprint v t :float-digits 2)
"
  (apply
   #'print-matrix
   (if (matrixp A) A (bind-columns A))
   args)
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                size
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun size (&rest x)
  "
;; Description: function size (&rest x)

;; Args: object(s) such as vectors, lists or arrays. Calculates the array
;; dimensions. Vectorized.

;; Examples:
 (size #2a((1 2 3)(4 5 6)))
 (size #(1 2 3))
 (size (iseq 4))

 ;; size is vectorized:
 (size #(1 2 3) (id 3) (list 1 2))
"
  (if (= (length x) 1)
      (let ((obj (first x)))
	(if (or (listp obj) (vectorp obj))
	    (length obj)
	  (array-dimensions obj)
	  )
	)
    (mapcar #'size x)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                square-matrix-p
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun square-matrix-p (x)
  "
;; Description: function square-matrix-p (x)
Checks that x is really a square matrix.

;; Examples:
 (square-matrix-p (id 3))
 ;; true: 3 by 3 identity matrix
 (square-matrix-p (matrix '(2 3) (iseq 6)))
 ;; nil
"
  (and (matrixp x) (= (array-dimension x 0) (array-dimension x 1)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 matrix-equalp
;;
(defun matrix-equalp(a b &key (test #'equalp))
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          matrix-equalp Doc:
;;
;; Author: Andy Long
;; Created: Thu Nov 20 10:35:38 1997
;;
;; Description/Arguments:

	Function matrix-equalp(a b &key (test #'equalp))

uses equalp on the elements of matrices a and b to determine if the two
matrices are equivalent.

;; Notes:

;; See Also:

;; Examples:
 (setq A (matrix '(3 3) '(a b c d e f g h i)))
 (setq B (id 3))
 (matrix-equalp a b)
 (matrix-equalp a a)
"
  (eval `(and ,@(mapcar test (matrix-to-list a) (matrix-to-list b))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 extract-upper-triangular
;;
(defun extract-upper-triangular(a)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          extract-upper-triangular Doc:
;;
;; Author: Andy Long
;; Created: Sat Nov 22 08:10:48 1997
;;
;; Description/Arguments:

       (extract-upper-triangular matrix)
  
;; Notes:

;; See Also:

;; Examples:
 (mprint (extract-upper-triangular #2a((1 2) (3 4))))
 (mprint (extract-upper-triangular 
          #2a((2 1 0 0) (1 2 1 0) (0 1 2 1) (0 0 1 2))))
 (mprint (extract-upper-triangular 
          #2a((0 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1))))
"
  (let* (
	 (xlen (first (size a)))
	 (1-xlen (1- xlen))
	 (tmp (zeros xlen xlen))
	 )
    (dotimes (i xlen)
	     (setf (select tmp i (iseq i 1-xlen)) (select A i (iseq i 1-xlen))))
    tmp
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Description for ZEROS:
;;
(defun zeros (nrow &optional ncol)
  "
Documentation:

     ZEROS (nrow &optional ncol)

Zero matrix of dimensions nrow by ncol.

;; Examples:
 (zeros 3 4)
 (zeros 3)
" 
  (if ncol
      (make-array (list nrow ncol) :initial-element 0)
    (coerce (repeat 0 nrow) 'vector)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                matrix-to-list
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-to-list (matrix)
  "
;; Description: 

	function (matrix-to-list matrix)

turns a matrix into a list, row by row: I once thought that combine would work,
but if the matrix elements are lists, then you get into trouble.

;; Examples:
 (matrix-to-list (id 3))
 (matrix-to-list (matrix '(3 4) '(1 2 3 4 5 6 7 8 9 10 11 12)))

 ;; Combine works, if you have numbers for matrix elements:
 (combine (matrix '(3 4) '(1 2 3 4 5 6 7 8 9 10 11 12)))
 ;; But it's disasterous for the following:
 (combine (matrix '(3 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12))))
 (matrix-to-list (matrix '(3 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12))))
"
  (let (
	(size (size matrix))
	newlist
	)
    (dolist
     (i (row-lists matrix))
     (setq newlist (append newlist i))
     )
    newlist
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 row-lists
;;
(defun row-lists(matrix)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; row-lists Doc:    Creation Date:3/31/97        Author: Andy Long
;;
;; Description/Arguments:

       (row-lists matrix)
  
	Returns the rows of a matrix as lists: this is to improve on
function row-list, which returns them as arrays.

;; Notes:

;; See Also: row-list

;; Examples:
 (row-lists (id 4))
 (row-lists (matrix (list 4 2) (rand.u 8)))
"
  (mapcar #'coerce (row-list matrix) (repeat 'list (first (size matrix))))
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Description for ID:
;;
(defun id (n)
  "
;; Documentation: ID (n)

Identity matrix of dimensions n by n.

 (id 3)
" 
  (identity-matrix n)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                normalize
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun normalize (v)
  "
;; Description: (normalize v)

;; Returns: the unit vector formed by dividing v by its norm (or signals an
   error, if v is the zero vector).

;; Examples:
 (normalize '(5 4))
 ;; causes an error:
 ;; (normalize '(0 0 0))
"
  (let ((l (norm v)))
    (if (= 0 l) 
	(error "in normalize: attempting to normalize the zero vector.")
      (/ v l))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; norm Doc:    Creation Date:2/4/96        Author: Andy Long
;;
;;
(defun norm(v &key (p 2))
  "
;; Description/Arguments:

        (norm v &key (p 2))
  
where
	v	- is a number, vector/list, or matrix.
	&key
	(p 2)	- means the 2 norm, i.e. the sqrt of the square of the vector
		  This should be a positive real, or 'infinity for vectors; for
		  matrices, only 1, 2 and infinity are permitted..
		  
;; Returns - the non-negative norm.

;; See Also: abs, inner-product

;; Examples:
 (norm #(1 2 3))

 (norm #(.5 .5) :p 'infinity)
 (norm #(.5 .5) :p 10)
 (norm #(.5 .5)) ;;  :p 2
 (norm #(.5 .5) :p 1)

 (norm '(1 #c(0 1)))					;; 1.4142135623730951
 (norm #2a((3 0 0)(0 2 0)(0 0 1)))			;; 3

 (norm #2a((1 2)(1.0001 2)) :p 1)
 (norm #2a((1 2 -1)(0 3 -1)(5 -1 1)) :p 'infinity)
 ;; but the following will cause an error (as only 1,2, and infinity norms are
 ;; defined for matrices):
 ;; (norm #2a((1 2)(1.0001 2)) :p 10)
"
  (setq p (if (numberp p) (abs p) -666))
  (cond
   ((numberp v) (abs v))
   ((matrixp v) 
    (cond
     ((= p 2) (first (second (svd v))))
     ((= p 1) (max (mapcar #'sum (column-lists (abs v)))))
     ((= p -666) (max (mapcar #'sum (row-lists (abs v)))))
     (t
      (error
       "in function norm.
Sorry: norms other than the 1, 2, or infinity-norm are only available
for vectors.")
      )
     )
    )
   (t
    (cond
     ((= p 2) (realpart (sqrt (inner-product v v))))
     ((= p 1) (sum (abs v)))
     ((= p -666) (max (abs v)))
     (t (^ (sum (^ (abs v) p)) (/ p)))
     )
    )
   )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 pseudo-inverse
;;
(defun pseudo-inverse(A &key (eps 1e-12) (verbose t) infinity)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          pseudo-inverse Doc:
;;
;; Author: Andy Long
;; Created: Thu Apr 24 16:05:57 CDT 1997
;;
;; Description/Arguments:

       (pseudo-inverse A &key (eps 1e-12) (verbose t) infinity)
  
	Calculates the pseudo-inverse of matrix A, using the singular value
decomposition. Once the sum of the first (largest to smallest) singular values
equals 1-eps or greater, the remainder of the singular values are considered to
be zero. Then, whereas the inverse of A would be given by

	V (Lambda)^{-1} U^T,

the pseudo-inverse of Lambda is given by entries of (1/lambda_i) for non-zero
lambda_i, and 0 otherwise:

	(pseudo-inverse A) = V (pseudo-inverse Lambda) U^T,

	If the singular value is less than eps, it is set to zero; if verbose
is true, a warning will be issued about the singularity of the matrix.

;; Notes: returns the pseudo-inverse, and by the values function a flag
;; indicating whether the matrix was numerically singular.

;; See Also: svd

;; Examples:
 (pseudo-inverse #2a((1 0) (0 0)))
 (pseudo-inverse #2a((1 0 0) (0 1 0) (1 1 0)))

 (def pseudo-inv (pseudo-inverse #2a((1 0 0) (0 1 0) (1 1 0))))
 (matmult pseudo-inv #2a((1 0 0) (0 1 0) (1 1 0)))

;; Here's an interesting example of the use of the pseudo-inverse:
 (title \"Skinning a Grid with the Pseudo-Inverse\")
 (func f(x y) sin(x)*sin(y))
 (def xs (rseq 0 1 3))
 (def ys xs) 
 (defun G(x) (mapcar (lambda(y) (f x y)) ys))
 (defun H(y) (mapcar (lambda(x) (f x y)) xs))
 (def pseudo-inv (pseudo-inverse (outer-product xs ys #'f)))
 (defun skin(x y) (matmult (g x) pseudo-inv (h y)))
 (defun diffy(x y) (- (skin x y) (f x y)))
 (spin-domain -pi pi -pi pi)
 (spin-points 21)
 (plot skin)
 (plot diffy)

 (def stuff 
  #2a(
     (  13.0000       97.0000       626.000       153.000       390.000    )
     (  97.0000       1139.00       4922.00       769.000       2620.00    )
     (  626.000       4922.00       33050.0       7201.00       15739.0    )
     (  153.000       769.000       7201.00       2293.00       4628.00    )
     (  390.000       2620.00       15739.0       4628.00       15062.0    )
    )
   )
 (mprint stuff)
 (mprint (pseudo-inverse stuff))
 (mprint (inverse stuff))
 (mprint (matmult stuff (pseudo-inverse stuff :eps 1e-12)))
 (mprint (matmult stuff (inverse stuff)))

 (def stuff #2a((0 #c(0 1))(#c(0 1) 0)))
 (svd stuff)
 (mprint (matmult stuff (pseudo-inverse stuff :eps 1e-12)))
"
  (let* (
	 (svd (svd A))
	 (u (first svd))
	 (lambda (second svd))
	 (v (third svd))
	 (len (length lambda))
	 (inv (repeat 0 len))
	 (sum 0)
	 ;; make sure it's real! The singular values may be returned as
	 ;; complex, for a complex matrix, even though their imaginary parts
	 ;; are all zero:
	 (total (sum lambda))
	 (k 0)
	 singular
	 lam
	 )
    (if (/= total 0)
	(do
	 ((i 1 (1+ i)))
	 ((> sum (- 1 eps)))
	 (setf
	  lam (elt lambda (1- i))
	  sum (+ sum (/ lam total))
	  (elt inv (1- i)) (/ lam))
	 (incf k)
	 )
      )
    (setq singular (/= k len))

    (if (and verbose singular)
	(format t
		"~%\tTo precision ~,4g the matrix is numerically singular,
with rank ~d out of ~d possible.~%" eps k len))
    
    (if (and singular infinity)
	(let ((n (where inv 0)))
	  (values
	   (list 
	    (select (column-lists u) n) lambda (select (column-lists v) n))
	   n))
      (values
       (matprod v (diagonal-to-matrix inv) (transpose (conjugate u)))
       singular
       )
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 where
;;
(defun where(variable value &optional (function #'equalp))
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; where Doc:    Creation Date:2/3/97        Author: Andy Long
;;
;; Description/Arguments:

       (where variable value &optional (function #'equalp))
  
Optional:
	function
		The function should be some comparison function for each
		element of the list. The default is equality

	I wrote this function to return the places where a list has a certain
value. It serves as a preliminary result to use with select.

;; Notes:

;; See Also: simul-keep-if, select

;; Examples:
 (def sex (list 1 1 0 1 1 0 0 0 0))
 (def bp (list 90 100 80 71 112 60 87 74 109))

 (where sex 1)
 ;; (0 1 3 4): another way to say that is with the not-equal function:

 (where sex 0 #'/=)
 ;; (0 1 3 4)

 (where sex 0)
 ;; (2 5 6 7 8)

 (select bp (where sex 0))
 ;; (80 60 87 74 109)

 (select bp (where sex 0))
 ;; (80 60 87 74 109)
"
  (second
   (simul-keep-if
    (if (listp variable) variable (coerce variable 'list))
    (iseq (length variable))
    #'(lambda(x) (apply function (list x value)))
    )
   )
  )
(defun simul-keep-if (a b &optional (test #'(lambda(x) (= x 0))))
  "
;; Description:
		simul-keep-if (a b &optional (test #'(lambda(x) (= x 0))))

	Note that the default is to keep zero elements.

;; See also: simul-remove-if

;; Examples:
 (simul-keep-if '(0 1 2) '(a b c)) ;; ((0) (A))
 (simul-keep-if '(0 1 2) '(a b c) #'(lambda(x) (< x 2))) ;; ((0 1) (A B))
"
  (let (
	(keepers
	 (which
	  (mapcar
	   (lambda(x) (setf x (if (funcall test x) t nil)))
	   (copy-list a)
	   ))))
    (list (select a keepers) (select b keepers))
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 column-lists
;;
(defun column-lists(matrix)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; column-lists Doc:    Creation Date:3/31/97        Author: Andy Long
;;
;; Description/Arguments:

       (column-lists matrix)
  
	Returns the columns of a matrix as lists: this is to improve on
function column-list, which returns them as arrays.

;; Notes:

;; See Also: column-list

;; Examples:
 (column-lists (id 4))
 (column-lists (matrix (list 4 2) (rand.u 8)))
"
  (mapcar
   #'coerce
   (column-list matrix)
   (repeat 'list (second (size matrix)))
   )
  )
;;
