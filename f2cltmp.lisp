; macros.l - all the basic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Confidential Trade Secret and Copyright (c) University of ;;;;;;;;
;;;;;;;;Waikato, Hamilton, New Zealand 1993 - all rights reserved;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(in-package :senac)
;; ael:
;(in-package "XLISP")
(in-package "USER")
;(setq *verbose* nil)
;; ael
(eval-when (compile load eval)
   (export '(do1 do! map-defvar double-cdr double-mapcar 
             fexport fproclaim fuse-package fin-package fdo arithmetic-if
             computed-goto assigned-goto
	     ;; ael
	     f2cl/
	     ;; lea
	     ;; ael
	     comment
	     oopen oopen1
	     cclose cclose1
	     rread rread1
	     clean-up-I/O
	     fformat fformat1
	     ;; lea
	     ))
   )

; macros:
;	rexpt
;	fexport
;	fproclaim
;	fuse-package 
;	fin-package
;	map-defvar
;	do1 
;	do!
;	double-cdr
;	putprop
;	defprop
;	array-cl
;	store-cl
;	apply!

;	rfref
;	rfset
;	fref
;	fset

;	while
;       fdo
;	reset-vble - a defun
;       arithmetic-if
;	computed-goto
;	assigned-goto
;	eqv
;	constant-list
;       Fortran intrinsic functions imax, dabs,...
;----------------------------------------------------------------------------

(eval-when (compile load eval) (proclaim '(special $verbose)))
;----------------------------------------------------------------------------
#+aclpc (defmacro rexpt (x y) `(realpart (expt ,x ,y)))
#-aclpc (defmacro rexpt (x y) `(expt ,x ,y))

(defmacro fexport (x) `(eval-when (compile load eval) (export ,x)))

(defmacro fproclaim (x) `(eval-when (compile load eval) (proclaim ,x)))

(defmacro fin-package (x)
  `(prog nil 
	 (defpackage ,x)
	 (in-package ,x)))

(defmacro fuse-package (x) `(eval-when (compile load eval) ,x))
;-------------------------------------------------------------------------

(defmacro apply! (fun args) (eval `(cons ,fun ,args)))

(defmacro map-defvar (&rest l)
   `(progn ,@(mapcar #'(lambda (x y) (list 'defvar x y))
                     (car (odd-even l)) 
                     (cadr (odd-even l)))))
;-----------------------------------------------------------------------------

(defmacro do! (var init step end &rest body)
   `(do ((,var ,init ,step)) (,end) ,@body))

; the body is an unquoted list of the terms of the actual body
(defmacro do1 (var end body)
   `(do ((,var 1 (1+ i))) ((> ,var ,end)) ,@body))

(defmacro double-cdr (lis)
   `(mapcar #'cdr (cdr ,lis)))

(defun putprop (a b c) (setf (get a c) b))

(defmacro defprop (sym prop ind)
  `(putprop ',sym ',prop ',ind))

(defmacro def (name body) `(defun ,name ,(cadr body) ,(caddr body)))

(defmacro array-cl (name type &rest dims)
 `(set ',name 
        (make-array ',(mapcar #'eval dims) 
           :element-type ,(cond ((equal type 'fixnum-block) ''integer)
                                ((equal type 'flonum-block)  ''flonum) ;###
                                ((equal type t) t)))))

(defmacro store-cl (name-indices val)
  `(setf (aref ,(car name-indices) ,@(cdr name-indices)) ,val))
;-----------------------------------------------------------------------------

(defmacro fref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices)))

(defmacro fset (a b) 
  `(setf (fref ,(second a) ,@(cddr a)) ,b))

(defmacro rfref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) (reverse indices))))

(defmacro rfset (a b) 
  `(setf (rfref ,(second a) ,@(cddr a)) ,b))


;----------------------------------------------------------------------------

#-aclpc (defmacro while (con &rest body)
            `(loop (if (not ,con) (return t)) ,@body))
;------------------------------------------------------------------

(defun comment (s) (when $verbose (princ s) (terpri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                fdo
;;
;; fdo has similar syntax as do except there will only be one do_vble
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fdo (do_vble_clause predicate_clause &rest body)
   `(prog* ((step ,(third (third do_vble_clause)))
            (iteration_count
               (max (truncate (+ (- ,(third (first predicate_clause))
                                    ,(second do_vble_clause)) step) step) 0)))
           ; initialise loop variable
           (setq ,(first do_vble_clause) ,(second do_vble_clause))
           loop
           (return
           (cond ; all iterations done
                 ((zerop iteration_count) nil)
                 ; execute loop, in/de-crement loop vble and decrement cntr
                 ,(cons 't
                        (append
                         (append body
                             `((setq ,(first do_vble_clause)
                                     ,(third do_vble_clause)
                                     iteration_count
                                            (1- iteration_count))))
                         '((go loop))))))))

;----------------------------------------------------------------------------
(defun constant-list (x n)
  (do  ((i 1 (1+ i)) 
        (ret nil (cons x ret))) 
       ((> i n) ret)))

;----------------------------------------------------------------------------

;; macro for a lisp equivalent of Fortran arithmetic IFs
(defmacro arithmetic-if (pred s1 s2 s3)
   `(cond ((< ,pred 0) ,s1)
          ((= ,pred 0) ,s2)
          (t ,s3)))

;; macro for a lisp equivalent of Fortran computed GOTOs
(defmacro computed-goto (tag-lst i)
   `(let ((tag ,(nth (1- (eval i)) tag-lst)))
       (if tag (go tag) nil)))

;; macro for a lisp equivalent of Fortran assigned GOTOs
(defmacro assigned-goto (i &optional tag-lst)
   `(if ,tag-lst
        (if (member ,i ,tag-lst) 
            (go ,i)
            (error "bad statement number in assigned goto"))
        (go ,i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      Additional macros: from f2cl, in the examples department. ael.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro for division 
(defmacro f2cl/ (x y)
  `(if (and (typep ,x 'fixnum) (typep ,y 'fixnum)) (floor ,x ,y) (/ ,x ,y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      Additional macros: from f2cl, in the examples department. ael.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;-----------------------------------------------------------------------------
; set up a list of intrinsic function names
;real xxx
(defvar intrinsic_function_names
  '(int ifix idint real float sngl dble cmplx ichar char aint dint
    anint dnint nint idnint iabs abs dabs cabs mod amod dmod isign sign dsign
    idim dim ddim dprod max max0 amax1 dmax1 amax0 amax1 min min0 amini dmini
    amini min1 len index lge lgt lle llt aimag conjg sqrt dsqrt csqrt 
    exp dexp cexp log alog dlog clog log10 alog10 dlog10 sin dsin csin
    cos dcos ccos tan dtan asin dasin acos dacos atan datan atan2 datan2
    sinh dsinh cosh dcosh tanh dtanh))

; some macros for intrinsic functions
(defmacro int (x)
   `(floor ,x))
(defmacro ifix (x)
   `(floor ,x))
(defmacro idfix (x)
   `(floor ,x))

(defmacro real_ (x)
   `(coerce ,x 'float))

(defmacro sngl (x)
   `(coerce ,x 'float))

(defmacro cmplx (x &optional y)
   `(complex ,x ,(if y y 0)))

(defmacro ichar (c)
   `(char-int ,c))
(defmacro fchar (i)  ;intrinsic function char
   `(char-int ,i))

(defmacro aint (x)
   `(float (truncate ,x)))
(defmacro dint (x)
   `(coerce (truncate ,x) 'double-float))
(defmacro anint (x)
   `(float (round ,x)))
(defmacro dnint (x)
   `(coerce (round ,x) 'double-float))
(defmacro nint (x)
   `(round ,x))
(defmacro idnint (x)
   `(round ,x))

#-aclpc (defmacro iabs (x) `(abs ,x))
(defmacro dabs (x)
   `(abs ,x))
(defmacro cabs (x)
   `(abs ,x))

(defmacro amod (x y)
  `(mod ,x ,y))
(defmacro dmod (x y)
  `(mod ,x ,y))

(defmacro sign (x y)
  `(if (>= ,y 0) (abs ,x) (- (abs ,x))))
(defmacro isign (x y)
  `(if (>= ,y 0) (abs ,x) (- (abs ,x))))
(defmacro dsign (x y)
  `(if (>= ,y 0) (abs ,x) (- (abs ,x))))

(defmacro idim (x y)
  `(abs (- ,x ,y)))
(defmacro dim (x y)
  `(abs (- ,x ,y)))
(defmacro ddim (x y)
  `(abs (- ,x ,y)))

(defmacro dprod (x y)
  `(coerce (* ,x ,y) `double-float))

(defmacro max0 (&rest x)
  `(funcall #'max ,@x))
(defmacro amax1 (&rest x)
  `(funcall #'max ,@x))
(defmacro dmax1 (&rest x)
  `(funcall #'max ,@x))
(defmacro amax0 (&rest x)
  `(round (funcall #'max ,@x)))
(defmacro max1 (&rest x)
  `(float (funcall #'max ,@x)))

(defmacro min0 (&rest x)
  `(funcall #'min ,@x))
(defmacro amin1 (&rest x)
  `(funcall #'min ,@x))
(defmacro dmin1 (&rest x)
  `(funcall #'min ,@x))
(defmacro amin0 (&rest x)
  `(round (funcall #'min ,@x)))
(defmacro min1 (&rest x)
  `(float (funcall #'min ,@x)))

(defmacro len (s)
   `(length ,s))

(defmacro index (s1 s2)
 (declare (ignore s1 s2))
   `(error "macro for intrinsic INDEX not yet implemented"))

(defmacro lge (s1 s2)
   `(string>= ,s1 ,s2))
(defmacro lgt (s1 s2)
   `(string> ,s1 ,s2))
(defmacro lle (s1 s2)
   `(string<= ,s1 ,s2))
(defmacro llt (s1 s2)
   `(string< ,s1 ,s2))

(defmacro aimag (c)
   `(imagpart ,c))
(defmacro conjg (c)
   `(conjugate ,c))

(defmacro dsqrt (x)
   `(sqrt ,x))
(defmacro csqrt (x)
   `(sqrt ,x))

(defmacro dexp (x)
   `(exp ,x))
(defmacro cexp (x)
   `(exp ,x))

(defmacro alog (x)
   `(log ,x))
(defmacro dlog (x)
   `(log ,x))
(defmacro clog (x)
   `(log ,x))
(defmacro alog10 (x)
   `(log ,x 10))
(defmacro dlog10 (x)
   `(log ,x 10))

(defmacro dsin (x)
   `(sin ,x))
(defmacro csin (x)
   `(sin ,x))

(defmacro dcos (x)
   `(cos ,x))
(defmacro ccos (x)
   `(cos ,x))

(defmacro dtan (x)
   `(tan ,x))
(defmacro ctan (x)
   `(tan ,x))

(defmacro dasin (x)
   `(asin ,x))
(defmacro dacos (x)
   `(acos ,x))
(defmacro datan (x)
   `(atan ,x))
(defmacro atan2 (x y)
   `(atan (/ ,x ,y)))
(defmacro datan2 (x y)
   `(atan (/ ,x ,y)))

(defmacro dsinh (x)
   `(sinh ,x))
(defmacro dcosh (x)
   `(cosh ,x))
(defmacro dtanh (x)
   `(tanh ,x))

;-----------------------------------------------------------------------------
; end of macros.l

;;ael
(defun comment (s) (when *verbose* (princ s) (terpri)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                oopen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oopen(I/O stream file &key (direction :input))
  "
;; Returns: changes the ith position of the I/O variable to a list containing
;; the stream, the filename, and the direction.
"
  `(oopen1 ,I/O ,stream ,file ,direction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                oopen1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun oopen1 (I/O stream file direction)
  "
	We're going to finally open a file!
1) If the stream is already open, we'll take a break to let folks know that
   that's a no-no;
2) if the direction is either output or delete, we'll open the file as output;
   if it is new or unknown, we'll open it as input, and only change our mind
   later (in fformat1) if the user tries to write to it.
"
  (let ((defined (first (aref I/O stream))))
    (if (and (streamp defined) (open-stream-p defined))
	(break (format nil "Can't open stream ~d as it's already open." file))
      (setf (aref I/O stream)
	    (list
	     (open file
		   :direction
		   (case direction
			 (:output :output)
			 (:delete :output)
			 (:unknown (if (probe-file file) :input :output))
			 (t :input)
			 )
		   )
	     file direction))
      )
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                clean-up-I/O
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clean-up-I/O (I/O)
  "
 Clean-up-I/O just closes any stream that happen to be open when all's said and
done. Close should have done that already, but if not we want to make sure to
get them closed so that we don't bleed files (easy to get a 'too many files
open' error).
"
  (dolist
   (i (iseq 1 99))
   (let ((stream-info (aref I/O i)))
     (if stream-info
	 (let (
	       (stream (elt stream-info 0))
	       (file (elt stream-info 1)) 
	       (direction (elt stream-info 2)) 
	       )
	   (if (streamp stream)
	       (let()
		 (close (first (aref I/O i)))
		 (if (eq direction :delete) 
		     (format
		      t "I should be deleting file ~a in clean-up-I/O.~%" file)
		   ;;(delete-file file)
		   )
		 )
	     )
	   )
       )
     )
   )
  )
;;lea
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                cclose
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cclose(I/O stream status)
  `(cclose1 ,I/O ,stream ,status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                cclose1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cclose1(I/O dest status)
  (let* (
	 (stream-info (aref I/O dest))
	 (stream (first stream-info))
	 (file (second stream-info))
	 (direction (third stream-info))
	 )
    (if stream 
	(let ()
	  (if status
	      (format t "did not delete file ~a~%" file)) ;; (delete-file file)
	  (close stream)
	  )
      (format t "Can't close unit ~d, as it doesn't appear to be open.~%" dest)
      )
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                rread
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rread(I/O stream)
  `(rread1 ,I/O ,stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                rread1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rread1(I/O stream)
  "
ael:	I added the eval, as I couldn't use 'pi', for example, in a program.
Of course, this is supposed to be fortran code, not lisp code, so we need to
think like a fortran code! I.e., I wouldn't type 'pi' to a fortran program. So
maybe I shouldn't bother....

	I won't. I've decided that we risk too much: for example, if a text
file contains the word pi on a line, it would be replaced by a numerical
approximation to pi: that could be really painful.
"
  (let* (
	 (stream (first (aref I/O (if (numberp stream) stream 0))))
	 (what
	  (if stream
	      (read stream nil 'EOF)
	    ;; otherwise we'll try standard in:
	    (read nil nil 'EOF)
	    )
	  )
	 )
    ;; (if (and (symbolp what) (boundp what)) (eval what) what)
    what
    )
  )
;;ael
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                fformat
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fformat (I/O dest cilist args)
  "
ael:
	I just replaced the format statements by fformat1, so that I can check
for numerical arguments, and treat them appropriately. 

	In the case of a ~4{ type argument, I need to add an additional layer
of listing.
"
  `(if (equal ,cilist '("~A~%")) 
       (and (mapcar #'(lambda (arg) (fformat1 ,I/O ,dest "~A " arg)) ,args)
;;ael	    (format ,dest "~%"))
	    (fformat1 ,I/O ,dest "~%" nil))
     ;; loop through directives, consume arguments
     (do ((directives ,cilist (cdr directives))
	  (arglist ,args arglist))
	 ((null directives))
	 (cond 
	  ((stringp (first directives))
	   ;;ael	(format ,dest (first directives)))
	   (fformat1 ,I/O  ,dest (first directives) nil))
	  #|
	  (t
	   ;; ael
	   ;; this is where we're screwed up: if we've got a repeater,
	   ;; then we're only pulling off the first of the whole list
	   ;; (which is supposed to be printed on a same line):
	   (fformat1 ,I/O ,dest
		     (car (first directives)) 
		     (first arglist))
	   (setq arglist (cdr arglist))
	   ;; so I'll modify things a little, opting for the additional case:
|#
	  ((and (listp (first directives))
		(char= #\~ (elt (first (first directives)) 0))
		(digit-char-p (elt (first (first directives)) 1))
		(search (list #\{) (coerce (first (first directives)) 'list))
		)
	   ;;ael - okay, we've got us a repeater! Now in fortran, you can give
	   ;;a repeater that pretends to need 4 values just 2 values, say, so
	   ;;we have to be ready to cut this down.
	   (let* (
		  (string (first (first directives)))
		  (wheres-a-brace (search (list #\{) (coerce string 'list)))
		  (how-many (read-from-string
			     (subseq string 1 wheres-a-brace)))
		  (len-args (length arglist))
		  (num-args (min how-many len-args))
		  (string (strcat
			   "~" (num-to-string num-args)
			   (subseq string wheres-a-brace)))
		  )
	     (fformat1 ,I/O ,dest
		       string
		       (select arglist (iseq num-args)))
	     (setq arglist 
		   (if (/= num-args len-args)
		       (select arglist (iseq num-args (1- len-args)))))
	     )
	   )
	  (t
	   ;; this is still the default if we don't have a ~4{ or such format:
	   (fformat1 ,I/O ,dest
		     (car (first directives)) 
		     (first arglist))
	   (setq arglist (cdr arglist))
	  ;;lea
	  )
	  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                fformat1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fformat1 (I/O dest directive arg)
  "
ael:
	The print statement in the original definition

 (defun fformat1 (dest directive arg)
   (if (arrayp arg)
       (print arg)
     (format dest directive arg)))

above was really pernicious!
"
  (let* (
	 (is-stream (if (member dest (iseq 1 99)) (aref I/O dest)))
	 (file (second is-stream))
	 (direction (third is-stream))
	 (stream
	  (cond
	   ((streamp (first is-stream)) (first is-stream))
	   (t t)))
	 )
    (case 
     direction
     (:input
      (error
       (format nil "Attempt to write to input file ~a, unit ~d" file dest)))
     (:unknown
      (format t "Re-opening file ~a with output status in fformat1.~%" file)
      (close stream)
      (setf (aref I/O dest) 
	    (list (setq stream (open file :direction :output)) dest :output))
      (if (and (arrayp arg) (not (stringp arg)))
	  (print arg stream) ;; ael - talk about bailing!
	(format stream directive arg)
	))
     (t ;; it's already output!
      (if (and (arrayp arg) (not (stringp arg)))
	  (print arg stream) ;; ael - talk about bailing!
	(format stream directive arg)
	)
      )
     )
    )
  )
;;ael
; end of my bunch of macros

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;              Numerical Recipes functions depending on these:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
"Numerical Recipes: The Art of Scientific Computing", Press et al., Cambridge
University Press, 1986.

	Okay, I could get the elmhes, etc. functions to compile if I replace
all fdos with dos. That, however, caused the code to barf on a couple of
examples that work with the uncompiled code! Yuck. 

|#
(eval-when (compile load eval)
	   (export '(elmhes hqr tqli tred2 balanc eigsrt jacobi))
	   (export '(julian caldate fullmoon day-of-the-week
			    gamma-incomplete
			    )))

(defun elmhes (a n np) (declare (type (simple-array double-float (* *)) a))
  (declare (type fixnum n)) (declare (type fixnum np))
  (prog
   ((y 0.0d0) (j 0) (i 0) (x 0.0d0) (m 0)) (declare (type double-float y))
   (declare (type fixnum j)) (declare (type fixnum i))
   (declare (type double-float x)) (declare (type fixnum m))
   (cond
    ((> n 2)
     (do
      ((m 2 (+ m 1)))
      ((> m (+ n (- 1))) nil)
      (tagbody
       (setf x 0.0) (setf i m)
       (do ((j m (+ j 1))) ((> j n) nil)
	    (tagbody
	     (cond
	      ((> (abs (fref a j (+ m (- 1)))) (abs x))
	       (setf x (fref a j (+ m (- 1)))) (setf i j)
	       ))))
       (cond
	((/= i m)
	 (do ((j (+ m (- 1)) (+ j 1))) ((> j n) nil)
	      (tagbody (setf y (fref a i j)) (fset (fref a i j) (fref a m j))
		       (fset (fref a m j) y)
		       ))
	 (do ((j 1 (+ j 1))) ((> j n) nil)
	      (tagbody (setf y (fref a j i)) (fset (fref a j i) (fref a j m))
		       (fset (fref a j m) y)
		       ))))
       (cond
	((/= x 0.0)
	 (do
	  ((i (+ m 1) (+ i 1))) ((> i n) nil)
	  (tagbody
	   (setf y (fref a i (+ m (- 1))))
	   (cond
	    ((/= y 0.0) (setf y (/ y x)) (fset (fref a i (+ m (- 1))) y)
	     (do ((j m (+ j 1))) ((> j n) nil)
		  (tagbody
		   (fset (fref a i j) (+ (fref a i j) (* (* -1 y) (fref a m j))))
		   ))
	     (do ((j 1 (+ j 1))) ((> j n) nil)
		  (tagbody
		   (fset (fref a j m) (+ (fref a j m) (* y (fref a j i)))))
		  )))))))))))
   (return (values a n np))
   ))

(defun hqr (a n np wr wi &key (itmax 30))
  "
	hqr is the lisp translation of a fortran program based on an algorithm
taken from Numerical Recipes.
"
 (declare (type (simple-array double-float (* *)) a))
 (declare (type fixnum n)) (declare (type fixnum np))
 (declare (type (simple-array double-float (*)) wr))
 (declare (type (simple-array double-float (*)) wi))
 (prog
  ((k 0) (v 0.0d0) (u 0.0d0) (r 0.0d0) (m 0) (z 0.0d0) (q 0.0d0) (p 0.0d0)
   (w 0.0d0) (y 0.0d0) (x 0.0d0) (s 0.0d0) (l 0) (its 0) (t_ 0.0d0) (nn 0)
   (j 0) (i 0) (anorm 0.0d0)
  )
  (declare (type fixnum k)) (declare (type double-float v))
  (declare (type double-float u)) (declare (type double-float r))
  (declare (type fixnum m)) (declare (type double-float z))
  (declare (type double-float q)) (declare (type double-float p))
  (declare (type double-float w)) (declare (type double-float y))
  (declare (type double-float x)) (declare (type double-float s))
  (declare (type fixnum l)) (declare (type fixnum its))
  (declare (type double-float t_)) (declare (type fixnum nn))
  (declare (type fixnum j)) (declare (type fixnum i))
  (declare (type double-float anorm)) (setf anorm (abs (fref a 1 1)))
  (fdo (i 2 (+ i 1)) ((> i n) nil)
   (tagbody
    (fdo (j (+ i (- 1)) (+ j 1)) ((> j n) nil)
     (tagbody (setf anorm (+ anorm (abs (fref a i j)))))
     )))
  (setf nn n)
  (setf t_ 0.0)
  label1
  (cond
   ((>= nn 1)
    (tagbody
     (setf its 0)
     label2
     (fdo
      (l nn (+ l (- 1)))
;;
;;ael      ((> l 2) nil)
;;
      ((< l 2) nil)
;;
;;  I can't believe this error! Did I make this, or was this in the original
;;  code?? Looks like all the fdo's have their inequalities in the opposite
;;  sense. This might be a general problem with the fdo...
;;
      (tagbody
       (setf s (+ (abs (fref a (+ l (- 1)) (+ l (- 1)))) (abs (fref a l l))))
       (if (= s 0.0) (setf s anorm))
       (if (= (+ (abs (fref a l (+ l (- 1)))) s) s) (go label3))
       ))
     (setf l 1)
     label3
     (setf x (fref a nn nn))
     (cond
      ((= l nn)
       (fset (fref wr nn) (+ x t_))
       (fset (fref wi nn) 0.0)
       (setf nn (+ nn (- 1)))
       )
      (t 
       (setf y (fref a (+ nn (- 1)) (+ nn (- 1))))
       (setf w (* (fref a nn (+ nn (- 1))) (fref a (+ nn (- 1)) nn)))
       (cond
        ((= l (+ nn (- 1)))
	 (setf p (* 0.5 (+ y (- x))))
         (setf q (+ (expt p 2) w))
	 (setf z (sqrt (abs q)))
	 (setf x (+ x t_))
         (cond
          ((>= q 0.0)
	   (setf z (+ p (sign z p)))
	   (fset (fref wr nn) (+ x z))
           (fset (fref wr (+ nn (- 1))) (fref wr nn))
           (if (/= z 0.0) (fset (fref wr nn) (+ x (/ (* -1 w) z))))
           (fset (fref wi nn) 0.0)
	   (fset (fref wi (+ nn (- 1))) 0.0)
	   )
          (t
	   (fset (fref wr nn) (+ x p))
           (fset (fref wr (+ nn (- 1))) (fref wr nn))
	   (fset (fref wi nn) z)
           (fset (fref wi (+ nn (- 1))) (- z))
	   ))
         (setf nn (+ nn (- 2)))
	 )
        (t
         (tagbody
	  (if (= its itmax) (error "too many iterations"))
	  (cond
           ((or (= its 10) (= its 20))
	    (setf t_ (+ t_ x))
            (fdo (i 1 (+ i 1)) ((> i nn) nil)
		 (tagbody (fset (fref a i i) (+ (fref a i i) (- x))))
		 )
            (setf s (+ (abs (fref a nn (+ nn (- 1))))
		       (abs (fref a (+ nn (- 1)) (+ nn (- 2))))
		       ))
            (setf x (* 0.75 s))
	    (setf y x)
            (setf w (* -0.4375 (expt s 2)))
	    ))
          (setf its (+ its 1))
;;
;;ael          (fdo ((m (+ nn (- 2)) (+ m (- 1)))) ((> m l) nil)
;;
          (fdo (m (+ nn (- 2)) (+ m (- 1))) ((< m l) nil)
;;
;;ael fdo, again!
;;
           (tagbody (setf z (fref a m m)) (setf r (+ x (- z)))
            (setf s (+ y (- z)))
            (setf p
             (+ (/ (+ (* r s) (- w)) (fref a (+ m 1) m)) (fref a m (+ m 1)))
            )
            (setf q (+ (+ (+ (fref a (+ m 1) (+ m 1)) (- z)) (- r)) (- s)))
            (setf r (fref a (+ m 2) (+ m 1)))
            (setf s (+ (+ (abs p) (abs q)) (abs r))) (setf p (/ p s))
            (setf q (/ q s))
	    (setf r (/ r s)) (if (= m l) (go label4))
            (setf u (* (abs (fref a m (+ m (- 1)))) (+ (abs q) (abs r))))
            (setf v
             (* (abs p)
              (+ (+ (abs (fref a (+ m (- 1)) (+ m (- 1)))) (abs z))
               (abs (fref a (+ m 1) (+ m 1)))
            )))
            (if (= (+ u v) v) (go label4))
          ))
          label4
          (fdo (i (+ m 2) (+ i 1)) ((> i nn) nil)
           (tagbody (fset (fref a i (+ i (- 2))) 0.0)
            (if (/= i (+ m 2)) (fset (fref a i (+ i (- 3))) 0.0))
	    ))
          (fdo (k m (+ k 1)) ((> k (+ nn (- 1))) nil)
           (tagbody
            (cond
             ((/= k m) (setf p (fref a k (+ k (- 1))))
              (setf q (fref a (+ k 1) (+ k (- 1)))) (setf r 0.0)
              (if (/= k (+ nn (- 1))) (setf r (fref a (+ k 2) (+ k (- 1)))))
              (setf x (+ (+ (abs p) (abs q)) (abs r)))
              (cond
               ((/= x 0.0) (setf p (/ p x)) (setf q (/ q x)) (setf r (/ r x)))
            )))
            (setf s (sign (sqrt (+ (+ (expt p 2) (expt q 2)) (expt r 2))) p))
            (cond
             ((/= s 0.0)
              (cond
               ((= k m)
                (if (/= l m)
                 (fset (fref a k (+ k (- 1))) (- (fref a k (+ k (- 1)))))
               ))
               (t (fset (fref a k (+ k (- 1))) (* (* -1 s) x)))
              )
              (setf p (+ p s)) (setf x (/ p s)) (setf y (/ q s))
              (setf z (/ r s)) (setf q (/ q p)) (setf r (/ r p))
              (fdo (j k (+ j 1)) ((> j nn) nil)
               (tagbody (setf p (+ (fref a k j) (* q (fref a (+ k 1) j))))
                (cond
                 ((/= k (+ nn (- 1))) (setf p (+ p (* r (fref a (+ k 2) j))))
                  (fset (fref a (+ k 2) j)
                   (+ (fref a (+ k 2) j) (* (* -1 p) z))
                )))
                (fset (fref a (+ k 1) j) (+ (fref a (+ k 1) j) (* (* -1 p) y)))
                (fset (fref a k j) (+ (fref a k j) (* (* -1 p) x)))
              ))
              (fdo (i l (+ i 1)) ((> i (min nn (+ k 3))) nil)
               (tagbody
                (setf p (+ (* x (fref a i k)) (* y (fref a i (+ k 1)))))
                (cond
                 ((/= k (+ nn (- 1))) (setf p (+ p (* z (fref a i (+ k 2)))))
                  (fset (fref a i (+ k 2))
                   (+ (fref a i (+ k 2)) (* (* -1 p) r))
                )))
                (fset (fref a i (+ k 1)) (+ (fref a i (+ k 1)) (* (* -1 p) q)))
                (fset (fref a i k) (+ (fref a i k) (- p)))
          ))))))
          (go label2)
     )))))
     (go label1)
  )))
  (return (values a n np wr wi))
))

(defun tqli (d e n np z &key (itmax 30))
 (declare (type (simple-array double-float (*)) d))
 (declare (type (simple-array double-float (*)) e)) (declare (type fixnum n))
 (declare (type fixnum np))
 (declare (type (simple-array double-float (* *)) z))
 (prog
  ((k 0) (b 0.0d0) (f 0.0d0) (p 0.0d0) (c 0.0d0) (s 0.0d0) (r 0.0d0) (g 0.0d0)
   (dd 0.0d0) (m 0) (iter 0) (l 0) (i 0)
  )
  (declare (type fixnum k)) (declare (type double-float b))
  (declare (type double-float f)) (declare (type double-float p))
  (declare (type double-float c)) (declare (type double-float s))
  (declare (type double-float r)) (declare (type double-float g))
  (declare (type double-float dd)) (declare (type fixnum m))
  (declare (type fixnum iter)) (declare (type fixnum l))
  (declare (type fixnum i))
  (cond
   ((> n 1)
    (fdo (i 2 (+ i 1)) ((> i n) nil)
     (tagbody (fset (fref e (+ i (- 1))) (fref e i)))
    )
    (fset (fref e n) 0.0)
    (fdo (l 1 (+ l 1)) ((> l n) nil)
     (tagbody (setf iter 0) label1
      (fdo (m l (+ m 1)) ((> m (+ n (- 1))) nil)
       (tagbody (setf dd (+ (abs (fref d m)) (abs (fref d (+ m 1)))))
        (if (= (+ (abs (fref e m)) dd) dd) (go label2))
      ))
      (setf m n) label2
      (cond
       ((/= m l) (if (= iter itmax) (error "too many iterations"))
        (setf iter (+ iter 1))
        (setf g (/ (+ (fref d (+ l 1)) (- (fref d l))) (* 2.0 (fref e l))))
        (setf r (sqrt (+ (expt g 2) 1.0)))
        (setf g
         (+ (+ (fref d m) (- (fref d l))) (/ (fref e l) (+ g (sign r g))))
        )
        (setf s 1.0) (setf c 1.0) (setf p 0.0)
;;;
;;; ael
;;;
;;;        (fdo ((i (+ m (- 1)) (+ i (- 1)))) ((> i l) nil)
        (fdo (i (+ m (- 1)) (+ i (- 1))) ((< i l) nil)
;;; ael
         (tagbody (setf f (* s (fref e i))) (setf b (* c (fref e i)))
          (cond
           ((>= (abs f) (abs g)) (setf c (/ g f))
            (setf r (sqrt (+ (expt c 2) 1.0))) (fset (fref e (+ i 1)) (* f r))
            (setf s (/ 1.0 r)) (setf c (* c s))
           )
           (t (setf s (/ f g)) (setf r (sqrt (+ (expt s 2) 1.0)))
            (fset (fref e (+ i 1)) (* g r)) (setf c (/ 1.0 r)) (setf s (* s c))
          ))
          (setf g (+ (fref d (+ i 1)) (- p)))
          (setf r (+ (* (+ (fref d i) (- g)) s) (* (* 2.0 c) b)))
          (setf p (* s r)) (fset (fref d (+ i 1)) (+ g p))
          (setf g (+ (* c r) (- b)))
          (fdo (k 1 (+ k 1)) ((> k n) nil)
           (tagbody (setf f (fref z k (+ i 1)))
            (fset (fref z k (+ i 1)) (+ (* s (fref z k i)) (* c f)))
            (fset (fref z k i) (+ (* c (fref z k i)) (* (* -1 s) f)))
        ))))
        (fset (fref d l) (+ (fref d l) (- p))) (fset (fref e l) g)
        (fset (fref e m) 0.0) (go label1)
  ))))))
  (return (values d e n np z))
))

(defun tred2 (a n np d e &key (evec t))
 (declare (type (simple-array double-float (* *)) a))
 (declare (type fixnum n)) (declare (type fixnum np))
 (declare (type (simple-array double-float (*)) d))
 (declare (type (simple-array double-float (*)) e))
 (prog
  ((hh 0.0d0) (j 0) (g 0.0d0) (f 0.0d0) (k 0) (scale 0.0d0) (h 0.0d0) (l 0)
   (i 0)
  )
  (declare (type double-float hh)) (declare (type fixnum j))
  (declare (type double-float g)) (declare (type double-float f))
  (declare (type fixnum k)) (declare (type double-float scale))
  (declare (type double-float h)) (declare (type fixnum l))
  (declare (type fixnum i))
  (cond
   ((> n 1)
;;;ael    (fdo ((i n (+ i (- 1)))) ((> i 2) nil)
    (fdo (i n (+ i (- 1))) ((< i 2) nil)
;;;ael
     (tagbody (setf l (+ i (- 1))) (setf h 0.0) (setf scale 0.0)
      (cond
       ((> l 1)
        (fdo (k 1 (+ k 1)) ((> k l) nil)
         (tagbody (setf scale (+ scale (abs (fref a i k)))))
        )
        (cond ((= scale 0.0) (fset (fref e i) (fref a i l)))
         (t
          (fdo (k 1 (+ k 1)) ((> k l) nil)
           (tagbody (fset (fref a i k) (/ (fref a i k) scale))
            (setf h (+ h (expt (fref a i k) 2)))
          ))
          (setf f (fref a i l)) (setf g (- (sign (sqrt h) f)))
          (fset (fref e i) (* scale g)) (setf h (+ h (* (* -1 f) g)))
          (fset (fref a i l) (+ f (- g))) (setf f 0.0)
          (fdo (j 1 (+ j 1)) ((> j l) nil)
           (tagbody (if evec (fset (fref a j i) (/ (fref a i j) h)))
                    (setf g 0.0)
            (fdo (k 1 (+ k 1)) ((> k j) nil)
             (tagbody (setf g (+ g (* (fref a j k) (fref a i k)))))
            )
            (cond
             ((> l j)
              (fdo (k (+ j 1) (+ k 1)) ((> k l) nil)
               (tagbody (setf g (+ g (* (fref a k j) (fref a i k)))))
            )))
            (fset (fref e j) (/ g h))
            (setf f (+ f (* (fref e j) (fref a i j))))
          ))
          (setf hh (/ f (+ h h)))
          (fdo (j 1 (+ j 1)) ((> j l) nil)
           (tagbody (setf f (fref a i j))
            (setf g (+ (fref e j) (* (* -1 hh) f))) (fset (fref e j) g)
            (fdo (k 1 (+ k 1)) ((> k j) nil)
             (tagbody
              (fset (fref a j k)
               (+ (+ (fref a j k) (* (* -1 f) (fref e k)))
                (* (* -1 g) (fref a i k))
       )))))))))
       (t (fset (fref e i) (fref a i l)))
      )
      (fset (fref d i) h)
  ))))
  (fset (fref d 1) 0.0) (fset (fref e 1) 0.0)
  (fdo (i 1 (+ i 1)) ((> i n) nil)
   (tagbody (if evec (tagbody (setf l (+ i (- 1)))
    (cond
     ((/= (fref d i) 0.0)
      (fdo (j 1 (+ j 1)) ((> j l) nil)
       (tagbody (setf g 0.0)
        (fdo (k 1 (+ k 1)) ((> k l) nil)
         (tagbody (setf g (+ g (* (fref a i k) (fref a k j)))))
        )
        (fdo (k 1 (+ k 1)) ((> k l) nil)
         (tagbody
          (fset (fref a k j) (+ (fref a k j) (* (* -1 g) (fref a k i))))
    ))))))))
    (fset (fref d i) (fref a i i)) (if evec (tagbody (fset (fref a i i) 1.0)
    (cond
     ((>= l 1)
      (fdo (j 1 (+ j 1)) ((> j l) nil)
       (tagbody (fset (fref a i j) 0.0) (fset (fref a j i) 0.0))
  )))))))
  (return (values a n np d e))
))

(defun balanc (a n np &key (radix 2.0) (sqrdx 4.0))
 (declare (type (simple-array double-float (* *)) a)) (declare (type fixnum n))
 (declare (type fixnum np)) (declare (type double-float radix))
 (declare (type double-float sqrdx))
 (prog ((s 0.0d0) (f 0.0d0) (g 0.0d0) (j 0) (r 0.0d0) (c 0.0d0) (i 0) (last 0))
  (declare (type double-float s)) (declare (type double-float f))
  (declare (type double-float g)) (declare (type fixnum j))
  (declare (type double-float r)) (declare (type double-float c))
  (declare (type fixnum i)) (declare (type fixnum last)) label1 (setf last 1)
  (fdo (i 1 (+ i 1)) ((> i n) nil)
   (tagbody (setf c 0.0) (setf r 0.0)
    (fdo (j 1 (+ j 1)) ((> j n) nil)
     (tagbody
      (cond
       ((/= j i) (setf c (+ c (abs (fref a j i))))
        (setf r (+ r (abs (fref a i j))))
    ))))
    (cond
     ((and (/= c 0.0) (/= r 0.0))
      (tagbody (setf g (/ r radix)) (setf f 1.0) (setf s (+ c r)) label2
       (cond ((< c g) (setf f (* f radix)) (setf c (* c sqrdx)) (go label2)))
       (setf g (* r radix)) label3
       (cond ((> c g) (setf f (/ f radix)) (setf c (/ c sqrdx)) (go label3)))
       (cond
        ((< (/ (+ c r) f) (* 0.95 s)) (setf last 0) (setf g (/ 1.0 f))
         (fdo (j 1 (+ j 1)) ((> j n) nil)
          (tagbody (fset (fref a i j) (* (fref a i j) g)))
         )
         (fdo (j 1 (+ j 1)) ((> j n) nil)
          (tagbody (fset (fref a j i) (* (fref a j i) f)))
  ))))))))
  (if (= last 0) (go label1)) (return (values a n np))
))

(defun eigsrt (d v n np) (declare (type (simple-array double-float (*)) d))
 (declare (type (simple-array double-float (* *)) v)) (declare (type fixnum n))
 (declare (type fixnum np))
 (prog ((j 0) (p 0.0d0) (k 0) (i 0)) (declare (type fixnum j))
  (declare (type double-float p)) (declare (type fixnum k))
  (declare (type fixnum i))
  (fdo (i 1 (+ i 1)) ((> i (+ n (- 1))) nil)
   (tagbody (setf k i) (setf p (fref d i))
    (fdo (j (+ i 1) (+ j 1)) ((> j n) nil)
     (tagbody (cond ((>= (fref d j) p) (setf k j) (setf p (fref d j)))))
    )
    (cond
     ((/= k i) (fset (fref d k) (fref d i)) (fset (fref d i) p)
      (fdo (j 1 (+ j 1)) ((> j n) nil)
       (tagbody (setf p (fref v j i)) (fset (fref v j i) (fref v j k))
        (fset (fref v j k) p)
  ))))))
  (return (values d v n np))
))

(defun jacobi (a n np d v nrot &key (nmax 100))
 (declare (type (simple-array double-float (* *)) a)) (declare (type fixnum n))
 (declare (type fixnum np)) (declare (type (simple-array double-float (*)) d))
 (declare (type (simple-array double-float (* *)) v))
 (declare (type fixnum nrot)) (declare (type fixnum nmax))
 (prog
  ((b (make-array nmax :element-type 'double-float))
   (z (make-array nmax :element-type 'double-float)) (j 0) (tau 0.0d0)
   (s 0.0d0) (c 0.0d0) (theta 0.0d0) (t_ 0.0d0) (h 0.0d0) (g 0.0d0)
   (tresh 0.0d0) (sm 0.0d0) (i 0) (iq 0) (ip 0)
  )
  (declare (type (simple-array double-float (*)) b))
  (declare (type (simple-array double-float (*)) z)) (declare (type fixnum j))
  (declare (type double-float tau)) (declare (type double-float s))
  (declare (type double-float c)) (declare (type double-float theta))
  (declare (type double-float t_)) (declare (type double-float h))
  (declare (type double-float g)) (declare (type double-float tresh))
  (declare (type double-float sm)) (declare (type fixnum i))
  (declare (type fixnum iq)) (declare (type fixnum ip))
  (fdo (ip 1 (+ ip 1)) ((> ip n) nil)
   (tagbody
    (fdo (iq 1 (+ iq 1)) ((> iq n) nil) (tagbody (fset (fref v ip iq) 0.0)))
    (fset (fref v ip ip) 1.0)
  ))
  (fdo (ip 1 (+ ip 1)) ((> ip n) nil)
   (tagbody (fset (fref b ip) (fref a ip ip)) (fset (fref d ip) (fref b ip))
    (fset (fref z ip) 0.0)
  ))
  (setf nrot 0)
  (fdo (i 1 (+ i 1)) ((> i 50) nil)
   (tagbody (setf sm 0.0)
    (fdo (ip 1 (+ ip 1)) ((> ip (+ n (- 1))) nil)
     (tagbody
      (fdo (iq (+ ip 1) (+ iq 1)) ((> iq n) nil)
       (tagbody (setf sm (+ sm (abs (fref a ip iq)))))
    )))
    (if (= sm 0.0) (go end_label))
    (cond ((< i 4) (setf tresh (/ (* 0.2 sm) (expt n 2)))) (t (setf tresh 0.0))
    )
    (fdo (ip 1 (+ ip 1)) ((> ip (+ n (- 1))) nil)
     (tagbody
      (fdo (iq (+ ip 1) (+ iq 1)) ((> iq n) nil)
       (tagbody (setf g (* 100.0 (abs (fref a ip iq))))
        (cond
         ((and (> i 4) (= (+ (abs (fref d ip)) g) (abs (fref d ip)))
           (= (+ (abs (fref d iq)) g) (abs (fref d iq)))
          )
          (fset (fref a ip iq) 0.0)
         )
         ((> (abs (fref a ip iq)) tresh)
          (setf h (+ (fref d iq) (- (fref d ip))))
          (cond ((= (+ (abs h) g) (abs h)) (setf t_ (/ (fref a ip iq) h)))
           (t (setf theta (/ (* 0.5 h) (fref a ip iq)))
            (setf t_ (/ 1.0 (+ (abs theta) (sqrt (+ 1.0 (expt theta 2))))))
            (if (< theta 0.0) (setf t_ (- t_)))
          ))
          (setf c (/ 1.0 (sqrt (+ 1 (expt t_ 2))))) (setf s (* t_ c))
          (setf tau (/ s (+ 1.0 c))) (setf h (* t_ (fref a ip iq)))
          (fset (fref z ip) (+ (fref z ip) (- h)))
          (fset (fref z iq) (+ (fref z iq) h))
          (fset (fref d ip) (+ (fref d ip) (- h)))
          (fset (fref d iq) (+ (fref d iq) h)) (fset (fref a ip iq) 0.0)
          (fdo (j 1 (+ j 1)) ((> j (+ ip (- 1))) nil)
           (tagbody (setf g (fref a j ip)) (setf h (fref a j iq))
            (fset (fref a j ip) (+ g (* (* -1 s) (+ h (* g tau)))))
            (fset (fref a j iq) (+ h (* s (+ g (* (* -1 h) tau)))))
          ))
          (fdo (j (+ ip 1) (+ j 1)) ((> j (+ iq (- 1))) nil)
           (tagbody (setf g (fref a ip j)) (setf h (fref a j iq))
            (fset (fref a ip j) (+ g (* (* -1 s) (+ h (* g tau)))))
            (fset (fref a j iq) (+ h (* s (+ g (* (* -1 h) tau)))))
          ))
          (fdo (j (+ iq 1) (+ j 1)) ((> j n) nil)
           (tagbody (setf g (fref a ip j)) (setf h (fref a iq j))
            (fset (fref a ip j) (+ g (* (* -1 s) (+ h (* g tau)))))
            (fset (fref a iq j) (+ h (* s (+ g (* (* -1 h) tau)))))
          ))
          (fdo (j 1 (+ j 1)) ((> j n) nil)
           (tagbody (setf g (fref v j ip)) (setf h (fref v j iq))
            (fset (fref v j ip) (+ g (* (* -1 s) (+ h (* g tau)))))
            (fset (fref v j iq) (+ h (* s (+ g (* (* -1 h) tau)))))
          ))
          (setf nrot (+ nrot 1))
    ))))))
    (fdo (ip 1 (+ ip 1)) ((> ip n) nil)
     (tagbody (fset (fref b ip) (+ (fref b ip) (fref z ip)))
      (fset (fref d ip) (fref b ip)) (fset (fref z ip) 0.0)
  ))))
  (error "50 iterations should never happen") (go end_label) end_label
  (return (values a n np d v nrot))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Starting to produce my own lsp programs based on numrec routines....
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 caldate
;;
(defun caldate (julian &key (igreg 2299161))
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          caldate Doc:
;;
;; Author: Andy Long (using f2cl and numerical recipes)
;; Created: Fri Dec 5 00:07:50 1997
;;
;; Description/Arguments:

       function caldate (julian &key (igreg 2299161))
  
;; Returns:

;; Notes:
	'inverse of the function Julian. Here julian is input as a julian day
number, and the routine outputs the month, day and year on which the specified
Julian day started at noon.'

;; See Also: julian, fullmoon

;; Examples:
 (caldate 2299161) ;; (10 15 1582) - the Gregorian calender was adopted on
		   ;; Oct. 15, 1582
 (caldate 2440000) ;; (5 23 1968)

"
  (declare (type fixnum julian))
  (declare (type fixnum igreg))
  (prog ((je 0)
	 iyyy id mm
	 (jd 0)
	 (jc 0)
	 (jb 0)
	 (ja 0)
	 (jalpha 0))
	(cond
	 ((>= julian igreg)
	  (setf
	   jalpha
	   (int (f2cl/ (+ (+ julian (- 1867216)) (- 0.25)) 36524.25)))
				      
	  (setf ja (+ (+ (+ julian 1) jalpha) 
		      (- (int (* 0.25 jalpha))))))
	 (t (setf ja julian)))

	(setf jb (+ ja 1524))
	(setf jc (int (+ 6680.0 (f2cl/ (+ (+ jb (- 2439870))
					  (- 122.1)) 365.25))))
	(setf jd (+ (* 365 jc) (int (* 0.25 jc))))
	(setf je (int (f2cl/ (+ jb (- jd)) 30.6001)))
	(setf id (+ (+ jb (- jd)) (- (int (* 30.6001 je)))))
		    
	(setf mm (+ je (- 1)))
	(if (> mm 12) (setf mm (+ mm (- 12))))
	    
	(setf iyyy (+ jc (- 4715)))
	(if (> mm 2) (setf iyyy (+ iyyy (- 1))))
	(if (<= iyyy 0) (setf iyyy (+ iyyy (- 1))))

	(return (list mm id iyyy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 julian
;;
(defun julian (mm id iyyy &key (igreg (+ 15 (* 31 (+ 10 (* 12 1582))))))
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          julian Doc:
;;
;; Author: Andy Long (using f2cl and numerical recipes)
;; Created: Thu Dec 4 23:57:02 1997
;;
;; Description/Arguments: 

	function julian (mm id iyyy &key (igreg 15))

;; Returns: values julian-day

;; Notes: after julday
	Julian returns the julian day number which begins at noon of the
calender date month, day, and year (all integers). Positive year signifies A.D,
negative  B.C. Remember that the year after 1 B.C. was 1 A.D.

;; See Also: fullmoon, caldate

;; Examples:
 (julian 10 15 1582)	;; the Gregorian calendar was adopted on this date
 (julian 12 4 1997)
 (julian 5 23 1968)	;; 2440000
"
  (declare (type fixnum iyyy))
  (declare (type fixnum id))
  (declare (type fixnum mm))
  (declare (type fixnum igreg))
  (prog ((ja 0)
	 (julday 0)
	 (jm 0)
	 (jy 0)
	 )
	(declare (type fixnum ja))
	(declare (type fixnum julday))
	(declare (type fixnum jm))
	(declare (type fixnum jy))
	(if (= iyyy 0)
	    (error "THERE IS NO YEAR ZERO."))
	(if (< iyyy 0)
	    (setf iyyy (+ iyyy 1)))
	(cond ((> mm 2)
	       (setf jy iyyy)
	       (setf jm (+ mm 1)))
	      (t (setf jy (+ iyyy (- 1)))
		 (setf jm (+ mm 13))))
	(setf julday (+ (+ (+ (int (* 365.25 jy))
			      (int (* 30.6001 jm)))
			   id)
			1720995))
	(cond ((>= (+ id (* 31 (+ mm (* 12 iyyy))))
		   igreg)
	       (setf ja (int (* 0.01 jy)))
	       (setf julday (+ (+ (+ julday 2)
				  (- ja))
			       (int (* 0.25 ja))))))
	(return julday)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 day-of-the-week
;;
(defun day-of-the-week(month day year)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          day-of-the-week Doc:
;;
;; Author: Andy Long
;; Created: Sun Jan 4 14:20:28 1998
;;
;; Description/Arguments:

       (day-of-the-week month day year)
  
;; Returns: day of the week of the date in question, as a symbol.

;; See Also: julian, caldate

;; Examples:
 (day-of-the-week 1 4 1998)	;; is a Sunday, as we see above!
 (day-of-the-week 1 5 1998)
 (day-of-the-week 1 6 1998)
 (day-of-the-week 1 7 1998)
 (day-of-the-week 1 8 1998)
 (day-of-the-week 1 9 1998)
 (day-of-the-week 1 10 1998)
 (day-of-the-week 1 11 1998)

 ;; Some of my favorite birthdays:
 (day-of-the-week 1 27 1968)	;; Mindy
 (day-of-the-week 2 28 1963)	;; Thad
 (day-of-the-week 2 4 1961)	;; Andy
 (day-of-the-week 2 14 1959)	;; Steve
 (day-of-the-week 4 5 1933)	;; Mom
 (day-of-the-week 4 10 1931)	;; Dad
 (day-of-the-week 12 24 1985)	;; Tchapo
"
  (elt 
   (list "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
   (mod (- (julian month day year) 2450818) 7)
   ;; I got 2450818 (a Sunday) from 1/4/1998: (julian 1 4 1998)
   )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 fullmoon
;;
(defun fullmoon (n nph &key (rad 0.017453293) julian)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          fullmoon Doc:
;;
;; Author: Andy Long (using f2cl and numerical recipes)
;; Created: Thu Dec 4 23:49:28 1997
;;
;; Description/Arguments:

       function fullmoon (n nph &key (rad 0.017453293) julian)
  
;; Returns: values jd and frac

;; Notes: (from flmoon, of NR)
	'calculates the phases of the moon. Given an integer n and a code nph
for the phase desired (nph=0 for the new moon, 1 for first quarter, 2 for full,
3 for last), the routine returns the Julian Day Number, and the fractional part
of a day (frac) to be added to it, of the Nth such phase since January,
1900. Greenwich Mean Time is assumed.'

;; See Also:

;; Examples:
 (fullmoon 1211 2) ;; (12 13 1997) - yep, checked it with my calendar... 
 ;; can give it as a julian day:
 (fullmoon 1000 2 :julian t)
"
  (declare (type fixnum nph))
  (declare (type fixnum n))
  (declare (type single-float rad))
  (prog ((i 0)
	 jd frac
	 (xtra 0.0)
	 (am 0.0)
	 (as 0.0)
	 (t2 0.0)
	 (t_ 0.0)
	 (c 0.0))
	(declare (type fixnum i))
	(declare (type single-float xtra))
	(declare (type single-float am))
	(declare (type single-float as))
	(declare (type single-float t2))
	(declare (type single-float t_))
	(declare (type single-float c))
	(setf c (+ n (f2cl/ nph 4.0)))
	(setf t_ (f2cl/ c 1236.85))
	(setf t2 (expt t_ 2))
	(setf as (+ 359.2242 (* 29.105356 c)))
	(setf am (+ (+ 306.0253 (* 385.816918 c))
		    (* 0.01073 t2)))
	(setf jd (+ (+ 2415020 (* 28 n))
		    (* 7 nph)))
	(setf xtra (+ (+ 0.7593299999999999 (* 1.53058868 c))
		      (* (+ 1.178E-4 (* (* -1 1.55E-7)
					t_))
			 t2)))
	(cond ((or (= nph 0)
		   (= nph 2))
	       (setf xtra (+ (+ xtra (* (+ 0.1734 (* (* -1 3.93E-4)
						     t_))
					(sin (* rad as))))
			     (* (* -1 0.4068)
				(sin (* rad am))))))
	      ((or (= nph 1)
		   (= nph 3))
	       (setf xtra (+ (+ xtra (* (+ 0.1721 (* (* -1 4.0E-4)
						     t_))
					(sin (* rad as))))
			     (* (* -1 0.628)
				(sin (* rad am))))))
	      (t (error "NPH IS UNKNOWN.")))
	(cond ((>= xtra 0.0)
	       (setf i (int xtra)))
	      (t (setf i (int (+ xtra (- 1.0))))))
	(setf jd (+ jd i))
	(setf frac (+ xtra (- i)))
	(return (values (if julian jd (caldate jd)) frac))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          gamma-incomplete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gamma-incomplete (a x)
  "
;; The incomplete gamma function, from Numerical Recipes ('Numerical Recipes:
 The Art of Scientific Computing', Press et al., Cambridge University Press,
 1986.)

;; Examples:
 ;; p. 161 of NR, figure 6.2.1:
 (title \"Plot of incomplete gamma Functions\")
 (plot
   (lambda(x) (gamma-incomplete .5 x))
   (lambda(x) (gamma-incomplete 1 x))
   (lambda(x) (gamma-incomplete 3 x))
   (lambda(x) (gamma-incomplete 10 x))
   0 15)
"
  (declare (type single-float x))
  (declare (type single-float a))
  (prog ((gammcf 0.0)
	 (gamma-incomplete 0.0)
	 (gln 0.0)
	 (gamser 0.0))
	(declare (type single-float gammcf))
	(declare (type single-float gamma-incomplete))
	(declare (type single-float gln))
	(declare (type single-float gamser))
	(if (or (< x 0.0)
		(<= a 0.0))
	    (error nil))
	(cond ((< x (+ a 1.0))
	       (multiple-value-setq (gamser a x gln)
				    (gser gamser a x gln))
	       (setf gamma-incomplete gamser))
	      (t (multiple-value-setq (gammcf a x gln)
				      (gcf gammcf a x gln))
		 (setf gamma-incomplete (+ 1.0 (- gammcf)))))
	(return gamma-incomplete)))


(defun gammq (a x)
  (declare (type single-float x))
  (declare (type single-float a))
  (prog ((gammcf 0.0)
	 (gammq 0.0)
	 (gln 0.0)
	 (gamser 0.0))
	(declare (type single-float gammcf))
	(declare (type single-float gammq))
	(declare (type single-float gln))
	(declare (type single-float gamser))
	(if (or (< x 0.0)
		(<= a 0.0))
	    (error nil))
	(cond ((< x (+ a 1.0))
	       (multiple-value-setq (gamser a x gln)
				    (gser gamser a x gln))
	       (setf gammq (+ 1.0 (- gamser))))
	      (t (multiple-value-setq (gammcf a x gln)
				      (gcf gammcf a x gln))
		 (setf gammq gammcf)))
	(return gammq)))


(defun gser (gamser a x gln &key (itmax 100)
		    (eps 3.0E-7))
  (declare (type single-float gln))
  (declare (type single-float x))
  (declare (type single-float a))
  (declare (type single-float gamser))
  (declare (type fixnum itmax))
  (declare (type single-float eps))
  (prog ((n 0)
	 (del 0.0)
	 (sum 0.0)
	 (ap 0.0))
	(declare (type fixnum n))
	(declare (type single-float del))
	(declare (type single-float sum))
	(declare (type single-float ap))
	(setf gln (gammln a))
	(cond ((<= x 0.0)
	       (if (< x 0.0)
		   (error nil))
	       (setf gamser 0.0)
	       (go end_label)))
	(setf ap a)
	(setf sum (f2cl/ 1.0 a))
	(setf del sum)
	(fdo (n 1 (+ n 1))
	     ((> n itmax)
	      nil)
	     (tagbody (setf ap (+ ap 1.0))
		      (setf del (f2cl/ (* del x)
				       ap))
		      (setf sum (+ sum del))
		      (if (< (abs del)
			     (* (abs sum)
				eps))
			  (go label1))))
	(error "A TOO LARGE ,  ITMAX TOO SMALL")
	label1 (setf gamser (* sum (exp (+ (+ (- x)
					      (* a (log x)))
					   (- gln)))))
	(go end_label)
	end_label (return (values gamser a x gln))))


(defun gcf (gammcf a x gln &key (itmax 100)
		   (eps 3.0E-7))
  (declare (type single-float gln))
  (declare (type single-float x))
  (declare (type single-float a))
  (declare (type single-float gammcf))
  (declare (type fixnum itmax))
  (declare (type single-float eps))
  (prog ((g 0.0)
	 (anf 0.0)
	 (ana 0.0)
	 (an 0.0)
	 (n 0)
	 (fac 0.0)
	 (b1 0.0)
	 (b0 0.0)
	 (a1 0.0)
	 (a0 0.0)
	 (gold 0.0))
	(declare (type single-float g))
	(declare (type single-float anf))
	(declare (type single-float ana))
	(declare (type single-float an))
	(declare (type fixnum n))
	(declare (type single-float fac))
	(declare (type single-float b1))
	(declare (type single-float b0))
	(declare (type single-float a1))
	(declare (type single-float a0))
	(declare (type single-float gold))
	(setf gln (gammln a))
	(setf gold 0.0)
	(setf a0 1.0)
	(setf a1 x)
	(setf b0 0.0)
	(setf b1 1.0)
	(setf fac 1.0)
	(fdo (n 1 (+ n 1))
	     ((> n itmax)
	      nil)
	     (tagbody (setf an (float n))
		      (setf ana (+ an (- a)))
		      (setf a0 (* (+ a1 (* a0 ana))
				  fac))
		      (setf b0 (* (+ b1 (* b0 ana))
				  fac))
		      (setf anf (* an fac))
		      (setf a1 (+ (* x a0)
				  (* anf a1)))
		      (setf b1 (+ (* x b0)
				  (* anf b1)))
		      (cond ((/= a1 0.0)
			     (setf fac (f2cl/ 1.0 a1))
			     (setf g (* b1 fac))
			     (if (< (abs (f2cl/ (+ g (- gold))
						g))
				    eps)
				 (go label1))
			     (setf gold g)))))
	(error "A TOO LARGE ,  ITMAX TOO SMALL")
	label1 (setf gammcf (* (exp (+ (+ (- x)
					  (* a (alog x)))
				       (- gln)))
			       g))
	(return (values gammcf a x gln))))


(defun gammln (xx)
  (declare (type single-float xx))
  (prog
   ((gammln 0.0)
    (j 0)
    (ser 0.0)
    (tmp 0.0)
    (x 0.0)
    (fpf 0.0)
    (one 0.0)
    (half 0.0)
    (stp 0.0)
    (cof (make-array (list 6) :element-type (quote single-float)))
    )
   (declare (type single-float gammln))
   (declare (type fixnum j))
   (declare (type (simple-array single-float (*))
		  cof))
   (declare (type single-float stp))
   (declare (type single-float half))
   (declare (type single-float one))
   (declare (type single-float fpf))
   (declare (type single-float x))
   (declare (type single-float tmp))
   (declare (type single-float ser)) 
   (replace cof (list 
		 76.18009173 -86.50532033 24.01409822
		 -1.231739516 .00120858003 -.00000536382)
	    :end 6)
   (setq stp 2.50662827465)
   (setq fpf 5.5)
   (setq one 1.0)
   (setq half 0.5)
   (setf x (+ xx (- one)))
   (setf tmp (+ x fpf))
   (setf tmp (+ (* (+ x half)
		   (log tmp))
		(- tmp)))
   (setf ser one)
   (fdo (j 1 (+ j 1))
	((> j 6)
	 nil)
	(tagbody (setf x (+ x one))
		 (setf ser (+ ser (f2cl/ (fref cof j)
					 x)))))
   (setf gammln (+ tmp (log (* stp ser))))
   (return gammln)
   )
  )

