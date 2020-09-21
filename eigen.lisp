(require "f2cltmp.lsp")
(require "eigs2.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           Eigenvalue/Eigenvector/ODE functions
;;
#|
	The following depends on the numerical recipes code in the file
f2cl_mac.lsp (so named because they were translated from f2cl, using f2cl
macros). The numerical recipes codes in f2cl_mac could be optimized - since
they are translations of fortran code, they are rather kludgy. These are only
used by eigenvalues, while the others make heavy use of eigenvalues, so the
numerical recipes routines are at the heart of the calculations.

	So far, the following functions are defined:

essential functions:

	eigenvalues			; eigenvalues of a general nxn matrix
					; (using numerical recipes routines)

	eigenvectors			; gives the eigenvectors in the case
					; that all multiplicities are 1

	generalized-eigenvectors	; gives the eigenvectors and
					; generalized eigenvectors in the
					; general case, plus the jordan form

	linear-first-order-ode		; uses generalized-eigenvectors to
					; solve first order linear systems,
					; returning a solution function
	linear-higher-order-ode		; solves higher-order linear odes by
					; turning them into linear systems and
					; calling linear-first-order-ode

helpers:
	break-sequence-into-runs	; a helper for jordan-associated-t-matrix
	eigen-verbosity			; prints some checks for eigenvectors and
					; generalized-eigenvectors 
	jordan-form			; uses generalized-eigenvectors to give
					; the jordan-form of a matrix
	jordan-associated-t-matrix	; the polynomial matrix associated with
					; a given jordan canonical form
	jordan-form-helper		; like hamburger helper, only for the
					; jordan form of a matrix.
|#
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 eigenvalues
;;
(defun eigenvalues(a &key (its 100))
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          eigenvalues Doc:
;;
;; Author: Andy Long
;; Created: Wed Nov 26 11:21:05 1997
;;
;; Description/Arguments:

       (eigenvalues matrix)
  
	This is a routine for generating the eigenvalues of a general nxn
matrix. Xlispstat didn't come with one (only for the symmetric case), so here's
an option. I've checked the code against some small matrices from a variety of
sources, and it seems to be working.

;; Returns: a list of the eigenvalues.

;; Notes:
	I used the numerical recipes routines balanc, elmhes, and hqr, which
had been converted from fortran into lisp using f2cl. I corrected one nasty
inequality error in the lisp code (see file f2cl_mac.lsp in the contrib
directory).

;; See Also:
	eigen for the symmetric real case. Balanc, elmhes, hqr are the
numerical recipes routines used to create eigenvalues.

;; Examples:
 (eigenvalues (id 3))

 ;; Strang's Introduction to Applied Mathematics:
 (eigenvalues (matrix '(3 3) '(1 -1 0 -1 2 -1 0 -1 1))) ;; 0, 1, 3
 (eigenvalues (matrix '(3 3) '(1 2 3 2 3 4 3 4 5)))
 (eigenvalues (matrix '(3 3) '(10 2 3 2 3 4 30 4 20)))

 ;; From Burden and Faires, numerical analysis:
 (eigenvalues (matrix '(3 3) '(2 -3 6 0 3 -4 0 2 -3))) ;; 2, 1, -1

 (defun special-tridiag(n a)
  (let (
	(initial (make-array (list n n) :initial-element 0))
	(diag (+ 1 (* 2 a)))
	)
    (mapcar (lambda(i) (setf (aref initial i i) diag)) (iseq n))
    (mapcar (lambda(i)
	      (setf
	       (aref initial (1+ i) i) (- a)
	       (aref initial i (1+ i)) (- a)
	       ))
	    (iseq (1- n)))
    (mprint initial)
    (print \"eigenvalues:\")
    (print
     (1+ (* 4 a 
            (square
	    (mapcar (lambda(i) (sin (/ (* pi i) (* 2 (1+ n))))) (iseq 1 n)))
            )))
    initial
    )
  )
 (eigenvalues (special-tridiag 2 1))
 (eigenvalues (special-tridiag 3 1))
 (eigenvalues (special-tridiag 4 1))
"
  (if (symmetric-matrix-p a)
      (coerce (first (eigen a)) 'list)
    (let* (
	   (size (size a))
	   (n (first size))
	   (wr (make-array n))
	   (wc (make-array n))
	   (temp (matrix size (combine a)))
	   (temp (balanc temp n n))
	   (temp (elmhes temp n n))
	   )
      (hqr temp n n wr wc :itmax its)
      (mapcar (lambda(x y) (if (= y 0) x (complex x y)))
	      (coerce wr 'list) (coerce wc 'list))
      )
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 eigenvectors
;;
(defun eigenvectors(a
		 &key
		 (its 100)
		 lambda
		 (eps 1e-12)
		 (its 30)
		 verbose
		 (infinity t)
		 )
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          eigenvectors Doc:
;;
;; Author: Andy Long
;; Created: Wed Nov 26 11:21:05 1997
;;
;; Description/Arguments:

       (eigenvectors matrix
		 &key
		 (its 100)
		 lambda
		 (eps 1e-12)
		 (its 30)
		 verbose
		 (infinity t)
		 )
  
;; Description:

	This is a routine for generating the eigenvalues and eigenvectors of a
general nxn matrix. Xlispstat didn't come with one (only for the symmetric
case), so here's an option. I've checked the code against some small matrices
from a variety of sources, and it seems to be working.

;; Returns: a list of two lists: the eigenvalues, and the eigenvectors.

;; Notes:
	Uses function eigenvalues, which is based on numerical recipes
routines; then an SVD method (motivated by the discussion in Numerical
Recipes) to compute (what I hope are) the eigenvectors.

	The heart of this method, as pointed out in Numerical Recipes, is that
there is a good probability that the matrix 

			A - lambda I

is singular, or near singular, provided that lambda is pretty close to a true
eigenvalue. We then want to 'replace zero pivots by some small number',
according to the authors. This is equivalent, I think, to weighing very
heavily the Schmidt-pairs associated with the singularity: thus I've added some
functionality to pseudo-inverse to make it produce only those Schmidt pairs
associated with zero (or near zero) singular values, allowing me to set the
eigenvector to the V vector associated with the smallest singular value from
the singular value decomposition 

			A = ULV'.

That would be the last V vector, since the singular values of L are ranked by
size. In the examples I've considered, there is always a single pair; in the
general case, one should take all eigenvectors corresponding to 0 singular
values (this just represents multiplicity), and check that there were as many
of them as there were multiples of the eigenvalue. If not, then one is obliged
to find the generalized eigenvectors. This is the process used in the function
generalized-eigenvectors.

	The main reason that this function deserves the name 'eigenvectors',
when it doesn't work if multiplicities are other than 1, is that 'the set of
operators on R^n that have n distinct eigenvalues is dense and open in 
L(R^n).' (page 154, Hirsch and Smale, 'Differential Equations, Dynamical
Systems, and Linear Algebra'). That is, given an arbitrary nxn matrix, this
routine will almost always work! Unfortunately, science is not arbitrary...
but as Hirsch and Smale say, if the components in a linear system are
susceptible to error, then we may as well assume genericity unless symmetry
conditions, etc. indicate otherwise.

;; See Also: generalized-eigenvectors, jordan-form

;; Examples:
 (eigenvectors (id 3))

 ;; Strang's Introduction to Applied Mathematics:
 (def strang (matrix '(3 3) '(1 -1 0 -1 2 -1 0 -1 1)))
 ;; eigenvalues: 0, 1, 3
 (def eig (eigenvectors strang))
 ;; more strang: zero can be an eigenvalue:
 (def eig (eigenvectors (diagonal-to-matrix '(3 2 0))))
 ;; There is only a single eigenvector for the following:
 (def eig (eigenvectors (matrix '(2 2) '(0 1 -9 6))))
 (eigenvectors (matrix '(2 2) '(3 1 0 3)))

 ;; other sources
 (def eig (eigenvectors (matrix '(3 3) '(1 2 3 2 3 4 3 4 5))))
 (def eig (eigenvectors (matrix '(3 3) '(10 2 3 2 3 4 30 4 20))))

 ;; From Burden and Faires, numerical analysis:
 ;; eigenvalues: 2, 1, -1
 (eigenvectors (matrix '(3 3) '(2 -3 6 0 3 -4 0 2 -3)))
"
  (if (not (matrixp a))
      (error "Function eigenvectors requires a matrix."))
  (let (eigen
	eigenvectors
	eigenvalues)
    (if (symmetric-matrix-p a) ;; Then we use the pre-existing routine:
	(setq
	 eigen (eigen a)
	 eigenvalues (coerce (first eigen) 'list)
	 eigenvectors (mapcar #'(lambda(x) (coerce x 'list)) (second eigen))
	 )
      ;; otherwise we use the svd routine:
      (let* (
	     (size (size a))
	     (n (first size))
	     (id (id n))
	     (b (apply
		 #'bind-columns
		 (mapcar
		  #'(lambda(x) (normalize (- (* 2 (uniform-rand n)) 1)))
		  (iseq n))))
	     )
	(setq eigenvalues (if lambda lambda (eigenvalues a)))
	(dolist
	 (lambda eigenvalues)
	 (let* (
		(p
		 (let (
		       (tmp 
			;; I have to 'catch-multiple-values', as the indicator
			;; of the type of response is given as the second
			;; value: I use the function multiple-value-list to do
			;; this, rather than the macros catch-multiple-values
			;; or cmv:
			(multiple-value-list
			 (pseudo-inverse
			  (- a (* lambda id))
			  :verbose nil
			  :infinity infinity
			  ))))
		   ;; The first part of tmp is either a list or a matrix,
		   ;; depending on whether infinity is t or nil. The second
		   ;; part of tmp is either a list of indices of 0 singular
		   ;; values, or a logical. 
		   (if (and (listp (second tmp))
			    (numberp (first (second tmp))))
		       (first (third (first tmp)))
		     (first tmp))))
		(y (if (matrixp p)
		       (normalize 
			(mapcar #'mean 
				(mapcar #'normalize
					(column-list (matmult p b)))))
		     p
		     )
		   )
		(change 1e31)
		)
	   (if (listp p)
	       ;; then we've just picked off the v vector associated with the
	       ;; singularity (already normalized):
	       (setq y p)
	     ;; otherwise, we'll have to do some iterating (perhaps the matrix
	     ;; wasn't quite singular enough - this argues that we really don't
	     ;; have the right eigenvalue, so we should be updating that too
	     ;; (I'm not yet).
	     (do
	      (
	       (i 0 (1+ i))
	       (yold y y)
	       )
	      ((or (> i its) (< change eps)))
	      (setq
	       y (normalize (matmult p y))
	       change (norm (- y yold))
	       )
	      )
	     )
	   (setq eigenvectors (append eigenvectors (list y)))
	   )
	 )
	)
      )

    (if verbose
	;; let's be talkative: the trace should equal the sum of the
	;; eigenvalues (see Strang); and if we multiply the matrix times the
	;; eigenvectors and check the norms of the results, we'd better see
	;; absolute values of the eigenvalues popping up:
	(eigen-verbosity a eigenvalues eigenvectors))

    ;; Return a list of eigenvalues and eigenvectors:
    (list eigenvalues eigenvectors)
    )
  )
;;
#|
;; from a note to my dad:

	Here's the mathematical issue of the day: I realized
awhile back that xlispstat doesn't come with a routine for
finding eigenvalues and eigenvectors in the general (rather than
symmetric) case, so I decided to take care of that. I picked up
that copy of Numerical Recipes that you let me bring along, and
found the necessary eigenvalue routines inside. These I could
implement quickly, thanks to the program f2cl (fortran to common
lisp converter) and the fortran code from Numerical Analysis.

	But for the eigenvectors, NR didn't even offer a routine
(suggesting instead, probably quite rightly, that it's better to
avail oneself of eispack or the like). The did describe inverse
iteration, however, and I endeavored to try that.

	Upon a moment's reflection, however (well, many many
moments in fact!), I realized that the SVD of 

			A - lambda I

provides the eigenvector(s) for lambda, including the case of
a base for multiple (algebraic=geometric multiplicity)
eigenvectors, etc.

	I imagine that this method is not discussed as a
serious method because it is not efficient. The authors are
remiss, I feel, in not indicating that the routines provide in NR
permit the simple calculation of the eigenvectors, however!

	The hard case is the generalized eigenvectors, but here
again the SVD can be used to good effect: you just have to keep
trying to solve

		(A - lambda I)^n y = x

for the eigenvectors x, until you get zero vectors for answers,
and for that I use the pseudo-inverse. And since the inverse of 
(A - lambda I)^n is just the inverse of (A - lambda I) n times,
it doesn't require much more computation. And at any rate, for
the size of problems I (or students) would deal with, we're not
going to be waiting around too long. One nice thing about writing
pedogogical rather than research software!

POSTSCRIPT:

	Well, that didn't quite work! The problem is that in the case of
multiple eigenvectors, the basis obtained from the SVD of (A - lambda I) can be
chosen in an infinite number of ways, only certain of which provide a natural
basis for the generalized eigenspace.

	Here's my intended solution: currently I move directly from step 1 to
part of step 4:

1)	Get a basis for the eigenspace B

	If it's a lonely vector, we're in no danger; otherwise, if
	algebraic=geometric multiplicity, then again we're done as there are no
	generalized eigenvectors; otherwise, 

2)	P = (A - lambda I) (A - lambda I)^{-1} B

	will contain a certain number of the eigenvectors (those with
	generalized eigenvectors associated). They may also contain other
	stuff, spurious stuff; this, too, is a headache. If it ain't in the
	span of B, then it ain't kosher.
		The distinct vectors corresponding to the span of S can
	then be normalized to form a basis for this space, call it S.

3)	Reexpress B in terms of S and the orthogonal complement to S in B.

4)	Now proceed to the stage of actually establishing the "order" of each
	eigenvector. This involves continuing to look at the pre-images

		P = (A - lambda I) (A - lambda I)^{-1} GEV

	to see if they're in the span of everything up to that point. If so,
	the distinct ones again get thrown into the soup.


	Okay: This has now been implemented, and my two problem cases are
problems no more. The question is what new problems lurk on the horizon? Only
the shadow knows....
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 generalized-eigenvectors
;;
(defun generalized-eigenvectors(a
				&key
				lambda
				(eps 1e-12)
				(meps 1e-12)
				(its 30)
				verbose
				)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          generalized-eigenvectors Doc:
;;
;; Author: Andy Long
;; Created: Wed Nov 26 11:21:05 1997
;;
;; Description/Arguments:

 (generalized-eigenvectors
	A		- a matrix

	&key
	lambda		- a guess (list of size n) for the eigenvalues
	(eps 1e-12)	- compared to norm of a vector to decide whether a
			  vector is zero or not.
	(meps 1e-12)	- used for multiple eigenvectors - might be better to
			  keep this on the large side of small! If the routine
			  punts, try setting this down to 1e-4, say.
	(its 30)	- if iteration is necessary, in the event of gross
			  failure... I'm not sure that it's ever used, actually.
	verbose		- be talkative
	)
  
	This is a routine for generating the eigenvalues and 'generalized
eigenvectors' of a general nxn matrix. By a generalized eigenvector we mean
a vector y which is not an eigenvector, but which satisfies

		(A - lambda I)y = x			(1)

for an eigenvector (or another generalized eigenvector) x. When the generalized
eigenvectors are combined with the eigenvectors and the Jordan form in the
proper way, they yield A.

;; Returns: 
	a list of either two or three things: a list of eigenvalues, and
 a matrix S containing the eigenvectors, in the case where all are true
 eigenvectors; or a list of eigenvalues, the matrix S of true and generalized
 eigenvectors, and the Jordan form J of the matrix.

 First case:	A = S (diagonal-to-full lambda) S^(-1);
 Second case:	A = S J S^(-1);

 The first vector corresponding to a Jordan block will be the true eigenvector,
 which the rest of the vectors in the block are generalized.

;; See also:
	eigen, eigenvectors (the first routine is used in the case of
 symmetry, and the second in the unsymmetric case of distinct eigenvalues).

;; Notes:
	Uses function eigenvalue, based on the numerical recipes routines
balanc, elmhes, and hqr, which had been converted from fortran into lisp using
f2cl. A Singular Value Decomposition (SVD) method (motivated by the discussion
in Numerical Recipes) is used to compute the generalized-eigenvectors.

	The point of this method, as pointed out in Numerical Recipes, is that
there is a good probability that the matrix 

			A - lambda I

is singular, or near singular. We then want to 'replace zero pivots by some
small number', according to them. This is equivalent, I think, to weighing very
heavily the Schmidt-pairs associated with the singularity: thus I've added some
functionality to pseudo-inverse to make it produce only those Schmidt pairs
associated with zero (or near zero) singular values, allowing me to set the
eigenvectors to the V vectors associated with the zero singular values. We
start with the SVD of A:

			A = ULV'.

These would be the last V vectors, since the singular values of L are ranked by
size. The number of zero singular values is the geometric multiplicity of the
eigenvalue, which, if equal to the algebraic multiplicity of the eigenvalue,
makes our life easy. If not, however, we have to go after the generalized
eigenvectors, solving the equation (1) above.

;; Examples:
 ;; these easy cases are actually handled by eigen or eigenvector:
 (generalized-eigenvectors (id 3))
 ;; the following has eigenvalues: 2, 1, -1:
 (generalized-eigenvectors (matrix '(3 3) '(2 -3 6 0 3 -4 0 2 -3)) :verbose t)

 ;; From Strang's 'Introduction to Applied Mathematics':
 ;; There is only a single eigenvector for the following:
 ;; should return it (x), and a generalized eigenvector, which has
 ;; the property that (A - lambda I)y=x:
 (generalized-eigenvectors (matrix '(2 2) '(0 1 -9 6)) :verbose t)
 ;; Here's another from Strang's book 
 (generalized-eigenvectors #2a((3 1)(0 3)))
 
 ;; What happens if I make a mistake in using :lambda? 
 ;;	Not so bad stuff:
 ;; (generalized-eigenvectors #2a((3 1)(0 3.01)) :lambda '(3 3) :meps 1e-4
 ;; :verbose t)
 ;;	Bad stuff:
 ;; (generalized-eigenvectors #2a((3 1)(0 3)) :lambda '(3.02 3.01) :verbose t)
 ;; Probably best to just let us do the lambda calculating!

 ;; We should probably check out a bunch of different cases of the Jordan
 ;; Canonical Form (see, for example 'Matrix Theory and Linear Algebra'
 ;; (Herstein and Winter):
 (generalized-eigenvectors #2a((0 1)(0 0)) :verbose t)
 (generalized-eigenvectors #2a((0 0 0 0)(1 0 0 0)(0 1 0 0)(0 0 1 0)))

 ;; This one was a casse-tete for generalized eigenvectors (from strang,
 ;; p. 82), but we licked it! It's rank one, and has all zero eigenvalues:
 (generalized-eigenvectors #2a((1 -1 1)(3 -3 3)(2 -2 2)) :verbose t)
 ;; Here's another former antagonist:
 (generalized-eigenvectors #2a((3 1 0 0)(0 3 0 0)(0 0 3 1)(0 0 0 2))
   :verbose t)
"
  (let (
	eigen
	eigenvectors
	eigenvalues
	distinct
	repetitions
	jordan
	)
    (cond
     ((not (matrixp a))	;; oops! Need a matrix:
      (error "Function generalized-eigenvectors requires a matrix."))
     ((symmetric-matrix-p a) ;; Then we use the pre-existing routine eigen:
      (setq
       eigen (eigen a)
       eigenvalues (coerce (first eigen) 'list)
       eigenvectors (mapcar #'(lambda(x) (coerce x 'list)) (second eigen))
       ))
     (t 		;; otherwise we use the svd routine:
      (setq 
       eigenvalues (if lambda lambda (eigenvalues a))
       distinct (remove-duplicates eigenvalues)
       )
      ;; if all eigenvalues are distinct, i.e.
      (if (= (length eigenvalues) (length distinct))
	  ;; then set eigenvectors to those obtained by the old routine:
	  (setq eigenvectors (second (eigenvectors a :verbose nil)))
	;; otherwise, we're into the POTENTIAL for generalized eigenvectors: 
	;; we may simply have multiplicity with distinct eigenvectors, but we
	;; don't know that yet, so here we go.
	;;
	;; 	We're effectively seeking the Jordan form of the matrix, which
	;; we can get once we know the multiplicities (algebraic and geometric)
	;; of the eigenvalues:
	;;
	;; algebraic: number of times an eigenvalue is repeated;
	;; geometric: number of distinct eigenvectors associated with the
	;;            eigenvalue. 
	;;
	;; For example, 0 is an eigenvalue of algebraic multiplicity 2 of the
	;; matrix #2a((0 1)(0 0)), and geometric multiplicity 1.
	(let* (
	       (n (first (size a)))
	       (id (id n))
	       )
	  ;; Repetitions is a list of the algebraic multiplicities:
	  (setq repetitions
		(mapcar
		 (lambda(x) (length (where eigenvalues x)))
		 distinct
		 ))
	  ;; We're now going to head over all the distinct eigenvalues, and
	  ;; their repetitions, trying to sort out what's a real eigenvector
	  ;; and what's a generalized eigenvector. For that reason, we're going
	  ;; to be updating jordan, which we'll ultimately hand to
	  ;; jordan-form-helper (like hamburger helper, only for Jordan forms)
	  ;; so that it can sort out things.
	  (mapcar
	   (lambda (lambda reps)
	     (let* (
		    which-bunch
		    (p
		     ;; I have to 'catch-multiple-values', as the
		     ;; indicator of the type of response is given as
		     ;; the second value: I use the FUNCTION
		     ;; multiple-value-list to do this, rather than
		     ;; the MACROs catch-multiple-values or cmv:
		     (multiple-value-list
		      (pseudo-inverse (- a (* lambda id))
				      :verbose nil
				      :infinity t
				      )))
		    ;; The first part of p is either a list of three
		    ;; lists (containing the u's corresponding to
		    ;; singularity, all the lambdas, and the v's
		    ;; corresponding to singularity) or a matrix,
		    ;; depending on whether infinity is t or nil. The
		    ;; second part of p is either a list of indices of 0
		    ;; singular values, or a logical.
		    (p
		     (if (and (listp (second p))
			      (numberp (first (second p))))
			 ;; then we've got a list of indices:
			 ;; we'll take all the v's:
			 (third (first p))
		       ;; otherwise this is a matrix:
		       (first p)))
		    (y (if (matrixp p)
			   (let ((b (apply
				     #'bind-columns
				     (mapcar
				      #'(lambda(x)
					  (normalize (- (* 2 (uniform-rand n)) 1)))
				      (iseq n))))
				 )
			     (normalize 
			      (mapcar #'mean 
				      (mapcar #'normalize
					      (column-list (matmult p b)))))
			     )
			 p
			 )
		       )
		    (change 1e31)
		    )
	       (if (listp p)
		   ;; then we'll just pick off the v vectors associated with
		   ;; the singularity (already normalized):
		   (setq
		    y
		    (if (= (length p) reps)
			;; then we have distinct eigenvectors, and we can
			;; quit worrying about jordan form for this lambda:
			(let ()
			  (setq jordan
				(append jordan (list (repeat 1 reps))))
			  p)
		      ;; otherwise we need to find some generalized
		      ;; eigenvectors. To do that, we'll solve
		      ;; (A-lambdaI)y=x, where x is an eigenvector,
		      ;; (A-lambdaI)z=y, where y is a generalized
		      ;; eigenvector, etc. until we've got a complete
		      ;; bunch: 
		      (let* (
			     ;;ael - stage 1:
			     (ashift (- a (* lambda id)))
			     ;;lea
			     (ashift-pinv (pseudo-inverse ashift :verbose nil))
			     
			     ;;ael - stage 2: we're only worried if the length
			     ;; of p is greater than 1:
			     (lp (> (length p) 1))
			     (pp (apply #'bind-columns p))
			     (S (if lp (matmult ashift ashift-pinv pp)))
			     (evs
			      (if lp
				  (complete-A-from-subspace-B pp S :eps meps)
				p
				)
			      )
			     ;;lea
			     
			     (k 0)
			     gvs 
			     )
			;;ael
			;; evs are our new modified (rotated) p's
			(setq p evs)
			;;lea
			(setf which-bunch (repeat 1 (length p)))
			(do*
			 (
			  (psi ashift-pinv (matmult psi ashift-pinv))
			  (gev (mapcar (lambda(x) (matmult psi x)) p)
			       (mapcar (lambda(x) (matmult psi x)) p))
			  ;; ael
			  ;; previously where-interesting was just where the
			  ;; norms were small. Now we want to take that a step
			  ;; farther: to be interesting, they have to project
			  ;; back to into the span of p or gvs space. 
			  (where-interesting
			   (intersection
			    (where (mapcar #'norm gev) eps #'>)
			    (which
			     (mapcar
			      #'(lambda(x)
				  (vector-in-span-of-vectors-p x pp :eps meps))
			      (column-lists
			       (matmult ashift (apply #'bind-columns gev))))))
						
			   (intersection
			    (where (mapcar #'norm gev) eps #'>)
			    (which
			     (mapcar
			      #'(lambda(x)
				  (vector-in-span-of-vectors-p x gvs :eps meps))
			      (column-lists
			       (matmult ashift (apply #'bind-columns gev))))))
			   )
			  ;; lea
			  (interesting
			   (select gev where-interesting)
			   (select gev where-interesting))
			  #|
			  ;; ael: adding a counter to decide if we've got all
			  ;; the eigenvectors we can stand:
			  (eigen-counter 
			   (length p)
			   (+ eigen-counter (length interesting)))
			  (crap
			   (pprint (list 'eigen-counter p eigen-counter gvs))
			   (pprint (list 'eigen-counter p eigen-counter gvs)))
			  ;; lea
|#
			  )
			 ((or (>= (incf k) n)
			      (not interesting)
			      #|
			      ;; ael: now adding the eigen-counter test:
			      (>= eigen-counter reps)
			      ;; lea
|#
			      ))
			 (setq gvs (append gvs interesting))
			 (mapcar
			  (lambda(i) (incf (elt which-bunch i)))
			  where-interesting
			  )
			 )
			;; which-bunch is a list of all those generalized
			;; vectors associated with a given eigenvector:
			(setq jordan (append jordan (list which-bunch)))
			(append p gvs)
			)
		      )
		    )
		 ;; otherwise, we'll have to do some iterating (perhaps the
		 ;; matrix wasn't quite singular enough - this argues
		 ;; that we really don't have the right eigenvalue, so
		 ;; we should be updating that too (I'm not yet).
		 (do
		  (
		   (i 0 (1+ i))
		   (yold y y)
		   )
		  ((or (> i its) (< change eps)))
		  (setq
		   y (normalize (matmult p y))
		   change (norm (- y yold))
		   )
		  )
		 )
	       (setq eigenvectors (append eigenvectors y))
	       )
	     )
	   distinct repetitions
	   )
	  )
	)
      )
     )
    ;; if Jordan, have to reorder the eigenvalues, according to distinct and
    ;; repetitions:
    (if jordan
	(setq eigenvalues
	      (combine 
	       (mapcar
		(lambda(eig rep) (repeat eig rep))
		distinct
		repetitions))))
    
    (if verbose
	;; let's be talkative: the trace should equal the sum of the
	;; eigenvalues (see Strang); and if we multiply the matrix times the
	;; eigenvectors and check the norms of the results, we'd better see
	;; absolute values of the eigenvalues popping up:
	(eigen-verbosity a eigenvalues eigenvectors))
    
    ;; Return a list of eigenvalues and a matrix of eigenvectors:
    (if jordan ;; then there are generalized eigenvectors:
	(append
	 (list eigenvalues)
	 (jordan-form-helper eigenvalues eigenvectors jordan :verbose verbose))
      (list eigenvalues (apply #'bind-columns eigenvectors))
      )
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 eigen-verbosity
;;
(defun eigen-verbosity(a eigenvalues eigenvectors)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          eigen-verbosity Doc:
;;
;; Author: Andy Long
;; Created: Sun Nov 30 19:52:06 1997
;;
;; Description/Arguments:

       (eigen-verbosity a eigenvalues eigenvectors)
  
;; Returns: 

;; Notes: just a helper for eigenvectors and generalized-eigenvectors

;; See Also: eigenvectors and generalized-eigenvectors

;; Examples:
"
  (let* (
	 (eigenmatrix (apply #'bind-columns eigenvectors))
	 (eigennorms (mapcar #'norm (column-list eigenmatrix)))
	 )
    (format t "~%Eigenvalues:~{ ~6,3g~}" eigenvalues)
    (format t "~%Sum of the Eigenvalues: ~6,3g" (sum eigenvalues))
    (format t "~%Trace of the Matrix   : ~6,3g" (sum (diagonal a)))
    (format t "~%Eigenvectors:~%")
    (mprint eigenmatrix)
    (format t "Eigenchecks:~{ ~6,3g~}~%" 
	    (/
	     (diagonal (matmult (transpose eigenmatrix) a eigenmatrix))
	     (* eigennorms eigennorms)
	     )
	    )
				  
    ;;	    (mapcar #'norm (column-list (matmult a eigenmatrix))))
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 jordan-form
;;
(defun jordan-form(a)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          jordan-form Doc:
;;
;; Author: Andy Long
;; Created: Wed Jan 7 20:24:16 1998
;;
;; Description/Arguments:

       (jordan-form matrix)
  
;; Returns: the Jordan canonical form of a matrix.

;; Notes: based on generalized-eigenvectors

;; See Also: generalized-eigenvectors

;; Examples:
 (jordan-form (id 2))
 (jordan-form #2a((1 1) (0 1)))
 ;; These two check out from Hirsch and Smale, 'Differential Equations,
 ;; Dynamical Systems, and Linear Algebra':
 (jordan-form #2a((0 1) (-1 0)))
 (jordan-form #2a((#c(1 1) 2) (0 #c(1 1))))
"
  (let ((ev (generalized-eigenvectors a)))
    (if (= (length ev) 3) (third ev) (diagonal-to-full (first ev)))
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 jordan-form-helper
;;
(defun jordan-form-helper(eigenvalues eigenvectors jordan &key verbose)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          jordan-form-helper Doc:
;;
;; Author: Andy Long
;; Created: Sun Nov 30 17:32:09 1997
;;
;; Description/Arguments:

       (jordan-form-helper eigenvalues all-eigenvectors jordan &key verbose)
  
;; Returns: S and J (Jordan form) matrices.

;; Notes: This is a helper for generalized-eigenvectors. The point is to
 provide a means for determining which generalized eigenvectors belong with
 which eigenvectors, and put them all together, creating the

		A = S J S^(-1)

 matrix decomposition. In the event that the eigenvectors are distinct, then
 J is just a diagonal matrix of eigenvalues; otherwise, it contains 1s in the
 super-diagonal of the matrix.

	If you just want to see the Jordan form of a matrix, then you should
 use function jordan-form.

;; See Also: jordan-form, generalized-eigenvectors

;; Examples:
 (jordan-form-helper
  '(3 3 3 4)
  '((-1 0 0 0) (0 0 1 0) (0 -1 0 0) ( 0 0 0 1))
  '((2 1) (1)) ;; this indicates that the first eigenvalue, 3, has multiplicity
 	       ;; 3, of which two are true eigenvectors and the third (picked
	       ;; up in the second round) belongs with the first eigenvector.
	       ;; There is a single eigenvector associated with four.
  )
 ;; should create S matrix
 (mprint #2A((-1 0 0 0) (0 -1 0 0) (0 0 1 0) (0 0 0 1)))
 ;; and Jordan matrix J (the matrix we used is already in Jordan form!):
 (mprint #2a((3 1 0 0)(0 3 0 0)(0 0 3 0)(0 0 0 4)))

 ;; to see the Jordan form of a matrix (that is not diagonalizable), try
 (generalized-eigenvectors #2a((3 3)(0 3)) :verbose t)
 (generalized-eigenvectors #2a((3 3 0)(0 3 3)(0 0 3)) :verbose t)

 (jordan-form-helper
  '(0.0 0.0 0.0)
  '((-0.816496580927726 -0.4082482904638628 0.408248290463863)
    (-1.0000716419349065E-16 -0.7071067811865475 -0.7071067811865476)
    (-0.029160592175990204 0.029160592175990204 -0.029160592175990204)
    (-0.08417937871268424 0.08417937871268424 -0.08417937871268424))
  '((2 1))
  :VERBOSE t)

 (JORDAN-FORM-HELPER '(3.0 3.0 3.0 3.0 3.0) '((-1.0 0.0 0.0 0.0 0.0) (0.0 0.0
  -1.0 0.0 0.0) (0.0 -1.0 0.0 0.0 0.0) (0.0 0.0 0.0 -1.0 0.0) (0.0 0.0 0.0 0.0
  -1.0)) '((2 3)) :VERBOSE T) 
"
  (let* (
	 (sum 0)
	 general

	 ;; choose indices so that generalized eigenvectors go with their
	 ;; eigenvectors; also establish the general from the regular, so that
	 ;; we can construct the jordan form:
	 (indices
	  (combine
	   (mapcar
	    (lambda(x)
	      (let ((tmp (+ sum (index-permute x))))
		(setq sum (+ sum (sum x))
		      general (combine general (mapcar #'rest tmp)))
		tmp
		)
	      )
	    jordan)))

	 (general (mapcar (lambda(x) (search (list x) indices))
			  (remove-if 'null general)))
	 
	 ;; Jordan form starts with eigenvalues on the diagonal
	 (j-matrix (diagonal-to-full eigenvalues))

	 (s-matrix 
	  (apply
	   #'bind-columns
	   (select
	    eigenvectors
	    indices)))
	 )
    
    ;; Complete the Jordan form of the matrix with appropriate ones:
    (dolist (g general) (setf (aref j-matrix (1- g) g) 1))

    (if verbose ;; give them the news:
	(let ()
	  (format t "~%Eigenvector Matrix S:~%")
	  (mprint s-matrix)
	  (format t "~%Jordan form J:~%")
	  (mprint j-matrix)
	  (format t "~%SJS^(-1) should recreate the matrix:~%")
	  (mprint (matmult s-matrix j-matrix (inverse s-matrix)))
	  ))

    ;; Return: list of generalized eigenvectors, and Jordan form:
    (list s-matrix j-matrix)
    )
  )
;;
(defun index-permute(x)
  "
;; Permutes the Indices of the jordan form properly so that we can sort out the
  eigenvectors and the generalized-eigenvectors.

;; Examples:
 (index-permute '(3 1 1))	;; (0 3 4 1 2) 
 (index-permute '(3 2))		;; (0 2 4 1 3) 
 (index-permute '(2 3))		;; (0 2 1 3 4) 
"
  (let* (
	 (n (max x))
	 (l (length x))
	 (bunch
	  (combine
	   (transpose
	    (mapcar
	     (lambda(i)
	       (if (= (- n i) 0) (repeat 1 i)
		 (combine (repeat 1 i)  (repeat 0 (- n i)))))
	     x))))
	 (nb
	   (mapcar
	    (lambda(i x) (+ i (* l (iseq x))))
	    (iseq l)
	    x
	    )
	  )
	 (correct
	  (1- (mapcar (lambda(i) (sum (select bunch (iseq 0 i))))
		      (combine nb)
		      )
	      ))
	 (sum 0)
	 )
    (mapcar (lambda(x) 
	      (let ((tmp (select correct (iseq sum (1- (+ sum x))))))
		(setq sum (+ sum x))
		tmp))
	    x)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 jordan-associated-t-matrix
;;
(defun jordan-associated-t-matrix(jordan-form)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          jordan-associated-t-matrix Doc:
;;
;; Author: Andy Long
;; Created: Mon Dec 1 14:36:59 1997
;;
;; Description/Arguments:

       (jordan-associated-t-matrix jordan-form)
  
;; Returns:
	the matrix function of a single variable which evaluates to the
 value of the associated t matrix of the given Jordan matrix.

;; Notes:
	The solution of a linear differential equation of the first degree is
 given by 

			U(t) = S (exp Jt) S^(-1)U(0)

 where this function returns the (exp Jt) matrix.

;; See Also: jordan-form-helper

;; Examples:

 (def jordan-function (jordan-associated-t-matrix #2a((0 1)(0 0))))
 ;; prints (nicely) the matrix at time t=1
 (mprint (funcall jordan-function 1))
 ;; prints the matrix at times t=0,1,2,3:
 (mapcar #'mprint (mapcar jordan-function (iseq 4)))

 (def jordan-function
    (jordan-associated-t-matrix #2a((1 1 0 0)(0 1 1 0)(0 0 1 0)(0 0 0 2))))
 (mprint (funcall jordan-function 3))

 (def jordan-function
    (jordan-associated-t-matrix #2a((0 1 0 0)(0 0 1 0)(0 0 0 1)(0 0 0 0))))
 (mprint (funcall jordan-function 3))
"
  (let* (
	 (size (size jordan-form))
	 (mat (id (first size)))
	 (eigs (diagonal jordan-form))
	 ;; The "jordan-part" is the nilpotent part on the super-diagonal:
	 (jordan-part (- jordan-form (diagonal-to-full eigs)))
	 ;; i will determine the powers of t needed:
	 non-zero
	 runs
	 powers-needed
	 lengths
	 tmp
	 result
	 sizes
	 neweigs
	 factorials
	 )
    (do
     ;; new-mat is powers of the jordan-part
     ((new-mat jordan-part (matmult jordan-part new-mat)))
     ;; since jordan-part is nilpotent, it will ultimately result in the zero
     ;; matrix: 
     ((= 0 (sum new-mat)))
     ;; in the meantime, we'll add it to mat to create the "form" matrix, which
     ;; I'll use to decide what to set to what in the jordan time matrix:
     (setq mat (+ mat new-mat))
     )

    ;; Now we'll break the non-zero elements into runs: this will give a
    ;; sequence of lists like ((0 1 2) (5 6) (10) (15)). For a jordan matrix,
    ;; this list will have sizes 3 2 1 1 that descend like this one. When we
    ;; hit one, we switch eigenvalues, and need to multiply by a different
    ;; exponential factor. The length of the runs indicates the powers of t
    ;; needed, as well.
    (setq 
     non-zero (where (combine mat) 0 '/=)
     runs (break-sequence-into-runs non-zero)
     lengths (mapcar #'length runs)
     powers-needed (max lengths)
     factorials (mapcar #'factorial (iseq powers-needed))
     )

    (mapcar
     (lambda(run eig)
       (if (= 1 (length run))
	   ;; Then we've got a whole bunch associated with a single
	   ;; eigenvalue, and we'll put the indices together:
	   (setq
	    tmp (if tmp (combine tmp run) run)
	    result (append result (list tmp))
	    tmp nil
	    neweigs (append neweigs (list eig))
	    )
	 ;; We're still picking up eigenvalues: continue:
	 (setq tmp (if tmp (combine tmp run) run))
	 ))
     runs eigs)

    (setq sizes
	  (mapcar
	   (lambda(p) (round (/ (1- (sqrt (1+ (* 8 (length p))))) 2)))
	   result))

    ;; Return the function which calculates the time-dependent matrix
    ;; associated with a given Jordan form, e^(Jt).
    (lambda(x)
      (let* (
	     (result 1)
	     (powers 
	      (dotimes
	       (i (1- powers-needed))
	       (setq result (combine result (* (final result) x)))))
	     (powers (/ result factorials))
	     (matrix (repeat 0 (apply #'* size)))
	     )
	(setf
	 (select matrix non-zero)
	 (combine
	  (mapcar
	   (lambda(ev y) 
	     (*
	      (exp (* ev x))
	      (mapcar (lambda(z) (select powers (iseq z))) (iseq y 1))
	      ))
	   neweigs sizes
	   )
	  )
	 )
	(matrix size matrix)
	)
      )
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 break-sequence-into-runs
;;
(defun break-sequence-into-runs(list)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          break-sequence-into-runs Doc:
;;
;; Author: Andy Long
;; Created: Mon Dec 1 15:35:31 1997
;;
;; Description/Arguments:

       (break-sequence-into-runs)
  
	Breaks an integer sequence into runs of consecutive increasing
 integers. Thus, the  sequence '(1 2 4 5 6 9 10) will be returned as
 '((1 2) (4 5 6) (9 10)).

;; Returns: a list of lists of runs.

;; Notes: this was written as a helper to jordan-associated-t-matrix.

;; See Also:

;; Examples:
 (break-sequence-into-runs '(1 2 4 5 6 9 10 14))
 (break-sequence-into-runs '(1 2 4 2 3 9 10))
"
  (let (
	(tmp (list (first list)))
	result
	)
    (do
     (
      (i (first list) (first list))
      (j (first (rest list)) (first (rest list)))
      )
     ((not j) (if tmp (setq result (append result (list tmp)))))
     (if (= (1+ i) j)
	 (setq tmp (combine tmp j))
       (setq
	result (append result (list tmp))
	tmp (list j)
	)
       )
     (setq list (rest list))
     )
    result
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 linear-first-order-ode
;;
(defun linear-first-order-ode(matrix initial &key verbose real)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          linear-first-order-ode Doc:
;;
;; Author: Andy Long
;; Created: Mon Dec 1 19:36:54 1997
;;
;; Description/Arguments:

       (linear-first-order-ode A x_0 &key verbose real)
  
	A:		the matrix of x'=Ax
	x_0:		the initial values of x
	verbose:	print out some info
	real:		if the solution is known to be real, set this to t

;; Returns:

	the solution vector function for a linear, first order ordinary
 differential equation, given by a matrix A and the initial conditions x_0:

		x' = Ax,  x(0)=initial

;; Notes:

	The solution to a linear first-order ode is determined by the Jordan
 Canonical form (SJS^(-1)) of the matrix A. Once it is given (by the matrix S
 containing the eigenvectors and generalized eigenvectors, and the Jordon
 matrix J containing the eigenvalues on the diagonal, and perhaps 1s on the
 super-diagonal if there are generalized-eigenvectors) the solution is just

			x(t) = S (exp Jt) S^(-1) x(0)

;; See Also: generalized-eigenvectors, jordon-form, jordan-associated-t-matrix

;; Examples:
 ;;----------------------------------------------------------------------
 ;; From 'Matrix Theory and Linear Algebra' (Herstein and Winter):
 (def solution (linear-first-order-ode #2a((0 1)(-2 3)) '(30000 40000)
				       :verbose t))
 (funcall solution 0) ;; (30000 40000)
 (defun known(x) (* 10000 (list (+ (exp (* 2 x)) (* 2 (exp x)))
	 		        (* 2 (+ (exp (* 2 x)) (exp x))))))
 (funcall solution 1)
 (known 1)
 ;;----------------------------------------------------------------------
 ;; From 'Introduction to Applied Mathematics', by Gilbert Strang:
 (def solution (linear-first-order-ode #2a((3 1)(0 3)) '(1 1) :verbose t))
 (funcall solution 0) ;; better be (1 1)!
 (defun known(x) (* (exp (* 3 x)) (list (1+ x) 1)))
 (funcall solution 1)
 (known 1)
 ;;----------------------------------------------------------------------
 (def solution (linear-first-order-ode #2a((0 1)(-1 0)) '(3 4)
				       :verbose t :real t))
 (funcall solution 0) ;; better be (1 1)!
 (defun known(x) (list (+ (*  3 (cos x)) (* 4 (sin x)))
		       (+ (* -3 (sin x)) (* 4 (cos x)))))
 (funcall solution 1)
 (known 1)
 ;;----------------------------------------------------------------------
 ;; this is a stiff equation, but doesn't cause us any problem....
 (def solution (linear-first-order-ode #2a((9 24)(-24 -51)) '(1 1)
				       :verbose t :real t))
 (funcall solution 0) ;; better be (1 1)!
 (defun known(x) (list (- (*  2 (exp (* -3 x)))      (exp (* -39 x)))
		       (+ (-    (exp (* -3 x))) (* 2 (exp (* -39 x))))))
 (funcall solution 1)
 (known 1)
 ;;----------------------------------------------------------------------
"
  (let* (
	 (jordan (generalized-eigenvectors matrix))
	 (eigenvalues (first jordan))
	 (s (second jordan))
	 (jordan (third jordan))
	 (jordan-function
	  (if jordan
	      (jordan-associated-t-matrix jordan)
	    (lambda(x) (diagonal-to-matrix (exp (* x eigenvalues))))))
	 (sinv (inverse s))
	 (rhs (matmult sinv initial))
	 )

    (if verbose
	(mapcar
	 #'mprint
	 (list
	  s 
	  (if jordan jordan (diagonal-to-matrix eigenvalues))
	  sinv
	  (matrix (list (length initial) 1) initial)
	  )))

    (if real
	;; if the solution is known to be real, force it to be returned real:
	(lambda(x) (realpart (matmult s (funcall jordan-function x) rhs)))
      ;; otherwise, fire away!
      (lambda(x) (matmult s (funcall jordan-function x) rhs))
      )
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                 linear-higher-order-ode
;;
(defun linear-higher-order-ode(coefficients initial &key verbose real)
  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          linear-higher-order-ode Doc:
;;
;; Author: Andy Long
;; Created: Mon Dec 1 21:30:23 1997
;;
;; Description/Arguments:

       (linear-higher-order-ode coefficients initial &key verbose real)
  
	coefficients: 	a list of the coefficients, starting with a 1 (or
			identity matrix) representing the highest order and
  			descending down the orders.
	initial:	a list of the initial values of u, u', etc.
	verbose:	just prints out some info
	real:		if the solution is known to be real, set this t!

 For example, if one wishes to solve the ode

 				u'' - 2u' + u = 0, with u(0)=3, u'(0)=3

 then the coefficients are     1    - 2    1     ;initial:   3        3.


;; Returns:

	a function giving the (vector) solution to the linear ode: u, u', and
 so on, up to (but not including) the highest power.

;; Notes:

	This function works by turning the higher order equation into a system
 of first order equations, and then calling linear-first-order-ode.

;; See Also: linear-first-order-ode

;; Examples:
 ;;----------------------------------------------------------------------
 ;; This solves       u'' - 2u' + u = 0, with u(0)=3, u'(0)=3:
 ;; coefficients:    1    - 2    1     ;initial:   3        3.
 (def solution (linear-higher-order-ode (list 1 -2 1) (list 3 3) :verbose t))
 ;; this is the true solution:
 (defun u(x) (list (* 3 (exp  x)) (* 3 (exp  x))))
 (u 0)
 (funcall solution 0)
 (u 1)
 (funcall solution 1)
 ;;----------------------------------------------------------------------
 ;; This solves       u'' + 6u' + 8u = 0, with u(0)=3, u'(0)=-8:
 ;; coefficients:    1    - 6     8     ;initial:   3        -8.
 (def solution (linear-higher-order-ode (list 1 6 8) (list 3 -8)))
 ;; this is the true solution:
 (defun u(x) (list (+ (* 2 (exp (* -2 x))) (exp (* -4 x)))
		   (* -4 (+ (exp (* -2 x)) (exp (* -4 x))))))
 (u 0)
 (funcall solution 0)
 (u 1)
 (funcall solution 1)
 ;;----------------------------------------------------------------------
 ;; Here's a higher order system with matrices:
 ;; 		u'' + A u = 0, with u(0)= (2 -1 -1) and u'(0)= (0 0 0):
 (def A #2a((1 -1 0)(-1 2 -1)(0 -1 1)))
 (def solution (linear-higher-order-ode (list (id 3) (zeros 3 3) A) 
				        '((2 -1 -1) (0 0 0)) :real t))
 (funcall solution 0)
 (funcall solution 1)

 ;; compare with the numerical solution obtained using Runga-Kutta:
 (def bigmat
     (combine-matrices
      (list (zeros 3 3)  (id 3)   )
      (list    (- A)    (zeros 3 3))
      ))
 (def points (rk-4 #'(lambda(vals) (matmult bigmat vals))
		   '(2 -1 -1 0 0 0) 0 1 200))
 (final points)
"
  (let* (
	 (coefficients (rest coefficients))
	 (n (length coefficients))
	 (matrix
	  (if (matrixp (final coefficients))
	      (let* (
		     (m (first (size (final coefficients))))
		     (zero (zeros m m))
		     (row (make-array (list n) :initial-element zero))
		     (row (coerce row 'list))
		     (tmp (setf (elt row 0) (id m)))
		     (setq initial (combine initial))
		     )
		(setq initial (combine initial))
		(apply
		 #'combine-matrices 
		 (append
		  (mapcar
		   (lambda(x) (setq row (circulate row :backward t)))
		   (iseq (1- n))
		   )
		  (list (- (reverse coefficients)))
		  )))
	    (combine-matrices
	     (list (zeros (1- n) 1) (id (1- n)))
	     (list (matrix (list 1 n) (- (reverse coefficients))))))
	  )
	 )
    (linear-first-order-ode matrix initial :verbose verbose :real real)
    )
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq 
 mat 
 '#2A((0.06968466 0.05556742 0.174681 0.369613 0.35956) 
      (0.09442135 0.06222814 0 0 0) (0.09308236 0.19995 0.265393 0 0) 
      (0.01824219 0.06995888 0.169444 0.216458 0)
      (0.001530221 0.01016977 0.04179888 0.140491 0.278759))
 )
(dolist 
 (eig (eigenvalues mat))
 (format t "~a~%" eig)
 )
(dolist 
 (eig (second (eigenvectors mat)))
 (format t "~a~%" eig)
 )

