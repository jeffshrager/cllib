; (load (compile-file "dotriangles.lisp"))

;; Solves a problem from Leo's application to Proof School's Lemma
;; Camp. There is a 4x5 grid and triangles (called "chips") connect
;; any three of the dots. It wants to know how many "good" chips there
;; are, that is, ones that have a right angle and the two legs are the
;; same length; There must be a trick to counting these, like the good
;; ones have to have a hypotenuse that's a perfect square, or
;; something like that, but this just counts them the brute force way.

(defvar *all* nil)

(defun pp ()
  (loop for (hyp ((ax . ay) (bx . by) (cx . cy))) in (sort *all* #'> :key #'first)
	do (format t "   [~a, [[~a, ~a], [~a, ~a], [~a, ~a]]],~%" hyp ax ay bx by cx cy)))

(defvar *hyp->ntris* (make-hash-table :test #'equal))

(defun all-possible-triangles ()
  (setf *all* nil)
  (loop for ax from 1 to 5
	do (loop for ay from 1 to 4
		 do (loop for bx from 1 to 5
			  do (loop for by from 1 to 4
				   do (loop for cx from 1 to 5
					    do (loop for cy from 1 to 4
						     as a = (cons ax ay)
						     as b = (cons bx by)
						     as c = (cons cx cy)
						     as tri = (list a b c)
						     as hyp = (if-works-get-hyp tri)
						     when (and hyp (not (zerop hyp)) (not (or (equal a b) (equal b c) (equal a b))))
						     do (pushnew (list hyp tri) *all* :test #'(lambda (t1 t2) (same-tri? (second t1) (second t2))))))))))
  (clrhash *hyp->ntris*)
  ;; Have to do this last bcs of the pushnew test
  (loop for (hyp) in *all* do (incf (gethash hyp *hyp->ntris* 0)))
  (loop for hyp being the hash-keys of *hyp->ntris*
	using (hash-value n)
	do (print (list hyp n)))
  (length *all*))

(defun same-tri? (t1 t2)
  (and (member (first t1) t2 :test #'equal)
       (member (second t1) t2 :test #'equal)
       (member (third t1) t2 :test #'equal)))

(defun d (a b)
  (let* ((ax (car a))
	 (ay (cdr a))
	 (bx (car b))
	 (by (cdr b))
	 (dx (- ax bx))
	 (dy (- ay by)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun if-works-get-hyp (tri)
  (let* ((a (first tri))
	 (b (second tri))
	 (c (third tri))
	 (dab (d a b))
	 (dac (d a c))
	 (dbc (d b c))
	 )
    (cond ((good-chip? dac dab dbc) (d b c))
	  ((good-chip? dbc dab dac) (d a c))
	  ((good-chip? dac dbc dab) (d a b)))))

(defun good-chip? (a b c)
  (and (= a b) (< (abs (- (+ (* a a) (* b b)) (* c c))) 0.001)))
				   
(all-possible-triangles)
(pp)
