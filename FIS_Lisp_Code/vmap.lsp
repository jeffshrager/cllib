;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- Compares voi sets with one another based upon distance
;;; metrics. 

;;; The method takes place in these steps; all distances are in
;;; t.coords.

;;; 1. Use *vs-inbrain-gather-limit* to compose within-brain groups.  That is,
;;; vois that are w/i inbrain-gather-limit of one another count as one.

;;; 2. Use *vs-inbrain-exclude-limit* to separate groups into sets that are
;;; assumed NOT to be relate to one another.  This mainly just speeds
;;; the fourth combinational step.

;;; 3. For each group in brain 1, find at most one group in brain 2
;;; that could be its partner. 

;;; 4. For each pairing, make all cross-brain combinations, and
;;; compute their distance score, filtering ones with scores below
;;; *vs-xbrain-gather-limit*
;;; 
;;; 5. Average the component signals into a grand signal.


(defvar *vs-inbrain-gather-limit* 5.0)
(defvar *vs-inbrain-exclude-limit* 10.0)
(defvar *vs-xbrain-gather-limit* 5.0) 

(defstruct (vset) vois tcenter signal)

(defvar *vsets*)

(defun vmap (voisets &aux vsets)
  ;; First use inbrain-gather-limit to compose the vois from each brain into vsets.
  (format t "(--- Step 1: Form initial inbrain vsets using inbrain-gather-limit = ~a.)~%" *vs-inbrain-gather-limit*)
  (setq vsets (mapcar #'compose-vois-into-sets voisets))
  ;; Next use maxlimit to separate subgroups.
  (format t "(--- Step 2: Form secondary inbrain vsets using inbrain-exclude-limit = ~a.)~%" *vs-inbrain-exclude-limit*)
  (setq vsets (mapcar #'compose-vsets-into-sets vsets))
  ;; Now we have a set of particles that we actually believe for
  ;; each individual brain.  Now use xbrain gather limit to find groupings
  ;; that are close enough to one another to believe that they are 
  ;; the same.
  (format t "(--- Step 3: Form xbrain vsets using xbrain-gather-limit = ~a.)~%" 
	  *vs-xbrain-gather-limit*)
  (setq vsets (pair-off-xbrain-vsets vsets))
  ;; And form the averaged signals.
  (setq *vsets* (mapcar #'bring-up-combined-signal vsets))
  )

;;; This does the first step where the inbrain-gather-limit is used to gather and
;;; separate vois into sets. 

(defun compose-vois-into-sets (vois &aux vsets)
  (format t "(There are ~a vois in this brain.)~%" (length vois))
  (dolist (voi vois)
    (let ((close-vset (find-close-vset voi vsets *vs-inbrain-gather-limit*))
	  )
      (if close-vset
	  (add-voi-to-vset voi close-vset)
	(push (make-vset :vois (list voi)
			 :tcenter (voi-tcenter voi))
	      vsets)
	)
      ))
  (describe-inbrain-vsets1 vsets)
  vsets)

;;; This finds ONE AND ONLY ONE voi set that matches the spec.
;;; There's a little bit of a problem if more than one fits, so a
;;; warning is printed and just the first one it listed.  I'll figure
;;; out later what to do about the problem of multiple fits.  It
;;; should be rare.

(defun find-close-vset (voi vsets limit &aux matches)
  (dolist (vset vsets)
    (if (< (xyz-distance* (voi-tcenter voi) (vset-tcenter vset))
	   limit)
	(push vset matches))
    )
  (if (cdr matches)
      (format t "(Warning; More than one vset was found near a voi.)~%")
    )
  (car matches))

(defun add-voi-to-vset (voi vset)
  (push voi (vset-vois vset))
  ;; Compute the center completely anew each time; do NOT simply use the
  ;; old tcenter to make the new one 'cause it's a mean already!
  (setf (vset-tcenter vset) 
	(xyzcentroid (mapcar #'voi-tcenter (vset-vois vset))))
  )

;;; Step 2: Subgroup vsets into ones that are outside of
;;; inbrain-exclude-limit from one another.  (If this were all oop'ed
;;; we could use the same ...-tcenter code as is used for inserting
;;; vois into vsets, but instead I just dup'ed most of the code.  Oh
;;; well. ("(There's a rat in separate!" - Steve Feutchbaum)

(defun compose-vsets-into-sets (oldvsets &aux newvsets)
  (dolist (oldvset oldvsets)
    (let ((close-newvset (find-close-newvset oldvset newvsets 
					     *vs-inbrain-exclude-limit*))
	  )
      (if close-newvset
	  (add-vset-to-vset oldvset close-newvset)
	(push (make-vset :vois (list oldvset)
			 :tcenter (vset-tcenter oldvset))
	      newvsets)
	)
      ))
  (describe-inbrain-vsets2 newvsets)
  newvsets)

(defun describe-inbrain-vsets2 (xbv)
  (format t "(There are ~a inbrain vsets.)~%" (length xbv))
  (dolist (v1 xbv)
    (format t "  (~a) @ ~a~%" (length (vset-vois v1))
	    (p2 (vset-tcenter v1)))
    (dolist (v2 (vset-vois v1))
       (format t "    (~a) @ ~a~%" (length (vset-vois v2))
	       (p2 (vset-tcenter v2))))
    )
  )

(defun describe-inbrain-vsets1 (ibv)
  (format t "(There are ~a inbrain vsets.)~%" (length ibv))
  (dolist (v ibv)
    (format t "  (~a) @ ~a~%" (length (vset-vois v))
	    (p2 (vset-tcenter v)))
    (dolist (v2 (vset-vois v))
       (format t "    #~a @ ~a~%" (voi-id v2) (p2 (voi-tcenter v2))))
    )
  )

(defun find-close-newvset (oldvset newvsets limit &aux matches)
  (dolist (newvset newvsets)
    (if (< (xyz-distance* (vset-tcenter newvset) (vset-tcenter oldvset))
	   limit)
	(push newvset matches))
    )
  (if (cdr matches)
      (format t "(Warning; More than one vset was found near a vset.)~%")
    )
  (car matches))

(defun add-vset-to-vset (newvset oldvset)
  (push newvset (vset-vois oldvset))
  (setf (vset-tcenter oldvset) 
	(xyzcentroid (mapcar #'vset-tcenter (vset-vois oldvset))))
  )

;;; Okay, so now we've got a list of vsets, each member of which is a
;;; vset, each member of which is a voi.  Third step: Pair each vset
;;; here with zero or one vsets from the other pair.  This uses xbrain
;;; limit and has to be done in both directions which makes it a
;;; little hard to keep everything straight.

(defun pair-off-xbrain-vsets (all-brain-vsets &aux collection)
  (dolist (bvs all-brain-vsets)
    (dolist (vs bvs)
      (let ((nearest-existing-vset (find-close-newvset vs collection
						       *vs-xbrain-gather-limit*)))
	(if nearest-existing-vset 
	    (add-vset-to-vset vs nearest-existing-vset)
	  (push (make-vset :vois (list vs)
			   :tcenter (vset-tcenter vs))
		collection))
	)))
  (describe-xbrain-vsets collection)
  collection
  )

(defun describe-xbrain-vsets (xbv)
  (format t "(There are ~a xbrain vsets.)~%" (length xbv))
  (dolist (v1 xbv)
    (format t "  (~a) @ ~a~%" (length (vset-vois v1))
	    (p2 (vset-tcenter v1)))
    (dolist (v2 (vset-vois v1))
       (format t "    (~a) @ ~a~%" (length (vset-vois v2))
	       (p2 (vset-tcenter v2))))
    )
  )

;;; Combine the signal information from all the vsets (by getting it
;;; from their component vois) and averaging it all into the vset's
;;; signal; Only works on xbrain vsets (top of the hieracrchy).  The
;;; signals are normalized before being averaged.  Don't know if this
;;; is actually right!

(defun bring-up-combined-signal (vset &aux signals signal)
  (dolist (v1 (vset-vois vset))
   (dolist (v2 (vset-vois v1))
    (dolist (v3 (vset-vois v2))
      (push (normalize (voi-tpldata v3)) signals))))
  (let ((l (length (car signals))))
    (do ((k 0 (1+ k)))
	((= k l))
	(push (mean (mapcar #'(lambda (s) (nth k s)) signals)) signal)
	))
  (setf (vset-signal vset) (reverse signal))
  vset)

;;; Just for examination purposes.

(defun pvm ()
  (prog (vset n)
    loop
    (dotimes (k (length *vsets*))
      (format t "~a ~a, " k (length (vset-vois (nth k *vsets*)))))
    (plot* (vset-signal (nth (read) *vsets*)))
    (go loop)))
