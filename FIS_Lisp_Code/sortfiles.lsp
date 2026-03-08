;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- Tries to understand the various files that are brought over
;;; from the scanner.

;;; --- Check everything in the study and move files as needed.  We assume
;;; that there is a pfile directory and a structurals directory under
;;; the named brain directory.  Everything else needs to be checked and
;;; and things from the top level pfiles and structurals directories
;;; sorted into the right places in the segments.  

;;; In the case of EPI, segment-pfiles will be the location of the 
;;; EPI images, which have to get renamed by linking into the functionals
;;; directory.  For the time being, we do NOTHING now;  This is done by
;;; the reconstruction process, which is a tiny bit bogus, but works.

(defun check-and-sort-everything (&optional (study *s*))

  ;; First make sure that there's a segment directory for each segment, and 
  ;; a log directory for each as well.
  (mapcar #'(lambda (segment) 
	      (check-and-create-directory 
	       (segment-path segment))
(print 'bug!)
;	      (init-logging-for-a-new-study segment)
	      )
	  (study-segments study))

  ;; Now sort the pfiles. These are initially all loaded up into the top
  ;; level pfile directory. We need to move them all to the appropriate
  ;; segment pfile directory. (Except in EPI, but that decision is taken
  ;; below this level.)
  (sort-out-pfiles study) ; this actually creates the links for pfiles

  ;; Next sort the structurals and mras.
  ;; (Note that mra postprocessing is done when a user actually
  ;;  does a use command to make a results directory.)
  (sort-structurals study)

  ;; Now, since we often end up doing the analysis and then moving 
  ;; the results to segment directories, we need to check for a /out directory
  ;; in the top and offer to link it in.
  (link-in-out-allover-or-not study)

)

;;; --- The /out is usually created by the recon-and-invert process, but often
;;; we just have a single segment, and the /out is created by the sysadmin
;;; before we get a chance to worry about it.  In this case, we can just link
;;; the top-level /out into each segment.

(defun link-in-out-allover-or-not (study)
  (let ((topoutdir (pfn (study-brain-dir study) "out")))
    (if (file-exists? topoutdir)
	(if (yes-or-no-p "An top-level /out directory exists.  Would you like to 
have it automatically linked into each segment directory?")
	    (mapcar #'(lambda (seg) (link-out-into topoutdir seg))
		    (study-segments study)))
	  (format t "Okay, no linking of /out was done.  You'll have to use recon to create /out's for each segment.~%"))))

(defun link-out-into (topoutdir seg)
  (check-and-link-in (segment-path seg "out") topoutdir))

;;; --- Pfiles.  The pfile-to-segment and map-to-segment associations
;;; were made in the fis.trials file.  What we need to do now is to sort
;;; through the pfiles and mapfiles and link them all into the correct
;;; segments pfile directories.

(defun sort-out-pfiles (study)
  (mapcar #'(lambda (segment) 
	      (if (not (epi-segment segment))
		  (sort-out-pfiles-in-segment 
		     segment 
		     (study-brain-dir study)
		     (pfn (study-brain-dir study)
			  (study-pfile-dir study)))))
	  (study-segments study)))

(defun sort-out-pfiles-in-segment (segment brain-dir top-pfile-dir)
  (let ((segment-pfile-dir (pfn brain-dir
			     (segment-dir segment)
			     (segment-pfile-dir segment))))
  (check-and-create-directory segment-pfile-dir)
  (dolist (trial (segment-trials segment))
    ;; link in the map file (if there is one)
    (if (trial-mapfile trial)
        (check-and-link-in (pfn segment-pfile-dir (trial-mapfile trial))
			   (pfn top-pfile-dir (trial-mapfile trial))))
    ;; link in the pfile
    (if (not (string-equal "no-pfile" (trial-pfile trial)))
	(check-and-link-in (pfn segment-pfile-dir (trial-pfile trial))
			   (pfn top-pfile-dir (trial-pfile trial)))
      )
    )))

;;; --- Structural interpretations.  This is harder to interpret than the
;;; pfiles because there's not a one-to-one correspondence for trials.
;;; So we need to ask the user about everything all the time.  Note
;;; that we assume that everything is in a top level directory called
;;; "structs" (or whatever's in the study-structural-dir slot), but
;;; this isn't the way that Mark does things at the moment, so someone
;;; has to create this directory by hand.  Another problem here is
;;; that the structural directories are numbered, as: 004, and
;;; understand-directory returns them as fixnums so they have to be
;;; reconverted into appropriate atoms.  (This is sort of a bug in
;;; understand-directory.)

;;; Sorting the structurals is a lot harder and requires more user
;;; intervention.  The problem is that the structurals have to be sort
;;; of guessed at about what they are (although we give a bit of a
;;; hint when possible), and also the same set of structurals can apply
;;; to many (sometimes all!) trials.  So we run through the segments and
;;; let the user choose structs to go into each inplane and volume dir from
;;; all possible structurals.

;;; (Note that we have assumed all along that the structurals are relocated into
;;;  a brain/structs directory before this takes place.  That's NOT our current
;;;  setup, so you might have to do that by hand first!)

;;; << A lot of this is redundant and ought to be collapsed into calls
;;; to a subfun. >>

(defun sort-structurals (study &aux structs)
  (let* ((braindir (study-brain-dir study))
	 (topstructdir (pfn braindir (study-structural-dir study)))
	 (topstructurals (understand-directory topstructdir))
	 )

    ;; Run through each structural dir and "understand" *it* recursively.

    (dolist (d topstructurals)
      (push (understand-structural-directory topstructdir d) structs))
    (setq structs (sort structs #'(lambda (a b) (< (car a)  (car b)))))

    ;; Now allow the user to make assignments and link things in.

    (dolist (segment (study-segments study))
      (let* ((segment-dir (pfn braindir (segment-dir segment)))
	     (voldir (pfn segment-dir (segment-volume-dir segment)))
	     (inplanesdir (pfn segment-dir (segment-inplanes-dir segment)))
	     (mradir (pfn segment-dir "mra"))
	     )

	;; Inplane assignment.

	(if (file-exists? inplanesdir)
	    (format t "An inplane link exists for segment ~a.~%" 
		    (segment-number segment))
  	    (let* ((nslices (prescription-slices (segment-prescription segment)))
		   (possible-inplanes
		    (let (l) (dolist (f structs l)
			       (if (= (third f) nslices) (push f l)))))
		   )

	      (setq possible-inplanes 
		    (sort possible-inplanes 
			  #'(lambda (a b) (string< (second a) (second b)))))

	      (if possible-inplanes
		  (progn
		    (format t "One of these is the inplanes for segment ~a, choose by the number at left.~%"
			    (segment-number segment))

		    (do ((n 1 (1+ n))
			 (l possible-inplanes (cdr l)))
			((null l))
		      (format t "~a. Structural series ~a containing ~a images.~%"
			      n (second (car l)) (third (car l))))
	    
		    (let ((f (second (nth (1- (prompt-for-number "Select one: " nil)) 
					  possible-inplanes))))
		      (check-and-link-in inplanesdir
					 (pfn topstructdir f)))
		    )
		(format t "There don't appear to be any possible inplanes for this condition.~%")
		) ; if possible-inplanes

	      )) ; let*, if
	    
	;; Volume assignment.

	(if (file-exists? voldir)
	    (format t "A volume link exists for segment ~a.~%"
		    (segment-number segment))

	    ;; Need some kind of heuristic for volumes.
	    ;; Sometimes you want to use scouts as volumes. (**)

	    (let* ((possible-volumes 
		    (let (l) (dolist (f structs l) 
				   ;; dummy test, as above (**)
			       (if (> (third f) 0) (push f l)))))
		   )
	      (if possible-volumes
		  (progn 
		    (format t "One of these is the volume for segment ~a, choose by the number at left.~%"
		      (segment-number segment))

		    (setq possible-volumes
			  (sort possible-volumes
				#'(lambda (a b) (string< (second a) (second b)))))

		    (do ((n 1 (1+ n))
			 (l possible-volumes (cdr l)))
			((null l))
			(format t "~a. Structural series ~a containg ~a images.~%" 
				n (second (car l)) (third (car l)))
			)
	    
		    (let ((f (second (nth (1- (prompt-for-number "Select one: " nil)) 
					  possible-volumes))))
		      (check-and-link-in voldir
					 (pfn topstructdir f)))
		    ) ; progn
		  (format t "There don't appear to be any possible volumes for this condition.~%")) ; if
	      )) ; let*, if
	    
	;;; --- MRA assignment.  Note that the MRA is a hack in many ways.  Firstly,
        ;; it's not in the segment header as a first class slot, but rather is
        ;; stuck in on the side.  This ought to be fixed.  Second, the way
	;; that mras work is that they are reconstituted as a result, but we can't
        ;; do this until the user does a use command, and so has a results dir.
        ;; So for now we just link in an mra to the segment subdir and let 
	;; code in the use command worry about that.

	(if (file-exists? mradir)
	    (format t "An MRA link exists for segment ~a.~%"
		    (segment-number segment))

	    ;; The 

	    (let* ((nslices (prescription-slices (segment-prescription segment)))
		   (possible-mras
		    (let (l) (dolist (f structs l) 
			       (if (= (third f) (1+ (* 2 nslices)))
				   (push f l)))))
		   )
	      (if possible-mras
		  (progn 
		    (format t "One of these is the MRA for segment ~a, choose by the number at left.~%"
			    (segment-number segment))
		    
		    (setq possible-mras
			  (sort possible-mras
				#'(lambda (a b) (string< (second a) (second b)))))
		    
		    (do ((n 1 (1+ n))
			 (l possible-mras (cdr l)))
			((null l))
			(format t "~a. Structural series ~a containg ~a images.~%" 
				n (second (car l)) (third (car l)))
			)
	    
		    (let ((f (second (nth (1- (prompt-for-number "Select one: " nil)) 
					  possible-mras))))
		      (check-and-link-in mradir
					 (pfn topstructdir f)))
		    ) ; progn
		(format t "There don't appear to be any possible mras for this condition.~%")
		) ; if possible-mras
	      ) ; let*
	    ) ; if file-exists
	)) ; let*, dolist
    )) ; let* defun

(defun understand-structural-directory (pathto dir &aux (nsfiles 0))
  (let ((sfiles (understand-directory (pfn pathto (file-name dir)))))
    (list (file-time dir) (file-name dir)
	  ;; 1- so as not to count the SERHDR file
	  (1- (length sfiles))
	  )))

	  
			
	   
       
	    
