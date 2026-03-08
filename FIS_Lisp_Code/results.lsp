;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- Handler for statistical output in the .../results directory.
;;; --- Also, various condition selection and checking code are here.
;;; --- Also, delete, tag, and mra processing.

;;; When there are lots of results, this can take a long time, and is
;;; called over and over.  Ought to be some internal caching for it.
;;; This cache is used to save time; It gets loaded by
;;; understand-existing-results, and then various other callers tell
;;; us to use the cache instead of redoing the undertanding step all
;;; the time.

(defvar *results-cache* ())

;;; --- Analyze, list, select, the results in the context set.

(defun understand-existing-results (&key (use-cache t))
  (if (and *results-cache* use-cache)
      *results-cache*
    (progn
      (setq *results-cache* (uer2))
      (gather-label-information)
      *results-cache*
      )))

(defun uer2 (&aux results)
  (format t "(The results cache is being updated.)~%")
  (let ((files (mapcar #'(lambda (file) (cons (file-name file) 
					      (parse-string (file-name file) 
							    :break-char #\.)))
		       (understand-directory (results-dir))))
	)
    (dolist (file files)
      (let ((result (find (get-result-parts (cdr file))
			     results
			     :test #'check-same-result)))
	(if result
	    (push (car file) (result-allfiles result))
	    (let ((parts (get-result-parts (cdr file))))
	      (push (make-result
		      :name (first parts)
		      :over (second parts)
		      :stat (cddr parts)
		      :allfiles (list (car file))
		      )
		    results)
	      )
	    )))
    )
  results)

(defun check-same-result (new-list old-result)
  (and (equal (first new-list) (result-name old-result))
       (equal (second new-list) (result-over old-result))
       (equal (cddr new-list) (result-stat old-result))))

;;; Remove the mri/dat identifier and atomize all the parts.

(defun get-result-parts (l)
  (mapcar #'read-from-string (detailnd l)))

;;; This has to be non-destructive since the caller might like its
;;; data intact.  These are short lists, so it's not a big deal.

(defun detailnd (l)
  (do ((m (setq l (copy-list l)) (cdr m)))
      ((null (cddr m))
       (rplacd m nil)))
  l)

;;; 

(defun display-result (c)
  (format t "~a " (result-input-form c))
  (if (result-labels c)
      (format t "~20t~a" (result-labels c)))
  (format t "~%")
  )

;;; The short form is the way that the result can be referred to
;;; elsewhere.  This is somewhat overcomplex.  There are three forms
;;; being bandied about: the result structure carries the various
;;; pieces, including a "tail" called the "stat" which represents
;;; anything after the third ".", so that in
;;; t3.avg.s03.p.foo.bar.baz, the "stat" slot gets '(p foo bar baz), as
;;; ATOMS!  This enables us to understand the results without
;;; worrying abouy capitalization or order of entry.  Now, the USER
;;; refers to results by all but the slice numbers, so the above
;;; could be referred to by the user (that is, in "input form") as any
;;; of these: foo.baz.bar.avg.p.t3, t3.p.avg.baz.bar.foo,
;;; T3.aVg.BaZ.fOO.BAR.P, etc. There's a lot of in-and-out hereafter
;;; to enable us to read and intertranslate these forms.  The
;;; canonical form is called "short form", which is just an unordered
;;; list of atoms, so that the above becomes, in sort form: '(foo bar
;;; baz p avg t3) [order isn't relevant in these tools].

(defun result-input-form (c &aux s)
  (setq s (format nil "~a.~a" (result-name c)(result-over c)))
  (dolist (stat (result-stat c))
    (setq s (format nil "~a.~a" s stat)))
  (read-from-string s))

(defun result-input-form-to-short-form (if)
  (mapcar #'read-from-string (parse-string (format nil "~a" if) :break-char #\.)))

(defun result-short-form (c)
  (cons (result-name c) (cons (result-over c) (result-stat c))))

(defun same-result-short-forms? (c1 c2)
  (cond ((null c1) (null c2))
	((member (car c1) c2)
	 (same-result-short-forms? (cdr c1) (remove (car c1) c2)))))

(defun tell-possible-input-results ()
  (format t "Input results must be one of these forms (item order is irrelevant): ~%")
  (setq rpl 3)
  (dolist (c (understand-existing-results))
    (format t " ~a " (result-input-form c))
    (if (zerop rpl) 
	(progn (setq rpl 3)
	       (format t "~%"))
        (decf rpl)))
  (format t "~%"))

;;; This checks a result against the possible results.  In order to save
;;; a TON of time, there is an option to use the cache made by another call
;;; to understand-existing results instead of redoing it every time!

(defun find-input-result (result-in-input-form &key (use-cache t))
  (setq result-in-input-form (result-input-form-to-short-form 
				 result-in-input-form))
  (dolist (c (understand-existing-results :use-cache use-cache) nil)
    (if (same-result-short-forms?
	   result-in-input-form 
	   (result-short-form c))
	(return c))))

;;; Select-results enables the user to enter a result or list of results
;;; in standard input form and returns the selection or nil if none were
;;; selected.  It gives the user the list of possible results.

(defun select-results (&aux result)
  (tell-possible-input-results)
  (prog (stop input-forms)
    loop
        (if stop (return result))
        (setq input-forms 
	      (mapcar #'read-from-string (parse-string
	       (prompt-for-string "Enter a result or results in input form from the above list (or none):"))))
	(if (eq 'none (car input-forms))
	    (return nil)) ; quit the loop, no result set
	(dolist (inform input-forms)
		(if (not (find-input-result inform :use-cache t))
		    (progn (format t "Some names results couldn't be found.  Did you use standard input form?~%Try again or type none and read the help for results.~%")
			   (go loop))))
	(setq result input-forms)
	)
  result
  )

;;; Verify results takes a list of results and makes sure that every
;;; one in the list is valid.  If any of them fail, then the entire
;;; list is replaced with a call to select-results.

(defun verify-results (results)
  (dolist (r results results)
    (if (not (find-input-result r))
	(return (select-results))
      )))

;;; Some programs, for reasons beyond me, absolutely require a
;;; wildcard filename.  Esp. orthoview, which at the moment requires
;;; as its last arg NOT the filenames (which it would get if you
;;; happened to be in the directory with the results) but a partially
;;; specified filename as a string, with * in the approporiate place.
;;; Therefore, we need to be able to convert from a result input for
;;; into the appropriate wildcard qualified filename.  Ugh.  This
;;; works in a sort of clever way.  It starts with the first filename
;;; and works its way through the list, char-by-char anding them
;;; together.  When the chars do not match, it replaces these with a
;;; ?.  (smash-strings does this dirty work.) If the strings are the
;;; same length, this will work.  If they aren't, we're gonna be
;;; screwed; really this ought to reset on #\., but screw that for
;;; orthoview idiocy; it's going in the trash soon anyhow.

(defun result-to-wildcard-filespec (result)
  (let* ((fnames (result-allfiles result))
	 (name (format nil "~a" (pop fnames))) ; format is used to copy here
                                               ; so that we don't muck up the
	                                       ; results structure in smashing
	 )
    (dolist (f fnames)
      (smash-strings f name))
    name))

(defun smash-strings (source target)
  (dotimes (k (length source))
    (if (not (equal (aref source k) (aref target k)))
	(setf (aref target k) #\?))))

;;; --- Delete all the files that compose a result, given a result
;;; spec that indicates target info about the result.  

(defun delete-results (target args)
  (case target
    ((c cond n name)
     (mapcar #'delete-result 
       (mapcan #'find-results-by-condition args)))
    ((r range o over t target)
     (mapcar #'delete-result 
       (mapcan #'find-results-by-target args)))
    ((s stat a analysis anal)
     (mapcar #'delete-result 
       (mapcan #'find-results-by-stat args)))
    (t (abort-command "Delete must be followed by: (c)ond, (r)ange, (o)ver, or (s)tat, and args."))
    ))

(defun delete-result (result &aux name)
  (if (yes-or-no-p "Really delete ~a?" (setq name (result-input-form result)))
      (progn
	(mapcar #'(lambda (f) (delete-file (results-dir f)))
		(result-allfiles result))
	(rlog! 'delete "~a" name))
    ))

;;; Each of these returns a list of all the results that match the
;;; given arg, or nil.

(defun find-results-by-condition (cond &aux return)
  (let ((r* (understand-existing-results)))
    (dolist (r r* return)
      (if (or (eq cond (result-name r))
	      (eq '* cond))
	  (push r return)))))

(defun find-results-by-target (cond &aux return)
  (let ((r* (understand-existing-results)))
    (dolist (r r* return)
      (if (or (eq cond (result-over r))
	      (eq '* cond))
	  (push r return)))))

(defun find-results-by-stat (cond &aux return)
  (if (atom cond) 
      (progn (format t "Warning: ~a is being considered as a separate statistical descriptor. ~%You might have intended to parenthesize your args.~%See '?del' for help.~%" cond)
	     (setq cond (list cond))))
  (let ((r* (understand-existing-results)))
    (dolist (r r* return)
      (if (dolist (c cond t)
	    (if (not (member c (result-stat r))) 
		(return nil)))
	  (push r return)))))

(defun summarize-delete-log-entry (rec)
  (format t "~a~%" rec))

;;; --- This transforms the MRA images in results.  There is always
;;; one more image than the number of slices in the prescription,
;;; times 2.  So, an 18 slice prescription produces 1+(2*18), or 37
;;; images.  The first image is always the sum image.  It gets left
;;; where it is.  The next nslices (e.g., 18) are the mra image.
;;; These are the ones we're looking for.  So what we do is simply to
;;; update a counter and slide 'em on over.  This doesn't even have to
;;; really understand the directory, as it oughta just look exactly
;;; like we're expecting it to.  (Maybe there ought to be some error
;;; checking someday.)

(defun transform-mras-into-results (mrapath resultspath)
  (let ((nslices (prescription-slices (segment-prescription *g*))))
    (dotimes (i nslices)
       (check-and-link-in (format nil "~a/mra.inplanes.s~2,'0d.all" resultspath (1+ i))
			  (format nil "~a/I.~3,'0d" mrapath (+ i 2))
			  :dont-ask t)
       )))

;;; --- Labeling (or "calling" as it's called because you say 'call
;;; result <result> <label>' enters a log entry indicating that the
;;; result has the given name.  The label table is then built by the
;;; describe processor on result set load or whatever.  Through this
;;; mechanism the relationship between results and labels is many to
;;; many, which may or may not be such a great idea.  Also, since the
;;; label information is maintained in the log index and is rebuilt
;;; each time, it relies upon the NAME of the result, which is
;;; actually probably okay since results SHOULDN'T really change if
;;; they are recreated; anyhow, there's no other way to tell this
;;; without doing some kind of fancy checking.

(defun label-result (result-name label)
  (let ((result (find-input-result result-name)))
    (if result 
	(progn
	  (assign-result-label result label)
	  (log! 'label-result "result-input-name ~a label ~a"
		result-name label)
	  )
      (abort-command "No valid result was indicated.")
      )))

(defun assign-result-label (result label &optional silently)
  (let* ((label-string (symbol-name label))
	 (+-char (aref label-string 0))
	 (real-label (read-from-string (string-left-trim "+-" label-string))))
    (if (eq #\- +-char)
	(if (member real-label (result-labels result))
	    (setf (result-labels result)
		  (remove real-label (result-labels result)))
	  (if (not silently)
	      (abort-command "The label ~a wasn't assigned to ~a." real-label
			     (result-input-form result)))
	  )
      (pushnew real-label (result-labels result))
      )
    ))

(defun summarize-label-result-log-entry (entry)
  (print entry))

(defun gather-label-information ()
  (format t "(Gathering label information.)~%")
  (let ((label-entries (find-log-entries 'label-result)))
    (mapcar #'(lambda (e) 
		(let ((data (nthcdr 3 e)))
		  (assign-result-label 
		   (find-input-result (getf data 'result-input-name))
		   (getf data 'label)
		   'silently)
		  )
		) ; lambda
	    label-entries) ; mapcar
    ))

;;; For recovering results that have a particular label.

(defun find-labeled-results (label &aux r)
  (dolist (result *results-cache* r)
    (if (member label (result-labels result))
	(push result r)))
  )
