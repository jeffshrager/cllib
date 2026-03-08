;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; --- The use command sets up a result-set/study/segment "context"
;;; for the user.  The command loads a study, which is assumed to be
;;; there (see load and save admin commands) and sets (or sets up, if
;;; it's not there) a results directory.  The result-set gets set up
;;; in the indicated location, or else under the *results-top-path*
;;; given in the user's '.fisinit'. It also sets a default segment for
;;; analyses.  There's a lot of dubious smarts in thie command which
;;; try to ensure that the study and segment are right for the
;;; results.

;;; Three globals define a contex.  See globals.lsp for their
;;; defns.
;;;  *s* is the study.
;;;  *r* is the result set (a structure, as below).
;;;  *g* is the segment (of the study) that is represented by this result set.

(defun use-context (context)
  (if (and (series-mode?)
	   (not (yes-or-no-p "There is a series loaded.  The use command will focus on just this one result set.
Are you sure you want to continue with this operation?"))
	   )
      (abort-command "Nothing has changed.")
    (setq *z* ()))
  ;; Unload an old session file if there was one.
  (remember-session-file)
  
  (let ((result-set-name (first context))
	(study-name (second context))
	(segment-name (third context)))
    ;; There are two forms of this command: just the set name, in which
    ;; case we try to get all the info out of a preexisting set, or all
    ;; the information.
    (if (null study-name)
	(load-preexisting-result-set result-set-name
				     (make-result-set-path result-set-name))
        (create-new-context result-set-name study-name segment-name
			    (make-result-set-path result-set-name)
			    )
	))
  (build-condition-graph)
  (clear-voi-tables)
  )

(defun make-result-set-path (name)
  (if (or (stringp name) (eq name 'HERE))
      (if (stringp name) 
	  name
	  (find-current-directory)
	  )
      (if (file-exists? *results-top-path*)
	  (pfn *results-top-path* name)
	(progn
	  (format t "The results top level directory that you have set up in your~%")
	  (format t "preferences (~a) doesn't exist.~%" *results-top-path*)
	  (format t "Be sure to create it, or esle use the pref command to fix the name.~%")
	  (abort-command "No context has been set."))
	)))

(defun create-new-context (result-set-name study-name segment-name path)
    (if (and (file-exists? path)
	     (file-exists? (pfn path "fis.results")))
	(progn
	  (format t "The result set called ~a already exists.  You need to use the~%" result-set-name)
	  (format t "simpler command (use ~a) to use it.  There might be some~%" result-set-name)
	  (format t "disagreement between the study and segment that you have in mind~%")
	  (format t "and the ones that are already given in that preexisting result set.~%")
	  (format t "In this case you'll have to either rename or delete the preexisting~%")
	  (format t "result set before you can use the complete form of the use command.~%")
	  (abort-command "No context was set.")))
    ;; We start with the study and segment and later attatch to the
    ;; result-set.  Either load a new study or, if it's the same as
    ;; the current one, do nothing.
    (cond ((and *loaded-study-name* 
		(not (equal *loaded-study-name* study-name)))
	   (if (yes-or-no-p "Are you really trying to replace study ~a with ~a?"
			    *loaded-study-name* study-name)
	       (load-study study-name)))
	  ((not *loaded-study-name*) (load-study study-name))
	  )
    ;; Next, select a segment.  Note that although the segment
    ;; gets selected from the STUDY structure, we're going to be
    ;; writing all the results into the result-set's directory location.
    ;; This is very complicated!
    (setq *g* (select-segment *s* segment-name))
    ;; Build the result-set: If it's the one we've got, just leave
    ;; well enough alone.  Else ensure that user wants to change, and
    ;; go for it.  This might entail creating a new result.
    (cond ((and *r* (not (equal result-set-name
				(result-set-name *r*))))
	   (if (yes-or-no-p "Really replace result set ~a with ~a?"
			    (result-set-name *r*) result-set-name)
	       (set-up-result-set result-set-name)))
	  ((not *r*) (set-up-result-set result-set-name))
	  )
    ;; And finally, force the user to describe it.
    (format t "Enter a brief description of the study.~%")
    (dump-strings-to-file (pfn (result-set-path *r*) "fis.description") 
			  (read-lines-until-period "Description >"))
    ))

;;; This does most of the dirty work for context setup.  It has to
;;; ensure that there's a results directory for this result, and make
;;; the internal structures for it as well.  It ensures that the
;;; internal result-set structure contains the current results.  We
;;; assume that there is a valid study and segment selection.  If the
;;; result-set's directory already exists, load up its fis.results file
;;; and ensure that it's correct.  If user gives a quoted string, assume
;;; that it's a complete path name, unless it's 'HERE, in which case
;;; use the current directory.

(defun set-up-result-set (name &aux path)
  (setq path (make-result-set-path name))
  (setq *r* (make-result-set :name name
			     :segment-name (segment-name *g*)
			     :study-name *loaded-study-name*
			     :fisversion *fis-version*))
  (if (and (file-exists? path)
	   (file-exists? (pfn path "fis.results"))) ; there must be an easier way!
      (load-preexisting-result-set name path)
      (if (yes-or-no-p "~%The result set called ~a doesn't exist.  Create it?" name)
 	  (create-new-result-set path)
	(progn (clear-context)
	       (abort-comment "No context is set."))
	)
    ))

(defun clear-context ()
  (setq *s* nil *r* nil *g* nil)
  (setq *loaded-study-name* ()))

(defun create-new-result-set (path)
  (setf (result-set-path *r*) path)
  (check-and-create-directory path)
  (format t "Created the directory ~a~%" path)
  (with-open-file (f (pfn path "fis.results") :direction :output)
    (print *r* f))
  (format t "And written out fis.results to it.~%")
  ;; Now create the results and log directories
  (check-and-create-directory (pfn path "results"))
  (check-and-create-directory (pfn path "pals"))
  (include-default-palettes (pfn path "pals"))
  (check-and-create-directory (pfn path "log"))
  ;; Okay, now, if there's an MRA directory, do the complex process of 
  ;; grabbing the appropriate MRA images and making bogus results out
  ;; of them.
  (let ((mrapath (pfn (segment-path *g* "mra"))))
    (if (file-exists? mrapath)
	(transform-mras-into-results mrapath (pfn path "results"))))
  ;; And I think we're done!
  )

;;; This loads up the fis.results file from the indicated location, if
;;; it exists, and then loads the study and sets the named segment.
;;; Need to ensure that there's an fis.results file, esp. since the HERE
;;; arg could have been used, in which case, just the test for the
;;; existence of the dir isn't good enough.

(defun load-preexisting-result-set (name path &key (describe t))
  (if (not (and (file-exists? path) 
		(file-exists? (pfn path "fis.results"))))
      (progn
	(format t "There's no result set called ~a.~%Trying a keyword search.~%~%" name)
	(list-result-sets (list name))
	(abort-command "~%Maybe try one of those?"))
    ) ; if
  (with-open-file (r (pfn path "fis.results") :direction :input)
    (setq *r* (read r)))
  ;; reset the result-set path in case the user moved things
  (setf (result-set-path *r*) path)
  (load-study (result-set-study-name *r*))
  (setq *g* (select-segment *s* (result-set-segment-name *r*)))
  (if describe (describe-result-set))
  (format t "(Results set created by FIS version ~a.)~%" (result-set-fisversion *r*))
  )

;;; Listing results is just like listing studies, except that it uses
;;; a different top path (the results top path instead of the the
;;; topdata path.  Also, in list you can leave out the keys, and
;;; you'll get a description of all your contexts.

(defun list-result-sets (keylist)
  (if keylist
      (study-finder keylist *results-top-path* "results sets")
      (list-all-result-sets)
      ))

(defun list-all-result-sets ()
  (let ((ss (understand-directory *results-top-path*)))
    (dolist (s ss)
      (let ((rd (get-result-hdr (pfn *results-top-path* (file-name s)))))
       (if rd
	(progn
	  (format t "Results set ~a analyses study ~a segment ~a.~%"
		  (result-set-name rd)
		  (result-set-study-name rd)
		  (result-set-segment-name rd)
		  )
	  (if (file-exists? (pfn (result-set-path rd) "fis.description"))
	      (let ((l (with-open-file (f (pfn (result-set-path rd) "fis.description")
					  :direction :input)
				       (read-line f nil nil))))
		(if l (format t "  ~a~%~%" l))
		))
	  ) ; progn
	)))))

(defun get-result-hdr (path)
  (if (file-exists? (pfn path "fis.results"))
      (with-open-file (r (pfn path "fis.results") :direction :input)
		      (read r))))

