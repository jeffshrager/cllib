;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; --- Series mode is a completely different world.  When you enter
;;; series mode, all of the results sets that compose the series are
;;; loaded into the *z* series struct, and then can be processed as a
;;; group.  

;;; Note that the only way to tell what mode you are in is to see if
;;; *z* has contents.  Testing *r* IS NOT ADEQUATE because the series
;;; will often load a result set for processing.

(defun series-mode? () *z*)

;;; Only certain commands operate in series mode.  The first are to
;;; the series command is the name of a new or old series, and the
;;; followed args are the names of result sets that must already
;;; exist.  If the indicated series exists, then results sets are
;;; added to it, otherwise, it is just loaded.

(defun series-handler (args &aux (new t))
  (let ((name (first args))
	(results-to-add (cdr args)))
    (if (null name)
	(list-all-series)
      (progn
	(cond ((series-exists? name)
	       (setq new nil)
	       (rs-series-guard)
	       (load-series name)
	       (describe-series)
	       )
	      ((yes-or-no-p "The series doesn't exist.  Make it anew?")
	       (rs-series-guard)
	       (create-new-series name)
	       )
	      )
	(if results-to-add
	    (progn
	      (mapcar #'add-series-member results-to-add)
	      )
	  )
	;; No need to resave it unless something got added or it's new.
	(if (or new results-to-add) (save-current-series))
	) ; progn
      ) ; if
    ))

(defun list-all-series ()
  (let ((ss (understand-directory *series-top-path*)))
    (dolist (s ss)
      (let* ((file-name (file-name s))
	     (rd (pfn *series-top-path* file-name))
	     (description-file (pfn rd "fis.description"))
	     )
       (if file-name
	     (if (file-exists? description-file)
		 (format t "~a: ~a~%" file-name
			 (with-open-file (f description-file
					    :direction :input)
					 (read-line f)))
	       (format t "~a (no description)~%" file-name)
	       )) ; if
       ))))

;;; When there are results loaded, we need permission to clobber them by
;;; loaded a series.

(defun rs-series-guard ()
  (if *z*
      (if (yes-or-no-p "There is a series loaded.  This will replace it.
Are you sure that you want to do this?")
	  ()
	(abort-command "!! Series operation aborted !!")
	)
    (if *r*
	(if (yes-or-no-p "There are results loaded.  Series operations will unload them from FIS.
Are you sure that you want to do this?")
	    ()
	  (abort-comand "!! Series operation aborted !!")
	  )))
  (setq *r* () *s* ())
  )
      
;;; Just a plain series guard.

(defun series-guard ()
  (if (not *z*)
      (abort-command "Need to have a series loaded.")))

;;; The caller knows that a new series needs to be set up.

(defun create-new-series (name)
  (setq *z* (make-series
	     :name name
	     :path (pfn *series-top-path* name)
	     ))
  (check-and-create-directory (series-path *z*))
  (check-and-create-directory (zlog-dir))
  (initialize-series-log)
  (format t "Enter a brief description of the series.~%")
  (dump-strings-to-file (series-dir "fis.description")
			(read-lines-until-period "Description >"))
  )

(defun save-current-series (&aux file)
  (with-open-file (f (setq file (series-dir "fis.series"))
		     :direction :output :if-exists :supersede)
    (print *z* f))
  (format t "(Saved series info for ~a.)~%" file))

;;; Adding a study to a series involves loaded up the result set and
;;; then saving the results set and study information in the series
;;; results-sets slot as a special srs struct.  The reason that this
;;; all gets saved is that sometimes (e.g., when building mriview save
;;; files) we don't want to have to load up each of these just to get,
;;; e.g., the volume info, which generally doesn't change, so we just
;;; get it from here.  HOWEVER NOTE: USUALLY we *WILL* want to
;;; actually go through the trouble of RELOADING these in order to
;;; ensure that the information is sound.  Eventually, I should just
;;; get the information I need into the srs, rather than saving all
;;; this gunk, but this will do for now.

(defun add-series-member (member-name &aux rs-path)
  (setq rs-path (pfn *results-top-path* member-name))
  (if (file-exists? rs-path)
      (progn
	(load-preexisting-result-set member-name rs-path :describe nil)
	(push (make-srs :result-set *r* 
			:study *s*
			:segment *g*)
	      (series-result-sets *z*))
	(add-member-log-ref member-name))
    (format t "!! The member ~a doesn't appear to exist as a valid result set.
   It is being ignored in the creation of this series !!~%" member-name)
    ))

(defun series-exists? (name)
  (file-exists? (pfn *series-top-path* name)))

;;; --- Series description is simple.

(defun describe-series ()
  (let ((rs* (series-result-sets *z*)))
    (if rs*
	(progn 
	  (format t "The result sets in this series are: ")
	  (dolist (rs rs*)
	    (format t "~a, " (result-set-name (srs-result-set rs)))))
      (format t "No results have been added to this series yet.~%"))
    (format t "~%")
    (describe-outboard 'z (series-name *z*))
    ))
  
;;; To recover a series by name.  (Whether or not really to do this has
;;; already been decided.)

(defun load-series (name &aux (path (pfn *series-top-path* name)))
  (with-open-file (f (pfn path "fis.series"))
    (setq *z* (read f)))
  (format t "(Loaded ~a from ~a.)~%" name path)
  ;; reset the path that we really got the series from.
  (setf (series-path *z*) path)
  )

;;; The series' log is just like a normal general log, except that it
;;; contains a pointer to a ref page which contains pointers to the
;;; logs for each of the member studies.  The reason for the ref page
;;; is that this was we can add new members by appending instead if 
;;; having to go in and complexly hack the log.

(defun initialize-series-log ()
  (let ((primary-log-file (zlog-dir "general.notes"))
	(ref-page (zlog-dir "ref.page"))
	)
    (save-note '("<a href=\"ref.page\">Logs for the member Results Sets</a>")
	       primary-log-file)
    (save-note '("Initialization") ref-page)
    ))

(defun add-member-log-ref (member-name)
  (let ((ref-page (zlog-dir "ref.page"))
	(member-log-file (pfn *results-top-path* member-name "log/general.notes")))
    (save-note (list (format nil "<a href=\"~a\"> ~a's Log</a>" 
			     member-log-file member-name))
	       ref-page)
    ))

;;; --- Calling mriview from series mode automatically loads up a
;;; temporary save file with one group per results in the series.
;;; If you give an arg, that overrides the series file default.

(defun view-series (args)
  (if (null args) (create-viewer-save-file (/tmp "vs")))
  (system! "cd ~a;/users/mri/alphabin/mriview ~a &" 
	   (series-dir)
	   (let ((nname (if args (case-blind-find-file (series-dir "*") (car args)))))
	     (if nname 
		 (series-dir nname)
	       (/tmp "vs"))
	     ))
  )

(defun create-viewer-save-file (file &aux (rsk 0))
  (with-open-file (f file :direction :output
		          :if-exists :supersede)
    (dbwrite f "defaults.paletteDir" *palette-path*)
    (dbwrite f "defaults.initialSliceView" 1)
    (dbwrite f "groupcount" (length (series-result-sets *z*)))
    (dolist (srs (series-result-sets *z*))
      ;; Temporarily set a context.
      (setq *s* (srs-study srs))
      (setq *r* (srs-result-set srs))
      (setq *g* (srs-segment srs))
      (dbwrite f (format nil "group.~a.name" rsk) (result-set-name *r*))
      (dbwrite f (format nil "group.~a.volumedir" rsk) (volume-dir))
      (dbwrite f (format nil "group.~a.resultsdir" rsk) (results-dir))
      (dbwrite f (format nil "group.~a.inplanedir" rsk) (inplanes-dir))
      (incf rsk)
      )
    )
  )

(defun dbwrite (stream var value)
  (format stream "~a = ~a~%" var value))

;;; Listing series is just like listing studies, except that it uses
;;; a different top path (the series top path instead of the the
;;; topdata path.  Also, in list you can leave out the keys, and
;;; you'll get a description of all your series.

(defun find-series (keylist)
  (if keylist
      (study-finder keylist *series-top-path* "series")
      (list-all-series)
      ))

(defun list-all-series ()
  (let ((ss (understand-directory *series-top-path*)))
    (dolist (s ss)
      (let ((rd (get-series-hdr (pfn *series-top-path* (file-name s)))))
       (if rd
	(progn
	  (format t "Series ~a ( " (series-name rd))
	  (dolist (srs (series-result-sets rd))
	    (format t "~a, " (result-set-name (srs-result-set srs))))
	  (format t ")~%")
	  (if (file-exists? (pfn (series-path rd) "fis.description"))
	      (let ((l (with-open-file (f (pfn (series-path rd) "fis.description")
					  :direction :input)
				       (read-line f nil nil))))
		(if l (format t "  ~a~%~%" l))
		))
	  ) ; progn
	)))))

(defun get-series-hdr (path)
  (if (file-exists? (pfn path "fis.series"))
      (with-open-file (r (pfn path "fis.series") :direction :input)
		      (read r))))

;;; --- Meta-Analyses carried out on studies in a series.  This
;;; function calls several other fns, given as args, in sequence.
;;; These are:
;;;
;;;  initor -- initializes the operation; generally clears a collection var
;;;  collector -- gets called once for each member study in the series;
;;;               generally pushes something gather from the member into the
;;;               collection var
;;;  computor -- does whatever post-collection calculation should happen to the
;;;              collection var.
;;;
;;; Each of these is called with the args as a single arg.

(defun meta-process (initor collector computor args &key (understand-results nil))
  (format t "(I never meta process is didn't like!)~%")
  (funcall initor args)
  (dolist (srs (series-result-sets *z*))
      (let* ((result-set (srs-result-set srs))
	     (name (result-set-name result-set))
	     (path (result-set-path result-set)))
	(load-preexisting-result-set name path :describe nil)
	(if understand-results (understand-existing-results :use-cache nil))
	(funcall collector args)
	))
  (funcall computor args)
  )

;;; The general meta command.

(defun meta-command (args)
  (series-guard)
  (case (pop args)
    (vmap (let ((label (pop args)))
	    (if (or (not (symbolp label))
		    (not (numberp (setq *vs-inbrain-gather-limit* (pop args))))
		    (not (numberp (setq *vs-inbrain-exclude-limit* (pop args))))
		    (not (numberp (setq *vs-xbrain-gather-limit* (pop args))))
		    )
		(abort-command "Requires: label, min limit, max limit, xbrain-gather limits."))
	    (if *mcvbl*
		(if (yes-or-no-p "Recollect voi sets?")
		    (collect-vois-by-label label)
		  (format t "Old sets retained.~%"))
	      (collect-vois-by-label label))
	    (print (list '*vs-inbrain-gather-limit* *vs-inbrain-gather-limit*))
	    (print (list '*vs-inbrain-exclude-limit* *vs-inbrain-exclude-limit*))
	    (print (list '*vs-xbrain-gather-limit* *vs-xbrain-gather-limit*))
	    (terpri)
	    (vmap *mcvbl*)
	    )
	  )
    (t (abort-command "Invalid meta command.")))
  )

;;; --- Meta command to collect vois and compare them.  The user has
;;; to give a label which must appear as a voi set label in each
;;; element result set in the series.  Association of one voi to
;;; another is based upon co-locality in t.space.  

(defun collect-vois-by-label (label)
  (if label
      (meta-process 'mcvbl-init 'mcvbl-collect 'mcvbl-compute label
		    :understand-results t)
    (abort-command "You must give a label for this meta command.")
    ))
  
(defvar *mcvbl* ())

(defun mcvbl-init (ignore)
  (setq *mcvbl* ()))

(defun mcvbl-collect (label)
  (format t "(Gathering the voi sets called ~a from each member result set.)~%"
	  label)
  (load-voi-set-by-label label) 
  (push *vois* *mcvbl*)
  )

(defun mcvbl-compute (ignore)
  (format t "(~a voi sets were gathered.)~%" (length *mcvbl*))
  )
