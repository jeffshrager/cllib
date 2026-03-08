;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; This drives both reconstruct and invert.  It checks to see what's
;;; done and tries not to redo work.  If we're gonna do both
;;; reconstruction and inversion we use an external script which can
;;; do them both in sequence, that enables the batch stream to do them
;;; one after the other without us having to wait around and sequence
;;; them ourselves.

(defun reconstruct-and-invert (study segment &aux dor doi)

  ;; There's a bunch of interactive error checking that has to happen
  ;; here eventually in case reconstruction and inversion have or 
  ;; haven't been fully completed, or both or neither, but it's all
  ;; too complicated.

  (create-script study segment)
  (fire-off-r&i study segment)
  
  ;; Tell advise what's up.

  (setq *hint* (list 'recon segment))
  )

;;; This dumps the internally-named conditions into a script file for
;;; invert.  When this happens we log it, under the assumption that
;;; we're about to do an invert.  The script gets copied into the log
;;; directory also.

(defun create-script (study segment)
  (let ((script-file (segment-path segment "script")))
    (with-open-file (script script-file :direction :output
			                :if-exists :supersede)
      (dolist (trial (segment-trials segment))
        (format script "~a 0 0 0 0 0 0 0 0 ~a 0 0 0~%"
		(trial-number trial)
		(trial-condition trial))))

    ;; (Note that we *could* use the script.log file as the realscript
    ;;  file instead of using the file called "script" in the segment
    ;;  directory.)

    (let* ((log-id (random 10000))
	   (script.log-file (log-dir segment (format nil "script.~a" log-id)))
	   )
      (system! "cp ~a ~a" script-file script.log-file)
      (log! segment 'script-written "~a" script.log-file)
      )))

(defun fire-off-r&i (study segment)
  (let* ((study-brain-dir (study-brain-dir study))
	 (segment-dir (segment-dir segment))
	 (functional-slices-dir (segment-functional-slices-dir segment))
	 (segment-pfile-dir (segment-pfile-dir segment))
	 (functional-full-path (pfn study-brain-dir
				    segment-dir
				    functional-slices-dir))
	 (pfile-full-path (pfn study-brain-dir
			       segment-dir
			       segment-pfile-dir))
	 (invert-full-path (pfn study-brain-dir segment-dir 
				(segment-invert-output-dir segment)))
	 (log-id (random 10000))
	 )
  (check-and-create-directory functional-full-path)
  (check-and-create-directory invert-full-path)
  (case (segment-scan-type segment)
    (spiral
     (system! "~a/spiral-recon-and-invert ~a ~a ~a ~a ~a ~a &" 
	      *home*
	      functional-full-path
	      (pfn *param-file-dir* 
		   (prescription-param-file (segment-prescription segment)))
	      pfile-full-path
	      (segment-path segment)
	      invert-full-path
	      log-id
	      ))
    (epi
     (epi-recon (segment-pfile-dir segment)
		functional-full-path
		(prescription-samples (segment-prescription segment))
		(prescription-slices (segment-prescription segment)))
     
     (system! "~a/invert ~a ~a ~a ~a ~a ~a &" 
	      *home*
	      functional-full-path
	      (pfn *param-file-dir* ;unused for epi
		   (prescription-param-file (segment-prescription segment))) ;unused for epi
	      pfile-full-path ;unused for epi
	      (segment-path segment)
	      invert-full-path
	      log-id
	      )
     )
    (t (break "Something's wrong; Neither spiral nor EPI was indicated!"))
    )
  (log! segment
	'recon-and-invert-started
	"logfile-is \"recon.log.~a\"" 
	log-id)
  ))
	   
;;; Completion of recon and inversion are indicated by the existance of
;;; recon.complete and invert.complete files, which are erased at the
;;; beginning of the recon-and-invert script, and are created when
;;; it finishes.

(defun rdone? (study segment)
  (let ((recon.log (log-dir segment (find-latest-recon.log-file segment))))
  (if (file-exists? recon.log)
      (rd?2 study segment)
      (progn (format t "There's no recon.log file.  Maybe you didn't start a recon?~%")
	     ()))))

(defun rd?2 (study segment)
  (file-exists? (segment-path segment "recon.complete")))

(defun idone? (study segment)
  (file-exists? (segment-path segment "invert.complete")))

;;; This is the interactive version of completion checking.

(defun check-rdone (study segment)
  (let ((rdone (rdone? study segment))
	(idone (idone? study segment))
	)
    (if rdone 
	(format t "Reconstruction is complete!~%")
        (progn (format t "Reconstruction is NOT complete!~%")
	       (grep-for-recon segment)
	       (report-recon-eta segment)))
    (if idone 
	(progn (format t "Inversion is complete!~%")
	       (setq *hint* (list 'recondone segment)))	       
        (progn (format t "Inversion is NOT complete!~%")
	       (setq *hint* (list 'reconnotdone segment))))
    ))

;;; This version waits instead of saying anything.

(defun wait-for-recon-completion (study segment &aux rcomp icomp)
  (prog (rdone idone)
	(setq rcomp (rdone? study segment))
	(setq icomp (idone? study segment))
    loop
        (if (and rcomp icomp) 
	    (return (progn (format t "All phases of reconstruction are completed!~%")
			   (setq *hint* (list 'recondone segment))
			   (return nil))))
	(pause 1000)
	(if (not rcomp)
	    (if (rdone? study segment)
		(progn (format t "Reconstrution is done.~%")
		       (setq rcomp t))
	      ))
	(if (not icomp)
	    (if (idone? study segment)
		(progn (format t "Inversion is done.~%")
		       (setq icomp t))
	      ))

	(go loop)))

;;; this is just a shortcut for the user having to know where the log file is
;;; and grep for complete raw files.

(defun grep-for-recon (segment)
  (let ((le (find-log-entries segment 'recon-and-invert-started)))
    (if le 
	(progn 
	  (system! "grep -i raw ~a" (log-dir segment (find-latest-recon.log-file2 le)))
	  (format t " (The first file above is an inhomogeneity map and doesn't count.)~%")
	  )
      )))

;;; This reads the log file and looks for the latest reconstruction log file's name.

(defun find-latest-recon.log-file (segment)
  (let ((le (find-log-entries segment 'recon-and-invert-started)))
    (if le (find-latest-recon.log-file2 le))))

(defun find-latest-recon.log-file2 (le)
  (format nil "~a" (fifth (car (last le)))))

;;; The user can change the condition names associated with a segment
;;; though the "cond" (condition) command, which is handled here.  It
;;; is also necessary to reinver the segment if the condition names are 
;;; changed, although we offer that as an option.

(defun change-condition-names-and-reinvert (segment &aux renamings k)
  (format t "Enter new condition names, or no entry to leave the name as is.~%")
  ;; we save these up in renamings and then go through and put them in only
  ;; if the user gives us the ok.
  (setq k 0)
  (dolist (trial (segment-trials segment))
    (push (cons trial 
		(let ((s (get-string-w-default 
			  (format nil "New condition name for trial ~a" 
				  (incf k)
				  (trial-condition trial))
			  (trial-condition trial))))
		  (if (stringp s)
		      (read-from-string s)
		      s)
		  ))
	  renamings))
  (format t "Here is the renaming you have chosen:~%")
  (setq k 0)
  (dolist (item (reverse renamings))
     (format t "Trial ~a, Old Condition: ~a, " 
	     (incf k)
	     (trial-condition (car item)))
     (if (eq (trial-condition (car item))
	     (cdr item))
	 (format t "Not renamed.~%")
         (format t " -> ~a.~%" (cdr item)))
     )
  (if (yes-or-no-p "Is this all correct?")
      (apply-renamings renamings)
      (abort-command "Renaming aborted; Everything is left as it was."))
  (if (yes-or-no-p "You should do a new inversion step.  Should I do this for you now?")
      (invert-only segment)
      (format t "No invert was started.")
      )
  (format t "** Warning: If you don't save the study again, the new condition names will be lost!~%")
  (if (yes-or-no-p "Would you like to save the study now?")
      (save-study nil)
      (format t "Not saved."))
  )

;;; This actaully does the renaming; whereas above we just got all the
;;; names together.

(defun apply-renamings (renamings)
  (dolist (r renamings)
    (setf (trial-condition (car r)) (cdr r))))

;;; This can be used to just do an invert, which happens with epi or when
;;; conditions are renamed.

(defun invert-only (segment)
  (create-script *s* segment)
  (let* ((study-brain-dir (study-brain-dir *s*))
	 (segment-dir (segment-dir segment))
	 (functional-slices-dir (segment-functional-slices-dir segment))
	 (segment-pfile-dir (segment-pfile-dir segment))
	 (functional-full-path (segment-path segment functional-slices-dir))
	 (invert-full-path (segment-path segment
					 (segment-invert-output-dir segment)))
	 (log-id (random 10000))
	 )
    (system! "~a/invert ~a ~a ~a ~a ~a ~a &" 
	     *home*
	     functional-full-path
	     "unused"
	     "unused"
	     (segment-path segment)
	     invert-full-path
	     log-id
	     )
    (log! segment
	  'separate-invert-started
	  "logfile-is \"invert.log.~a\"" 
	  log-id)
    ))

;;; Estimate when reconstruction will be done.  This is a sort of
;;; hack; it counts up the output messages generated by reconstruction
;;; each time that process begins processing a raw file.  Also, note
;;; that the invert process time is NOT taken into account here.

(defun report-recon-eta (segment)
 (let ((le (find-log-entries segment 'recon-and-invert-started)))
  (if le 
  (let* ((recon-event (car (last le)))
	 (n-raw-done (1- (count-raws-done segment recon-event)))
         (n-pfiles (length (segment-trials segment)))
	 (start-time-as-int (second recon-event))
	 (current-time-as-int (time-as-int)))
    ;; Watch out for having gone over a day boundry, which actually
    ;; can happen easily in recon.  We notice this by the current
    ;; time begin BEFORE the start-time (we exclude the possibility
    ;; that it's actually come around 24 hours).  In this case,
    ;; correct by adding 24 hours to the current time, and then
    ;; subtract it on the other end, mod'ing out 24 hours there.
    (if (< current-time-as-int start-time-as-int)
	(incf current-time-as-int (* 24 3600)))
    (format t "~a done out of ~a (~a%); started at ~a, ETA is ~a~%"
	    n-raw-done
	    n-pfiles
	    (truncate (* 100 (/ (float n-raw-done) n-pfiles)))
	    (pretty-time start-time-as-int)
            (pretty-time (mod (+ start-time-as-int 
				 (truncate (/ (- current-time-as-int start-time-as-int)
					      (/ (float n-raw-done) n-pfiles))))
			      (* 24 3600))
			 )
	    ))
  )))

(defun count-raws-done (segment recon-event)
  (system! "grep -i raw ~a | wc > /tmp/fis.tmp" 
           (log-dir segment (find-latest-recon.log-file2 (list recon-event)))
	   )
  (with-open-file (f "/tmp/fis.tmp" :direction :input)
    (1- (read f))))

