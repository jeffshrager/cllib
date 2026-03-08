;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; --- This module manages the log directory associated with each
;;; segment.  There is always one file called log.index and then any
;;; number of variously named files that log.stream points to.  Also
;;; code for screen snapshoting, log summarization, etc.

(defun init-logging-for-a-new-study (segment)
  (check-and-create-directory (slog-dir segment))
  (slog! segment 'creation))

;;; --- The log! function appends an entry to the study log file,
;;; creating one if necessary.  Each log entry is forced to be in lisp
;;; load format so that it can be easily read in if necessary.

(defun slog! (segment ident &optional format &rest format-args)
  (with-open-file (logfile (slog-dir segment "log.index")
			   :direction :output
			   :if-exists :append
			   :if-does-not-exist :create)
    (format logfile "(~a ~a ~a " (date-as-int) (time-as-int) ident)
    (if format (apply #'format (cons logfile (cons format format-args))))
    (format logfile ")~%")
    ))

;;; rlog! is superseded by log!, below.  (Which has to be applied because
;;; of the &rest!)

(defun rlog! (ident &optional format &rest format-args)
  (apply #'log! `(,ident ,format ,@format-args)))

;;; Log! is a generalization of rlog! which decides whether we're logging
;;; in the rlog or series (z) log and does the right thing.  Everything 
;;; that was rlog! should use this now, and all new result-set or series
;;; logging ought to go through this.

(defun log! (ident &optional format &rest format-args)
  (with-open-file (logfile (appropriate-log-dir "log.index")
			   :direction :output
			   :if-exists :append
			   :if-does-not-exist :create)
    (format logfile "(~a ~a ~a " (date-as-int) (time-as-int) ident)
    (if format (apply #'format (cons logfile (cons format format-args))))
    (format logfile ")~%")
    )

  ;; put in the log entry w/o more info.
  (if format 
      (take-note 
       (list (apply #'format (cons nil (cons format format-args)))
		    (format nil "Operation: ~a, args: " ident))
       nil))
  )

;;; --- Routines to scan the logs for various things.

(defun find-log-entries (&optional id)
  (fle2 (appropriate-log-dir "log.index") id))

;;; There's some complexity to this.  Sometimes, when you're in series
;;; mode you really want to use the results log instead of the series
;;; log, esp. if we're trying to read info out of the results that go
;;; with each series.  Therefore, altough find-rlog-entries is indeed
;;; outdated, there's a really-find-rlog-entries which does just that
;;; regardles of mode.

(defun find-rlog-entries (&optional id)
  (warn! "Outdated call to find-rlog-entries!")
  (find-log-entries id))

(defun really-find-rlog-entries (&optional id)
  (fle2 (rlog-dir "log.index") id))

(defun fle2 (lf id &aux result)
  (if (file-exists? lf)
      (with-open-file (l lf)
	 (prog (le)
	  loop (setq le (read l nil '**eof**))
	       (if (eq '**eof** le)
		   (return (reverse result)))
	       (if (or (null id) (eq id (third le)))
		   (push le result))
	       (go loop)
	       ))
    ))

;;; --- Some note taking.  Other code can pass in prefix information
;;; by giving the optional notes argument.  Note that this gets
;;; reversed when it's saved so we have to reverse the result of the
;;; read-lines!  All the reversing ought to get cleaned up here
;;; sometime.

;;; The note command itself can take a filename arg, which must be a string.
;;;

(defun note-cmd (arg)
  (cond ((null arg) (take-note))
	((eq '* arg) (include-file-in-log (/tmp "runlog")))
	((or (atom arg) (stringp arg)) (include-file-in-log arg))
	(t (abort-command "Argument must be a filename or * for the dribble log."))
	))

(defun take-note (&optional notes (take-more-notes t))
  (if take-more-notes
      (setq notes (append (reverse (read-lines-until-period "Notes >")) notes)))
  (if notes
      (save-note (reverse notes)))
  )

;;; Note that NIL entries are converted to " " here, but if nothing is
;;; entered at all, this returns nil.  Also, " " lines are removed from the
;;; beginning of the entry.

(defun read-lines-until-period (prompt &aux text)
  (format t "Enter lines of text.  Enter a period on a line by itself to end.~%")
  (loop 
   (let ((entry (prompt-for-string prompt)))
     (if (null entry) (setq entry " "))
     (if (equal entry ".")
	 (return nil)
       (push entry text))
     ))
  (do ((ctext (reverse text) (cdr ctext)))
      ((or (null ctext) (not (string-equal " " (car ctext))))
       ctext))
  )

(defun save-note (notes &optional file)
  (with-open-file (f (or file 
			 (if (series-mode?)
			     (zlog-dir "general.notes")
			   (rlog-dir "general.notes")
			   ))
		     :direction :output :if-exists :append
		     :if-does-not-exist :create)
    (format f "<html><h1>---- Notes created on ~a at ~a ----</h1>~%" 
	    (date-as-int) (pretty-time (time-as-int)))
    (dolist (nl notes)
      (format f "<br>~a~%" nl))
    (format f "</html><hr>")
    ))

;;; --- Log inspection.  There are a number of subcommands to inspection, and 
;;; it's getting built as new needs arise.

(defun log-cmd (args)
  (if (null args)
      (system! "netscape ~a &" (appropriate-log-dir "general.notes"))
      (case (pop args)
	((notes n netscape examine read r i inspect)
	 (system! "netscape ~a &" (appropriate-log-dir "general.notes")))
	((t type l list)
	 (rs-guard)
         (summarize-log-by-type (pop args)))
	((r result f find)
	 (rs-guard)
         (summarize-log-by-result (verify-results args)))
	((a all *)
	 (summarize-log))
	(t (abort-command "Invalid log command args.  See ?log for help."))
	)))

;;; Including a file is just like taking a snapshot, except that the file
;;; gets named and entered instead of a gif.

(defun include-file-in-log (fn)
  (if (not (stringp fn))
      (abort-command "Need to give a quoted complete filepath."))
  (if (not (file-exists? fn))
      (abort-command "~a doesn't appear to exist." fn))
  (let* ((inclfile (format nil "inclfile.~a.~a" (date-as-int) (time-as-int)))
	 (fullinclfile (appropriate-log-dir inclfile))
	)
	(format t "Copying over ~a to the log.~%" fn)
        (system! "cp ~a ~a" fn fullinclfile)
        (format t "Enter notes for ~a.~%" fn)
        (take-note (list (format nil "<br><a href=~s>~a</a><br>~%" inclfile fn)))
       ))

;;; Each log entry is assumed to have its own summarization function which
;;; is called "summarize-<type>-log-entry", which must be defined in the
;;; various type-specific modules.  The time and date are created here.

(defun summarize-log (&optional type)
  (let ((entries (find-rlog-entries type)))
    (dolist (e entries)
      (present-log-entry e)
      )))

(defun summarize-log-by-type (type)
  (let ((entries (find-rlog-entries type)))
    (dolist (e entries)
      (if type 
	  (present-log-entry e)
          (format t "~a@~a, ~a~%" (first e) (pretty-time (second e)) (third e)))
      )))

(defun summarize-log-by-result (results)
  (let ((entries (find-rlog-entries)))
    (dolist (e entries)
      (if (scan-log-entry-for-results e results)
	  (present-log-entry e))
      )))

(defun scan-log-entry-for-results (e rl)
  (dolist (r rl)
    (if (member r e) (return t))))

(defun present-log-entry (e)
  (format t "~a@~a, ~a: " (first e) (pretty-time (second e)) (third e))
  (funcall (read-from-string (format nil "summarize-~a-log-entry" (third e)))
	   (nthcdr 3 e))
  )

;;; --- Tool to grab an x screen image.  Makes up a filename and logs it, and
;;; lets you enter notes as well.

(defun xgrab ()
  (let* ((imfile (format nil "snapshot.~a.~a" (date-as-int) (time-as-int)))
	 (fullimfile (if (series-mode?)
			 (zlog-dir imfile)
		       (rlog-dir imfile)
		       ))
	)
	(format t "Use the left mouse button to sweep an area.~%")
        (system! "~a -ppm | /usr/local/bin/ppmtogif > ~a" 
		 *xgrab-location* fullimfile)
        (format t "Enter notes for this image.  Enter a period on a line by itself to end.~%")
        (take-note (list (format nil "<img src=~s><br>~%" imfile)))
       ))

(defun SUMMARIZE-XGRAB-LOG-ENTRY (e)
  (format t "~a" e))

