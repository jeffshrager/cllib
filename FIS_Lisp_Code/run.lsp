;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; --- Tools and driver for user macros.

;;; The args to the run command are the user macro or filename and its
;;; args.  If a filename is given, the file must contain a macro defun
;;; with the given name.  These aren't actually defuned; rather, they
;;; are loaded and the fis macro processor evals the body after some
;;; arg checking.

(defvar *user-macro* ()) ; this is set by loading the .fis file.
                         ; it's actually sort of residual

(defun run-user-macro (args &aux macro)
  (let ((name-or-file (car args))
	(in-args (cdr args)))
    (if (not (setq name-or-file (find-macro-file name-or-file)))
	(abort-command "No such macro file was found."))
    (format t "(Loading from ~a)~%" name-or-file)
    (load name-or-file)
    (setq macro *user-macro*)
    (if (null macro) 
	(abort-command "No such macro was found."))
    (format t "Executing user macro ~a ...~%" (first macro))
    (let ((args (resolve-args (second macro) in-args)))
      (log! 'macro-begin "macroname ~a args ~a" (car macro) args)
      (dribble (/tmp "runlog"))
      (format t "~%~%Beginning macro ~a with args:~%" (car macro))
      (mapcar #'(lambda (argname argv) (format t "  ~a = ~a~%" argname argv))
	      (second macro) args)
      (format t "~%~%")
      (apply (cons 'lambda (cdr macro)) args))
    (format t "~%...Completed user macro ~a!~%" (first macro))
    (dribble)
    (if (yes-or-no-p "Automatically enter the dribble into your notes?")
	(note-cmd '*)
      (format t "~%You can use 'note *' to log the dribbled results.~%"))
    (log! 'macro-end "macroname ~a" (car macro))
    (setq *user-macro* ())
    ))

(defun summarize-macro-begin-log-entry (rec)
  (format t "~a~%" rec))
(defun summarize-macro-end-log-entry (rec)
  (format t "~a~%" rec))

;;; Macros can be in the local directory or the user's fis main
;;; directory.  Check for various versions of the name.

(defun find-macro-file (filename)
  (if (file-exists? filename)
      filename
    (let* (
	   (n1 (pfn *user-top-path* filename))
	   (n2 (pfn *user-top-path* (format nil "macros/~a.fis" filename)))
	   (n3 (pfn *user-top-path*
		    (format nil "macros/~a.fis"
			    (string-downcase (format nil "~a" filename)))))
	   )
      (cond ((file-exists? n1) n1)
	    ((file-exists? n2) n2)
	    ((file-exists? n3) n3)
	    )
      )
    ))

(defmacro defismacro (&rest macro)
  `(setq *user-macro* ',macro))

;;; If the args aren't all supplied in the run command, then they are
;;; prompted for.

(defun resolve-args (arglist given-args)
  (if (= (length arglist) (length given-args))
      given-args
    (progn
      (format t "Needed ~a args but ~a were given.~%~%Prompting for all args.  Enter a blank return to abort!~%"
	      (length arglist) (length given-args))
      (setq given-args ())
      (dolist (a arglist)
        (let ((entry (prompt-for-string "  Enter a value for ~a:" a)))
	  (if (null entry)
	      (abort-command "Macro abort!")
	      (push (read-from-string entry) given-args)
	      ))
	) ;dolist
      (reverse given-args))))
