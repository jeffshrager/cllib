;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; --- Preferences definition and changing.  Prefs are just a list of 
;;; globals that get save in the user's .fisinit file and reloaded
;;; whenever fis starts up.

;;; The initforms are saved with the prefs and can be edited by the user
;;; to do things on load.  They are just eval'ed in series.

(defvar *initforms* ())

(defvar *prefs* ())

(defmacro defpref (name default-value type-string description)
  `(progn 
     (push (list ',name ,type-string ,description ,default-value)
	   *prefs*)))

;;; Make sure prefs are cleared in case of reload.

(setq *prefs* ())

;;; --- Here are the system-default preferences.  Note that a pref can't have a
;;; nil value because preference scanning (ensure-prefs) will reset it to 
;;; its default!

(defvar *user-top-path*)
(defpref *user-top-path* '(pfn (homepath) "fis")
  "a new quoted directory pathname"
  "
FIS will currently look for (and put) result sets
in the directory: ~a
unless you use HERE or give a quoted path name.  
See '? use' for more information.~%")

(defvar *region-spec-method* ())
(defpref *region-spec-method* '(quote diagonals)
  "One of: DIAGONAL (or D) for the 2-point method, RANGE (or R) for the 6-point method, or VICINITY (or V):"
  "Avoi region specification will be based upon the ~a method.~%")

(defvar *on-startup-expr* ())
(defpref *on-startup-expr* 'nil
  "A new expression to eval on startup:"
  "When FIS is started up, the following expression will be evaluated: ~%~a~%")

(defvar *optional-avoi-output* ())
(defpref *optional-avoi-output* '(quote excel)
  "one of these words: EXCEL MATLAB, "
  "
The 'stat template' command writes out .lsp files to be read into lisp.
In addition, you have chosen to have output formated for ~a.~%")

(defvar *percent-plot-yrange* nil)
(defpref *percent-plot-yrange* "[0:5]"
  "A new quoted range expression, including brackets, or AUTO."
  "A y-axis range of \"~a\" will be used when plotting percentages.~%")

(defvar *percent-plot-yrange* nil)
(defpref *percent-plot-yrange* "AUTO"
  "A new quoted range expression, including brackets, or AUTO."
  "A y-axis range of \"~a\" will be used when plotting percentages.~%")

(defvar *dbg* T)
(defpref *dbg* T
  "T or NIL"
  "When FIS starts up, the debugging switch value is ~a.~%")

;;; The .fisinit should be found in the user's top level directory.
;;; If it's not, we warn him and tell him about the prefs command for
;;; creating it.

(defun load-init ()
  (let ((initfile-path (pfn (homepath) ".fisinit")))
  (if (file-exists? initfile-path)
      (progn
	(load initfile-path)
	(mapcar #'eval *initforms*)
	)
    (progn
      (format t "You don't have an .fisinit file in your home directory.~%")
      (format t "You really ought to create one using the pref command.~%")
      (dolist (p *prefs*)
        (set (car p) (eval (fourth p))))
      ))
  )
  (ensure-prefs))

;;; --- Fn for changing preferences.

(defun change-preferences ()
  (ensure-prefs)
  (dolist (p *prefs*)
    (let ((pref (car p))
	  (string (cadr p))
	  (desc (caddr p)))
      (format t desc (eval pref))
      (let ((newval (prompt-for-string 
		     (format nil "Enter ~a or press ENTER to leave it as is:" string))))
	(if newval
	    (progn (set pref (read-from-string newval))
		   (format t "Changed to: ~a~%" (eval pref)))
	  (format t "Unchanged!~%")
	  ))
      ))
  (save-prefs)
  )

(defun save-prefs ()
  (with-open-file (f (pfn (homepath) ".fisinit") 
		     :direction :output
		     :if-exists :supersede)
    (dolist (p *prefs*)
      (format f "(setq ~a '" (car p))
      (print (eval (car p)) f)
      (format f ")~%")
      )
    (format f "(setq *initforms* '(")
    (format f "~%;;; Put your init forms between here and the next comment...~%")
    (dolist (form *initforms*)
      (format f "~a~%" form))
    (format f "~%;;; ... your init forms should end here~%")
    (format f "))~%")
    ))

;;; Ensure that each nil pref has its default value.  this gets done at init
;;; time as well!

(defun ensure-prefs ()
  (dolist (p *prefs*)
    (if (null (eval (car p)))
	(set (car p) (fourth p))))
  )

