;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- This implements the top level command loop.

;;; Entry to the interaction loop is here.  Loads up inits, prints
;;; news, and then it's just a command loop.  Note that the (fis)
;;; function calls this after doing a read-line to suck up excess
;;; input, but when we come in from the shell, we come straight here
;;; through *startup-functions* (see main.lsp).

(defun analyze-mri-study ()

  (ensure-undribble)

  ;; Lots of tools, esp. ones that use tmp files or create log files,
  ;; depend upon randomization, so each time in we ensure randomization.
  ;; For some reason, gcl doesn't actually randomize on (make-random-state t)
  ;; so we have to do a loop ourselves.

  (setq *random-state*
    (dotimes (k (mod (get-universal-time) 1000) (make-random-state t))
      (make-random-state t)))

  (format t "

                              FIS - Functional Imaging System
                                                                (v. ~a)
      Copyright (c) 1996 The University of Pittsburgh
                         Psychological Software Tools, Inc.
                         Jeff Shrager

" *fis-version*)

  (if (file-exists? (homefile "fis.news"))
      (system! "cat ~a" (homefile "fis.news")))

  (load-init)

  ;; Need to set up special paths under the user's given top path.
  (setq *series-top-path* (pfn *user-top-path* "series"))
  (setq *results-top-path* (pfn *user-top-path* "results"))
  (ensure-user-fis-dirs)

  (if *on-startup-expr*
      (progn 
	(format t "Evaluating startup expr: ~a~%" *on-startup-expr*)
	(eval *on-startup-expr*)
	))

  (fis-cmd-loop))

(defun fis-cmd-loop ()
  (prog (cmd-string cmd)
    loop
    (use-fis-error-mode)
    (catch 'abort-command
    (setq cmd-string
      (cond ((series-mode?)
	     (prompt-for-string
	      "~%~%=== [Series: ~a] -- command (? for help)> "
	      (series-name *z*)))            
	    (*r*
	     (prompt-for-string
	      "~%~%=== [~a (~a, ~a)] -- command (? for help)> "
	      (result-set-name *r*)
	      *loaded-study-name*
	      (segment-name *g*)))
	    (t 
	     (prompt-for-string "~%~%=== [no context] -- command (? for help)> ")
	)))
    ;; Special hack for ! unix command so that we can do it uncased.
    (if (and (> (length cmd-string) 0) 
	     (equal #\! (aref cmd-string 0)))
	(progn
	  (system! (subseq cmd-string 1))
	  (abort-command "")))

    (if cmd-string (setq cmd (multi-read-from-string cmd-string))
      (abort-command ""))

  ;; Before we do anything else, look for various help forms:
  ;; cmd? ?cmd and: cmd ?  (don't have to look for ? cmd since that'll
  ;; be caught below.  In fact, all we do is translate into that form.
  
  (if (not (eq '? (car cmd)))
      (if (eq '? (second cmd))
	  (setq cmd (list '? (car cmd)))
	(let ((string-cmd-word (format nil "~a" (car cmd))))
	  (if (find #\? string-cmd-word)
	      (setq cmd (list '? (intern (remove #\? string-cmd-word))))))
	))
  
  ;; Also look for various forms of lisp call: /...
  
  (if (not (member (car cmd) '(/)))
      (let ((string-cmd-word (format nil "~a" (car cmd))))
	(if (equal #\/ (aref string-cmd-word 0))
	    (setq cmd (list 'lisp (read-from-string 
				   (subseq string-cmd-word 1))))
	  )))

  ;; Main dispatch.

  (if (eq '*EXITFIS*
	  (dispatch-command cmd cmd-string))
      (return nil)
    )
  ) ; close catch

  (go loop)

  ))

;;; --- This is the main command dispatcher.  It's broken out of
;;; fis-command-loop so that after a 'reload', you get changes in
;;; your commands w/o having to get out and back into fis.

(defun dispatch-command (cmd cmd-string)
  (case (car cmd)

	  ;; ----- BASIC COMMANDS

          ;; The use command: use study results-set [segment] does a
          ;; load, and also sets up (or uses an existing) results
          ;; directory, and localies (if given) the segment that
          ;; you're on.

          ((use)
	   (use-context (cdr cmd))
	   )

	  ((series z ser)
	   (series-handler (cdr cmd))
	   )

	  (stat
	   (rs-guard)
	   (run-stats (second cmd) (cddr cmd)))

          ;; Find gives information about studies and segments or
	  ;; about result sets.  It searches the directory of studies
	  ;; for studies whose fis.description matches certain
	  ;; keywords, or, for result sets, in the user's default
	  ;; context directory.  For results sets it can operate
	  ;; without an arg, in which case it lists all your results
	  ;; sets, but not in the case of study listing.

	  ((f find list li)
	   (case (cadr cmd)
	     ((r results c contexts context result result-set)
	      (list-result-sets (cddr cmd)))
	     ((s study studies)
	      (find-studies (cddr cmd)))
	     ((z series)
	      (find-series (cddr cmd)))
	     (t (abort-command "List (find) must be followed by '{r}esult' (sets), '{s}tudies' or 'z' (series), and optional keywords.")
		)
	     )
	   )

	  ;; The pref command sets up the system preferences.
          ((pref prefs) (change-preferences))

	  ;; Note taking appends to the log file "general.notes" in the
	  ;; result set's log directory.
          (note 
	   (if (or (series-mode?) (rs-guard))
	       (note-cmd (cadr cmd)))
	   )

	  ((? h help)
	   (if (null (cdr cmd))
	       (progn
		 (format t "(Starting netscape for help; This might take a moment.)~%")
	         (system! "netscape ~a/tophelp.html &" *help*)
		 )
	     (let ((xterm (translate-help-request (cadr cmd))))
	       (if xterm
		   (progn
		     (format t "(Starting netscape for help; This might take a moment.)~%")
		     (system! "netscape ~a/~a.html &" *help* xterm)
		     )
		 (format t "No help on ~a is available.~%" (cadr cmd))
		 ))
	     ))

	  ;; "Calling" enters a log entry indicating the nomonated
	  ;; result or voi has the given label.  This can later be used by
	  ;; series analysis.  

	  ((call lab label) 
	   (rs-guard)
	   (case (second cmd)
             ((r result) (label-result (third cmd) (fourth cmd)))
             ((v voi) (label-voi-set (third cmd)))
             (t (abort-command "What sort of thing are you labeling; voi? result?"))
	     )
	   )

	  ((m mriview mv v view) 
	   (if (series-mode?)
	       (view-series (cdr cmd))
	     (progn
	       (rs-guard)
	       (mriview (cdr cmd)))
	     )
	   )

	  ;; Take screen dumps and put them in the log dir.
	  ((grab makeps mps snap snapshot)
	   (xgrab))

	  ;; Palette commands load, describe and edit palettes.
	  ((pal pals)
	   (rs-guard)
	   (pal (cdr cmd)))

	  ;; xv just fires up an xv in background.  If there's a study
	  ;; or series around, fires up in the appropriate log location
	  ;; with snap*

	  (xv 
	   (if (or *r* *z*)
	       (system! "xv ~a &" (appropriate-log-dir "snap*"))
	       (system! "xv &"))
	   )

	  ((del delete dr) ; results deletion
	   (rs-guard)
	   (delete-results (cadr cmd) (cddr cmd)))
	   
	  ;; Description is a little weird in this world because
	  ;; there's a user's view and the admin's view.  Here we're
          ;; focussed on the user's view under the assumption that 
          ;; the admin will be worked out later and that s/he knows
	  ;; what s/he's doing.  Describing involves tells the user
	  ;; primarily about his results.  If there are any args,
	  ;; then it's a different story altogether.

          ((d display desc describe)
	   (if (cdr cmd)
	       (describe-outboard (second cmd) (third cmd))
	     (if *z*
		 (describe-series)
	       (progn
		 (study-guard)
		 (describe-study)
		 (rs-guard)
		 (describe-result-set)
		 ))))

	  ((log l)
	   (rs-guard t)
	   (log-cmd (cdr cmd)))

	  ;; The condition command adds new links to the condition graph
	  ;; and saves it back in the right place.  Note that this command
	  ;; has two forms, which are forked internally to it, one for
	  ;; superuser and one for normal users.

	  ((cond condition)
	   (rs-guard)
	   (condition-cmd (cadr cmd) (cddr cmd)))

	  ;; Run user defined macros.

	  ((run r do)
	   (run-user-macro (cdr cmd))
	   )
	  
	  ;; ----- CONTROL COMMANDS

	  ((/ lisp) 
	   (if (cdr cmd)
	       (print (eval (cadr cmd)))
	     (progn
	       (use-default-error-mode)
	       '*EXITFIS*)))
	  ((exit q quit bye)
	   (if (yes-or-no-p "Are you sure that you want to exit?  ")
	       (progn
		 (remember-session-file)
		 (bye))))
	  ((cd) (cd cmd-string))
	  ((unix u) (run-unix-commands))

          ;; Avoi tools; see that .lsp module for details.
          ((avoi)
	   (rs-guard)
	   (avoi (cdr cmd)))

          ;; This is just a cute shortcut to 'avoi i'
          ((surf)
	   (rs-guard)
	   (avoi '(i)))

	  ;; The meta command runs analyzes over series members.

	  ((meta)
	   (meta-command (cdr cmd)))

	  ;; ----- ADMIN COMMANDS

  	  (su 
  	    (if (superuser?) 
	        (setq *mode* 'user)
	        (setq *mode* 'superuser))
	    (format t "Now in ~a mode.~%" *mode*))

	  ((reload) (reload))

          ((bug test debug) 
	   (setq *dbg* (not *dbg*))
	   (format t "Testing mode is now: ~a" *dbg*)
	   )

	  (recon 
	   (su-guard)
	   (reconstruct-and-invert *s* (select-segment *s* (cadr cmd))))

	  (rwait 
	   (su-guard)
	   (wait-for-recon-completion *s* (select-segment *s* (cadr cmd))))

	  (rd? 
	   (su-guard)
	   (check-rdone *s* (select-segment *s* (cadr cmd))))

	  ;; Reconditioning is a very rare practice that changes the
	  ;; birth conditions associated with each trial.

          ((recondition)
	   (su-guard)
           (change-condition-names-and-reinvert
	    (select-segment *s* (cadr cmd))))

	  ;; rnvol is actually mostly a utility that should only be used
	  ;; by the admin, but it doesn't generally hurt anything, so it
	  ;; isn't protected.

	  ((rn rnvol)
	   (rs-guard)
	   (rnvol (result-dir)))

          ;; Load and save in this version are super user (or at least
          ;; administrator commands), used primarily to work on the 
          ;; STUDY data structures, which is, in this version, separate
          ;; from the results.  See the "use" command, below, for user
          ;; loading.

	  ((save)
	   (su-guard)
	   (study-guard)
	   (save-study (cadr cmd)))

          ((load)
	   (su-guard)
	   (if (check-really-kill-current-study?)
	       (load-study (if (cadr cmd) (cadr cmd)
				  (prompt-for-string "Load which study file? "))))
	   )

	  (new
	   (su-guard)
	   (if (check-really-kill-current-study?)
               (progn
		 (setq *s* ())
		 (input-study))
	     )
	   (check-and-sort-everything *s*)
	   )

          (check 
	   (su-guard)
	   (study-guard)
	   (check-and-sort-everything *s*)) ; this is undocumented

	  ;; ----- ERROR CASE!!

	  (t (format t "Say What!?~%"))
	  )
  )

;;; This can be called from pretty much anyplace in the system and
;;; throws back up to the command loop, terminating all processing.

(defun abort-command (&optional msg &rest args)
  ;; Ensure that dribbling is off if it was on.
  (ensure-undribble)

  (if msg (apply #'format `(t ,msg ,@args)))
  (throw 'abort-command ())
  )

(defun ensure-undribble ()
  (if system::*dribble-stream* (dribble))
  )

;;; These throw up if there's not study study or result set, or we're
;;; superuser.  They're used to defend against trying to do things
;;; without the right stuff in core.

(defun su-guard ()
  (if (not (superuser?))
      (progn (format t "This is a protected command.  You probably don't want to be using it.~%")
	     (abort-command))))

(defun study-guard ()
  (if (null *s*)
      (progn (format t "Probably best to load a study first, eh?~%")
	     (abort-command))))

(defun rs-guard (&optional permit-series-also?)
  (if (not (or *r* (and permit-series-also? *z*)))
      (progn
	(format t "You need to use the USE command to set a context.~%")
	(if permit-series-also? 
	    (format t "or the SERIES command to load up a series.~%"))
	(abort-command "Command aborted.")
	)
    t))

;;; Used before load because we're about to clobber the incore info.

(defun check-really-kill-current-study? ()
  (if (null *s*) t
      (progn
	(yes-or-no-p "This will clobber the currently loaded study.  Really do it?"))))


;;; --- Various simple commands that don't deserve their own files.

;;; The viewer is forked out view &.  If debugging is off, messages
;;; are sent into /dev/null, otherwise they're just left in the output
;;; stream.  Before calling the viewer, see if there's a session file
;;; for this brain and segment in the aux dir, and map it into the
;;; viewer's way if so.  (The session file will get put back out to
;;; the aux directory by either the quit command or when a new load
;;; takes place.  This is sort of crufty, but because we don't know
;;; when the viewer session ends, we can't do it synchronously.)

(defun mriview (args)
  (install-session-file)
  (if args
      (progn
	(if (not (stringp args))
	    (format "(Warning: If this doesn't find your file, you might have to pass in a quoted string argument.)~%")
	  )
	(system! (format nil 
		    "cd ~a;/users/mri/alphabin/mriview ~a &"
		    (result-set-path *r*)
		    (car args)
		    ))
	)
    (system! (format nil 
		    "cd ~a;/users/mri/alphabin/mriview -p ~a -r ~a -i ~a -v ~a &"
		    (result-set-path *r*)
		    (pal-dir-or-default)
		    (results-dir)
		    (inplanes-dir *g*)
		    (volume-dir *g*)
		    ))
    )
   )

(defun install-session-file ()
  (if (and *s* *r* *g*)
      (let ((target-path (result-set-path *r*))
	    (sf (/aux "session"
		      (study-brain-id *s*)
		      (segment-name *g*))))
	(if (file-exists? sf)
	    (progn
	      (format t "(Copying Talairach coord database from your aux dir.)~%")
	      (system! "cp ~a ~a" sf (pfn target-path "session"))
	      ))
	)))

(defun remember-session-file ()
  (if (and *s* *r* *g*)
      (let ((target-path (result-set-path *r*))
	    (sf (/aux "session"
		      (study-brain-id *s*)
		      (segment-name *g*))))
	(if (file-exists? (pfn target-path "session"))
	    (progn
	      (format t "(Copying Talairach coord database to your aux dir.)~%")
	      (system! "cp ~a ~a" (pfn target-path "session") sf)
	      ))
	)))

;;; --- Here's a way to issue unix system commands.  NO COMMAND
;;; INTERPREATION IS PERFORMED!

(defun run-unix-commands ()
  (format t "Your current working directory is ~a; 
 Use the FIS cd command (not the unix cd command) to change it.~%"
	  (find-current-directory))
  (format t "~%Enter a line with just a single period to return to FIS.~%")
  (loop 
   (let ((cmd (prompt-for-string "Unix >")))
     (if cmd 
         (if (not (equal cmd "."))
	     (system cmd)
	     (abort-command "Returning to FIS!~%"))))
   ))

(defun cd (path)
  (setq path (subseq path 2)) ; remove "cd"
  (let ((r (set-working-directory path)))
    (if (not r)
	(abort-command "No such directory.")))
  )

;;;

(defun superuser? () 
  (eq *mode* 'superuser))

;;; --- Help term translator.

(defun translate-help-request (interm)
  (case interm
   ((voi avoi) 'AVOI)
   ((context) 'CONTEXT)
   ((xv) 'xv)
   ((del delete) 'DELETE)
   ((do) 'DO)
   ((find list li f) 'FIND)
   ((l log i inspect) 'log)
   ((load) 'LOAD)
   ((color pal pals palettes) 'PAL)
   ((pref prefs) 'PREFS)
   ((print printing) 'PRINTING)
   ((recon) 'RECON)
   ((RESULTS) 'RESULTS)
   ((voi ROI) 'ROI)
   ((z SERIES) 'SERIES)
   ((statistics stat STATS) 'STATS)
   ((TEMPLATE) 'TEMPLATE)
   ((! UNIX) 'UNIX)
   ((USAGE) 'USAGE)
   ((USE) 'USE)
   ((VIEW) 'VIEW)
   ((note) 'note)
   ((label call) 'label)
   (t (abort-command "Sorry.  There's no help for you!"))
   ))
