;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- This is a set of tightly interlocked tools that interactively
;;; process the output of avoi and template.  

;;; (The two top level functions are slightly redundant.)

(defun avoi (args)
  ;; load up the region database if you haven't already.  (Note that we
  ;; have to ensure that this is cleared whenever a new brain gets loaded!)
  (if (and (not *avoi-regions*)
	   (file-exists? (region-database-filename)))
      (load (region-database-filename)))

  (if (null args)
      (report-loaded-avoi-info)
   (case (pop args)
    ((clean cleanup cl) (avoi-cleanup))
    ((note notes n) (avoi-note))
    ((list) (avoi '(reg list)))
    ((tc) (report-nearest-tc-label args))
    ((cor correlate corr)
     (correlate-vois args))
    ((label region reg lab regions labels)
     (avoi-region-labeling args))
    ((plot) (toggle-avoi-plot-status))
    ((i interactive) (iavoi))
    ((f find) (favoi (second args) (third args) (fourth args)))
    ((l load)
     (if (first args)
	 ;; Loading by label.
	 (load-voi-set-by-label (first args))
         ;; Otherwise, user needs to be hand held.
         (avoi-wizard 'load)))
    ((t tpl template) (load-tpl-file (car args)))
    ((part p particle partical)
     (favoi (car args) (cadr args)))
    ((a all)
     (favoi 'all (car args)))
    ((append)
     ;; tests for given args and wizardization as w/load.
     (if (first args)
	 ;; In this case assume the user knows what's up and just go for it.
	 (append-additional-tpl-file  (first args) ; threshold
			 (second args) ; count
			 (third args) ; masked result
			 (fourth args) ; templated condition
			 )
         ;; Otherwise, user needs to be hand held.
         (avoi-wizard 'append))
     )

    ;; Edit the voi table.  Note that you have to explicitly resave it after!
    ((include replace rep inc)
     (replace-voi (car args)))
    ((exclude del delete exc ex remove rm)
     (exclude-voi (car args)))

    ((save resave)
     (resave-voi-table))

    (t (abort-command "Bad avoi args.  See ?avoi for help."))
    )))

(defvar *avoiplot* ()) ; Tells whether plotting ought to be done or not.
                       ; Changed by the avoi plot command and is set
                       ; ON (t) by the tk interface.

;;; Correlate takes as arg a list of vois to cross-correlate with one
;;; another.  (Giving no arg means to cross-correlate every voi with 
;;; every other voi.)

(defun correlate-vois (vois &aux v1 allccs)
  ;; Convert args to the trace data (if no args, use all vois!)
  (if (null vois)
      (setq vois (mapcar #'voi-id *vois*)))
  ;; Do the intercorrelation of every voi's data with all others.
  (loop
     (if (null vois) (return nil))
     (setq v1 (pop vois))
     (dolist (v2 vois)
       (let ((c (correlate (find-trace-for-voi v1) 
			   (find-trace-for-voi v2))))
	 (push (cons v1 (cons v2 c)) allccs))
       )
     ) ; loop
  ;; Now display them in best-r^2-frist order.
  (dolist (v (sort allccs #'(lambda (a b) (> (getf (cddr a) :r^2)
					     (getf (cddr b) :r^2)))))
    (format t "~a ~a v. ~a ~a -> ~a~%"
	    (car v) (p2 (scanner-location-from-voinum (car v)))
	    (cadr v) (p2 (scanner-location-from-voinum (cadr v)))
	    (p2 (cddr v))
	    )
    )
  ;; If there were only two given, and plotting is on, plot them both.
  (if (and *avoiplot* (= 1 (length allccs)))
      (let* ((cc (first allccs))
	     (v1 (first cc))
	     (v2 (second cc))
	     )
	(plot1 (list (tpl-to-plot1 (cons " " (find-trace-for-voi v1))))
	       (format nil "vois #~a" v1))
	(sleep 2) ; make sure we don't overwrite gnuplot's inputs!
	(plot1 (list (tpl-to-plot1 (cons " " (find-trace-for-voi v2))))
	       (format nil "vois #~a" v2))
	)
    )
  )

;;; There are probably ten of these translation fns all over here
;;; already, but I can't keep track of them.  Some day I ought to put
;;; all of these together in the same place and/or use a defstruct.

(defun scanner-location-from-voinum (vn)
  (voi-scenter (find-voi-by-id vn)))

(defun find-voi-by-id (id &optional (voi-list *vois*))
  (find id voi-list :test #'(lambda (a b) (= id (voi-id b)))))

;;; Upon load or use change, we clear out everything that we know about 
;;; loaded vios.

(defun clear-voi-tables ()
  (setq *excluded-vois* ())
  (setq *vois* ())
  (setq *tpl* ())
  (setq *loaded-voi-info* ())
  (setq *grand-avoi-data* ())
  (setq *avoi-regions* ())
  (format t "(All voi info has been cleared!)~%")
  )

;;; The call for avoi plot, changes the above.

(defun toggle-avoi-plot-status ()
  (format t "Avoi plotting is now ~a.~%"
    (if (setq *avoiplot* (if *avoiplot* nil t))
	'on 'off)))

;;; Processes most other avoi commands.  This figures out what it's
;;; supposed to do according to what sort of args are passed in.

(defun favoi (x &optional y z)
  (cond ((and x y z)
	 (process-voi (gather-voi-data (find-nearest-voi-from-scanner-xyz x y z))))
	((numberp x)
	 (process-voi (gather-voi-data x) y))
        ((listp x)
         (process-voi (find-nvois-or-region-tag x) y))
	((member x '(c click))
	 (favoi-from-click-file y))
	(t (abort-command "Bad 'avoi p' args; See 'avoi?'"))
	))

(defvar *loaded-voi-info* ()) ; mostly for report headers
(defvar *vois* ())
(defun *tlp* ())

;;; Load up the avoi and or template data.  This knows a lot about the
;;; exact format of the avoi output file.  When that changes, this
;;; must change as well!

(defun load-avoi-file (threshold count masked-result templated-condition ignore)
  (let ((af (find-avoi-file 'avoi threshold count masked-result)))
    (if af
        (progn
	  (format t "(Excluded voi table has been cleared.)~%")
	  (setq *excluded-vois* ())
	  (load af)
	  (report-loaded-avoi-info)
	  )
        (abort-command "No avoi file was loaded."))
    (setq *loaded-voi-info* (list af))
    )
  (format t "(Template data has been cleared.)~%")
  (setq *tpl* ())

  (if templated-condition
      (let ((tf (find-avoi-file 'template threshold count
				masked-result templated-condition)))
	(if tf 
	    (progn
	      (setq *tpl* (load-tpl-file tf))
	      (format t "Loaded ~a template entries from ~a.~%" (length *tpl*) tf))
	  (abort-command "No template file was loaded."))
	(setq *loaded-voi-info* (rplacd *loaded-voi-info* (list tf)))
	)
    )
  (recompute-avoi-grand-mean)
  ;; The old voi structure format didn't include the template
  ;; information in the voi strucures themselves.  Here we
  ;; reform all the vois to include the tpl info in the vois themselves.
  ;; Lot of code still expects the vois and tpl data separately, so
  ;; we can't really do away with the separation quite yet, however.
  (insert-template-data-into-vois)

  (format t "Loaded ~a~%" *loaded-voi-info*)
  (format t "** NOTE ** Avoi plotting is ~a.~%" (if *avoiplot* 'on 'off))
  )

;;; This has to also rebuild all the vois because avoi files saved with
;;; the old struct don't have a tpl slot.

(defun insert-template-data-into-vois ()
  (setq *vois*
    (mapcar #'(lambda (oldv) 
		(make-voi :id (voi-id oldv)
			  :volume (voi-volume oldv)
			  :mean (voi-mean oldv)
			  :peak (voi-peak oldv)
			  :scenter (voi-scenter oldv)
			  :icenter (voi-icenter oldv)
			  :tcenter (voi-tcenter oldv)
			  :tpldata (find-trace-for-voi (voi-id oldv))
			  ))
	    *vois*)))

;;; This is used to report what's currently loaded.  We assume that
;;; the car of the info is the avoi file and that there is one or more
;;; nth tpl files.

(defun report-loaded-avoi-info ()
  (format t "Avoi file: ~a~%" (car *loaded-voi-info*))
  (format t " with templates: ")
  (dolist (tpl (cdr *loaded-voi-info*))
    (format t "~a, " tpl))
  (format t "~%")
  (format t "There are ~a vois in the dataset.~%" (length *vois*))
  (if *excluded-vois* 
      (format t "~a are excluded.~%" (length *excluded-vois*)))
  (format t "** NOTE ** Avoi plotting is ~a.~%" (if *avoiplot* 'on 'off))
  )

;;; We sometimes want to compute and look at the grand mean, at least
;;; as revealed by the loaded voi values.  This is recomputed on each
;;; load or append and is stored in the *grand-avoi-data* along with
;;; other grand info.  It is plotted at compute time (if plotting is
;;; on) and can be used as an ANCOVA variate in later calculations.

(defvar *grand-avoi-data* ())

(defun recompute-avoi-grand-mean (&aux temp)
  (dotimes (n (1- (length (car *tpl*)))) ; 1- excludes the particle id number (car)
    (push (mean (mapcar #'(lambda (tp) (nth n (cdr tp))) *tpl*))
	  temp))
  (set-grand-avoi-data 'mean (setq temp (reverse temp)))
  (if *avoiplot*
      (plot1 (list (tpl-to-plot1 (cons " " temp)))
	     "Grand Mean")
    )
  )

(defun get-grand-avoi-data (type)
  (cdr (assoc type *grand-avoi-data*)))

(defun set-grand-avoi-data (type value)
  (let ((oldentry (assoc type *grand-avoi-data*)))
    (if oldentry (rplacd oldentry value)
	(push (cons type value) *grand-avoi-data*)
      )))

;;; Appending is a kludge.  We store off the old *tpl* and *vois*,
;;; load up entirely new datasets, make sure that the vois are the
;;; same, and then put together the tpl data.  This is just a way of
;;; avoiding averaging.  Eventually, the avoi load command ought to be
;;; able to accept a list of number and do this itself.

(defun append-additional-tpl-file (threshold count masked-result 
					     templated-condition ignore
				   &aux (savedvois *vois*) (savedtpl *tpl*)
				        (savedinfo *loaded-voi-info*))
  (load-avoi-file threshold count masked-result templated-condition)
  (if (not (equal *vois* savedvois))
      (progn (setq *vois* savedvois *tpl* savedtpl)
	     (abort-command "Voiss don't match current set -- Nothing is changed!"))
    (setq *tpl* (append-tpl savedtpl *tpl*)))
  (let ((newtplinfo (second *loaded-voi-info*))) ; this came from the load op.
    (setq  *loaded-voi-info* (cons (car savedinfo) (cons newtplinfo (cdr savedinfo)))))
  (recompute-avoi-grand-mean)
  )

(defun append-tpl (tpl1 tpl2)
  (mapcar #'(lambda (voi) (let ((t1 (find-trace-for-voi (car voi) tpl1))
				(t2 (find-trace-for-voi (car voi) tpl2)))
			    (cons (car voi) (append t1 t2))))
	  *vois*))

;;; This finds either the latest avoi file or template file that has
;;; the indicated properties and confirms that the file actually
;;; exists.

(defun find-avoi-file (type threshold count masked-result
			    &optional templated-condition
			    &aux entry)
  (let ((traces (really-find-rlog-entries type)))
    (dolist (tr traces)
       (let* ((str (cdddr tr)) ; strip the date, time, and type for getf'ing
	      (sthreshold (getf str 'threshold))
	      (scount (getf str 'count))
	      (smask-formed-from (getf str 'mask-formed-from))
	      (scondition (getf str 'condition)))
	 (if (and (= sthreshold threshold)
		  (= scount count)
		  (eq smask-formed-from masked-result)
		  (if templated-condition
		      (eq templated-condition scondition)
		      t) ;(eq masked-result scondition)) << WHY WAS THIS HERE??
		  )
	     (setq entry (getf str 'saved-in))
	   )))
    entry))

;;; Here are the readers for the file types.

(defun read-avoi-file (fn &aux r l)
  (with-open-file (f fn :direction :input)
    ;; throw out the first two lines of header junk
    (read-line f) 
    (read-line f)
    ;; Now read lines until you find a *, which is the beginning of the 
    ;; size footnote entry.  We're done at that point.
    (loop (setq l (read f))
	  (if (eq '* l) (return r))
	  (push l r))
    ))

(defun read-all-from-string (s &aux (start 0) i r)
  (loop (multiple-value-bind (item newstart) 
			     (read-from-string s nil nil :start start)
			     (setq i item)
			     (setq start newstart))
	(if (null i) (return (reverse r)))
	(push i r)
	))

;;; This loads up a template file which is assumed to correlate with
;;; the avois in *avoi*.  There's only one cursory check done, of the
;;; number of particles.  Note that the file (f) arg here is just the
;;; fileform and has to be translated into the mean (or someday
;;; fourier) for by adding tpl....mean.lsp to it.

(defun load-tpl-file (f)
  (load (rlog-dir (format nil "tpl.~a.mean.lsp" f)))
  (setq *tpl* template-data)
  (if (= (+ (length *vois*) (length *excluded-vois*)) (length *tpl*))
      (format t "The number of particles (~a) agrees between the voi and template files.~%" (length *tpl*))
    (progn
      (format t "The number of particles in the voi (~A+~a excluded) and template (~a) files does not agree.~%" (length *vois*) (length *excluded-vois*) (length *tpl*))
      (if (yes-or-no-p "Do you wish to abort the load?")
	  (progn (clear-voi-tables)
		 (abort-command "Voi table load aborted!"))
	)
      )
    )
  *tpl*)

;;; Needs to check also whether there's a closer EXCLUDED voi!

(defun find-nearest-voi-from-scanner-xyz (x y z &aux nearest v)
  (let ((closest-included 
	 (car 
	  (sort (copy-tree *vois*)
		#'(lambda (v1 v2) (< (dcalc x y z (voi-scenter v1))
				     (dcalc x y z (voi-scenter v2))
				     ))
		)))
	(closest-excluded 
	 (car 
	  (sort (copy-tree *excluded-vois*)
		#'(lambda (v1 v2) (< (dcalc x y z (voi-scenter v1))
				     (dcalc x y z (voi-scenter v2))
				     ))
		)))
	)
    (if (and closest-excluded 
	     (< (withloc tx ty tz (voi-scenter closest-excluded)
			 (xyz-distance tx ty tz x y z))
		(withloc tx ty tz (voi-scenter closest-included)
			 (xyz-distance tx ty tz x y z)))
	     )
	(format t "WARNING! EXCLUDED VOI #~a IS CLOSER THAN THE RETURNED INCLUDED ONE (#~a)!
IT'S VERY LIKELY THAT YOU HAVE CLICKED AN EXCLUDED VOI!~%"
		(voi-id closest-excluded)
		(voi-id closest-included))
      )
    closest-included))

(defun dcalc (x y z l)
  (xyz-distance x y z (first l) (second l) (third l)))

;;; Process the vois in the click file, which is in scanner mm coords.
;;; Delete the file after.

(defun favoi-from-click-file (which)
  (let ((cf (/tmp "jeff.foo")))
    (if (file-exists? cf)
	(process-voi (gather-voi-data 
		      (voi-id 
		       (find-nearest-voi-from-scanner-xyz-list
			(get-last-click))))
		     which)
        (abort-command "No click file was found. Try again!")
	)))

;;; FFF This needs to be changed to take into account whether the
;;; measure is mm or talairach, which is delivered by the viewer into
;;; the file, which which isn't at the moment handled (the 'mm is just 
;;; dropped on the floor.

(defun get-last-click ()
  (cdar (last (get-all-clicks))))

(defun get-all-clicks (&aux (cf (/tmp "jeff.foo")))
  (if (not (file-exists? cf))
      (abort-command "Something's wrong; Couldn't find any clicks.
Did you select a point and are you running the right viewer?")
      (mapcar #'read-from-string (read-lines-from-file cf))
      ))

(defun find-nearest-voi-from-scanner-xyz-list (xyz)
  (find-nearest-voi-from-scanner-xyz (first xyz) (second xyz) (third xyz)))

;;; Excluding a voi removes it from the voi table.  To make these
;;; changes permanent, the user MUST use 'avoi resave'.

(defvar *excluded-vois* ())

(defun exclude-clicked-voi ()
  (let* ((v (find-nearest-voi-from-scanner-xyz-list (get-last-click))))
    (exclude-voi (voi-id v))
    ))

(defun exclude-voi (p &aux (v (find-voi-by-id p)))
  (if (and v 
	   (yes-or-no-p "~%Really exclude partical ~a at ~a?" p (p2 (voi-scenter v))))
      (progn (push v *excluded-vois*)
	     (setq *vois* (remove p *vois*
				  :test #'(lambda (seek test)
					    (eq seek (voi-id test)))
				  ))
	     (format t "Excluded particle #~a; use 'avoi replace ~a' to re-include it.~%" p p)
	     )
    (format t "Nothing was excluded.~%")
    )
  )

(defun replace-voi (pn)
  (cond ((numberp pn)
	 (if (find-voi-by-id pn)
	     (abort-command "It's already there!"))
	 (let ((voi (find-voi-by-id pn *excluded-vois*)))
	   (if voi 
	       (progn (push voi *vois*)
		      (setq *excluded-vois* (remove voi *excluded-vois*))
		      (format t "Included #~a. ~a vois still excluded.~%"
			      pn
			      (if *excluded-vois* (length *excluded-vois*) "No"))
		      )
	     (format t "Voi #~a doesn't seem to have been excluded.~%" pn)
	     )))
	((eq pn 'all)
	 (mapcar #'(lambda (voi) (replace-voi (voi-id voi))) *excluded-vois*))
	(t (abort-command "Avoi include needs a voi number or 'all'~%"))
	))

;;; This is used for the continuous fft.  It can be set from the tk control
;;; window, or just through lisp.

(setq *fft-window-width* 32)

;;; This is needed in case it's been reset by the avoi i tk interface

(defun fft-window-width (&optional new)
  (if new
      (setq *fft-window-width* new)
    (if (numberp *fft-window-width*)
	*fft-window-width*
      (if (string-equal "" *fft-window-width*)
	  (abort-command "Need a window width!")
	(read-from-string *fft-window-width*)))))

;;; This is where the task-specific computation enters in.  Stat tells
;;; us what to do: 'basic 'resid 'anova etc.  Note that the voidata
;;; can come in as a real trace, or as a group trace, which looks
;;; like: (group (nv1...t1...) (nv2...t2...) ...), but that the
;;; general info package always looks the same.  The Nvi are the
;;; number of voxels in this roi, which is used to weigth the means.

(defvar *my-avoi-stats* ())

(defun process-voi (voidata stat &optional (plot? t) &aux voi trace)
 (setq trace (cadr voidata))
 (setq voi (car voidata))
 (format t "~%Particle ~a, at ~amm~%" (voi-id voi) (p2 (voi-scenter voi)))
 (let ((perstat (assoc stat *my-avoi-stats*)))
  (if perstat
      (funcall (eval (second perstat)) voi trace)
   (case stat
    ((basic b)
     (let* ((results (voi-stats trace 'regress)))
      (if *avoiplot* 
	  (plot1 (list (tpl-to-plot1 (cons " " (voi-stats trace 'basic))))
		 (format nil "Particle ~a raw signal, coef = ~a, r^2 = ~a"
			 (voi-id voi)
			 (p2 (getf results :tm)) 
			 (p2 (getf results :tr2))))
	) ; if
      )
    ) ; case basic

   ((resid r)
    (let* ((results (voi-stats trace 'regress)))
      (if *avoiplot*
	  (plot1 (list (tpl-to-plot1 (cons " " (getf results :tresids))))
		 (format nil "Particle ~a signal residuals: coef = ~a, r^2 = ~a"
			 (voi-id voi) (pround 2 (getf results :rm))
			 (pround 2 (getf results :rr2))))
	) ; if *avoiplot*
      ) ;let*
    ) ;case resid

   ;; (Note that some of these don't bother checking *avoiplot* since they
   ;;  aren't ever called w/o wanting a plot.)

   ((fft)
    (let ((fft (cdr (voi-stats trace 'fft))))
       (plot1 (list (tpl-to-plot1 (cons " " fft)))
	      (format nil "Particle ~a raw fft" (voi-id voi)))
       ))

   ((f)
      (plot1 (list (tpl-to-plot1 (cons " " (voi-stats trace 'f))))
	     (format nil "Particle ~a windowed F-score, width ~a, period ~a"
		     (voi-id voi)
		     (fft-window-width)
		     (prescription-period (segment-prescription *g*))
		     )
	     ))

   ;; The * form plots each trace in a group individually.

   ((f*)
    (plot1 (tpl-to-plot1* (voi-stats trace 'f*))
	   (format nil "Particle ~a windowed F-score, width ~a, period ~a"
		   (voi-id voi)
		   (fft-window-width)
		   (prescription-period (segment-prescription *g*)))
	      ))

   ((pc)
    (plot1 (list (tpl-to-plot1 (cons " " (voi-stats trace 'pc))))
	   (format nil "Particle ~a continuous power % change, width ~a, period ~a"
		   (voi-id voi) 
		   (fft-window-width)
		   (prescription-period (segment-prescription *g*)))
	   *percent-plot-yrange* 
	      ))
   ((pc*)
    (plot1 (tpl-to-plot1* (voi-stats trace 'pc*))
	   (format nil "Particle ~a continuous power % change, width ~a, period ~a"
		   (voi-id voi) 
		   (fft-window-width)
		   (prescription-period (segment-prescription *g*)))
	   *percent-plot-yrange* 
	      ))

   ((apc*)
    (plot1 (tpl-to-plot1* (voi-stats trace 'apc*))
	   (format nil "#~a Adjusted continuous % change, width ~a, period ~a"
		   (voi-id voi) 
		   (fft-window-width)
		   (prescription-period (segment-prescription *g*)))
	   *percent-plot-yrange* 
	      ))

   ((pcw*)
    (plot1 (tpl-to-plot1* (voi-stats trace 'pcw*))
	   (format nil "Particle ~a power % change, width ~a, period ~a, skip ~a"
		   (voi-id voi)
		   (fft-window-width)
		   (prescription-period (segment-prescription *g*))
		   (fft-window-width)
		   )
	   *percent-plot-yrange* 
	      ))

   ((fw*)
    (plot1 (tpl-to-plot1* (voi-stats trace 'fw*))
	   (format nil "Particle ~a windowed F-score, width ~a, period ~a, skip ~a"
		   (voi-id voi)
		   (fft-window-width)
		   (prescription-period (segment-prescription *g*))
		   (fft-window-width)
		   )
	      ))

   (t (abort-command "Invalid avoi calculation.  See ?avoi"))

   ) ; case stat
   ) ; if perstat
  ) ; let
 )

;;; The various voi stats are accomplished here for a given voi. Is
;;; smart in a very specific way in that if it gets a grouped trace,
;;; which begins with 'group, then it does whatever is desired (on a
;;; per-stat basis) for muliple traces, sometimes taking the average
;;; before, and sometimes after the basic computation.  

(defun voi-stats (trace stat)
 (case stat
   ;; Needs a basic clause in case the caller got a group trace.
   (basic
    (if (eq 'group (car trace))
	(find-mean-of-multiple-traces (cdr trace))
      trace)
    )

   (regress 
    (if (eq 'group (car trace))
	(setq trace (find-mean-of-multiple-traces (cdr trace))))
    (let* ((n (length trace))
	   (treg (regress (ncount n) trace))
	   (tresids (getf treg :resids))
	   (tm (getf treg :m))
	   (tb (getf treg :b))
	   (tr2 (getf treg :r2))
	   (rreg (regress (ncount n) (mapcar #'abs tresids)))
	   (rresids (getf rreg :resids))
	   (rm (getf rreg :m))
	   (rb (getf rreg :b))
	   (rr2 (getf rreg :r2))
	   )
      (format t " Trace = ~a(signal) + ~a (r^2=~a)~%" 
	      (pround 2 tm) (pround 2 tb) (pround 2 tr2))
      (format t " Resid = ~a(signal) + ~a (r^2=~a)~%" 
	      (pround 2 rm) (pround 2 rb) (pround 2 rr2))
      (list :treg treg
	    :tresids tresids
	    :tm tm
	    :tb tb
	    :tr2 tr2
	    :rreg rreg
	    :rresids rresids
	    :rm rm
	    :rb rb
	    :rr2 rr2
	    )))

   (anova 
    (if (eq 'group (car trace))
	(setq trace (find-mean-of-multiple-traces (cdr trace))))
    (anova4 trace))

   (fft 
    (if (eq 'group (car trace))
	(find-mean-of-multiple-traces
	 (mapcar #'(lambda (one-trace) 
		     (cons (car one-trace) ; replace the id/size
			   (raw-real-fft 
                             (first-n (fft-window-width) 
				      (cdr one-trace))))) ; ... removed here
		 (cdr trace))
	 )
      (raw-real-fft (first-n (fft-window-width) trace))
      )
    )

   (f
    (let ((period (prescription-period (segment-prescription *g*))))
      (if (eq 'group (car trace))
	  (find-mean-of-multiple-traces 
	   (mapcar #'(lambda (one-trace) 
		       (cons (car one-trace) ; replace the id/size
		       (continuous-windowed-f-score
		 	 (cdr one-trace) ; ... removed here
			 (fft-window-width) period)))
		   (cdr trace)))
	(continuous-windowed-f-score trace (fft-window-width) period)
	) ; if
      ))

   ;; The * form plots each trace in a group individually, reading the info
   ;; left by group construction in the car of each trace, that is: (id . volume)

   (f*
    (let ((period (prescription-period (segment-prescription *g*))))
      (if (eq 'group (car trace))
	  (mapcar #'(lambda (one-trace)
		      (cons (format nil "#~A(~a)"
				    (caar one-trace)
				    (cdar one-trace))
			    (continuous-windowed-f-score
			     (cdr one-trace)
			     (fft-window-width) period)))
		  (cdr trace))
	(abort-command "Pc* must operate on a group.")
	) ; if
      )) ; pc*

   (pc
    (let ((period (prescription-period (segment-prescription *g*))))
      (if (eq 'group (car trace))
	  (find-mean-of-multiple-traces
	   (mapcar #'(lambda (one-trace) 
		       (cons (car one-trace) ; replace the id/size
		       (continuous-windowed-pc
		 	 (cdr one-trace) ; ... removed here
			 (fft-window-width) period)))
		   (cdr trace)))
	(continuous-windowed-pc trace (fft-window-width) period)
	) ; if
      ))

   (pc*
    (let ((period (prescription-period (segment-prescription *g*))))
      (if (eq 'group (car trace))
	  (mapcar #'(lambda (one-trace)
		      (cons (format nil "#~A(~a)"
				    (caar one-trace)
				    (cdar one-trace))
			    (continuous-windowed-pc
			     (cdr one-trace) ; ... removed here
			     (fft-window-width) period)))
		  (cdr trace))
	(abort-command "pc* must operate on a group.")
	) ; if
      )) ; pc*

   (apc*
    (let ((period (prescription-period (segment-prescription *g*)))
	  (gmpc (get-grand-avoi-data 'pc)))
      ;; If there wasn't any grand mean continuous pc data calculated
      ;; then do it here and save it for later.
      (if (null gmpc)
	  (progn
	    (format t "(Calculating pc of mean for adjustment.)~%")
	    (set-grand-avoi-data 'pc 
	      (setq gmpc (continuous-windowed-pc
			  (get-grand-avoi-data 'mean)
			  (fft-window-width) period)))
	    ))
      (if (eq 'group (car trace))
	  (mapcar #'(lambda (one-trace)
		      (cons (format nil "#~A(~a)"
				    (caar one-trace)
				    (cdar one-trace))
			    (mapcar #'/
				    (continuous-windowed-pc
				     (cdr one-trace)
				     (fft-window-width) period)
				    gmpc)
			    ))
		  (cdr trace))
	(abort-command "pc* must operate on a group.")
	) ; if
      )) ; pc*

   (pcw*
    (let ((period (prescription-period (segment-prescription *g*))))
      (if (eq 'group (car trace))
	  (mapcar #'(lambda (one-trace)
		      (cons (format nil "#~A(~a)"
				    (caar one-trace)
				    (cdar one-trace))
			    (continuous-windowed-pc
			     (cdr one-trace) ; ... removed here
			     (fft-window-width) period
			     :skip (fft-window-width))))
		  (cdr trace))
	(abort-command "pcw* must operate on a group.")
	) ; if
      )) ; pcw*

   (fw*
    (let ((period (prescription-period (segment-prescription *g*))))
      (if (eq 'group (car trace))
	  (mapcar #'(lambda (one-trace)
		      (cons (format nil "#~A(~a)"
				    (caar one-trace)
				    (cdar one-trace))
			    (continuous-windowed-f-score
			     (cdr one-trace) ; ... removed here
			     (fft-window-width) period
			     :skip (fft-window-width))))
		  (cdr trace))
	(abort-command "fw* must operate on a group.")
	) ; if
      )) ; fw*

   ) ; case stat
 )

;;; By the time we get here, the list of traces must be just values
;;; all of which are the same length.  Note that the cdar of each trace
;;; is a multiplier, which is generally the number of voxels (added
;;; when the group is formed).  This is used to weight each entry so
;;; that we are effectively counting voxels, not vois.

(defun find-mean-of-multiple-traces (traces &aux means data)
  (dotimes (i (1- (length (car traces))))
    (setq data () k 0)
    (dolist (tr traces)
      (push (* (cdar tr) (nth i (cdr tr))) data)
      (incf k (cdar tr)))
    (push (/ (sum data) (float k)) means))
  (reverse means))

(defun ncount (i &aux r)
  (dotimes (v i r) 
    (push (1- (- i v)) r)))

(defun find-trace-for-voi (vn &optional (tpl *tpl*))
  (cdr (assoc vn tpl)))

;;; What we're after is the residuals.  This will tell us how much
;;; each value differs from the mean, which is a measure of
;;; activation.  What we're after is a reduction in residuals over
;;; time when learning takes place.  So we do TWO regressions: one on
;;; the signal, and one on the abs(residuals).  The slope of the
;;; SECOND regression tells us the trend in the signal.

(defun put-out-resd-file (r file)
  (with-open-file (f file
		     :direction :output
		     :if-exists :supersede)
     (dotimes (i (length (car r)))
       (dolist (l r)
         (format f "~a " (nth i l))
	 )
       (format f "~%")
       )
     (format t "wrote ~a" file)
     )
  )

;;; For plotting particles.  Note that this assume that the first
;;; (car) is a label and pops it off, so outsiders have to be sure to
;;; ADD something there if there's not anything there already.

(defun tpl-to-plot1 (l &aux r)
  (dotimes (i (length (cdr l))) (push (1+ i) r))
  (list (car l)
	(reverse r)
	(cdr l)))

;;; Multiple version of the same game; Again, headers get poped and replaced.

(defun tpl-to-plot1* (l*)
  (mapcar #'tpl-to-plot1 l*))

;;; This is a try at a really simple tk interface to the avoi tools so that
;;; you can press buttons on the viewers and get plots of the timecourse.
;;; We delete the file beforehand, and favoi-from-click file at the moment
;;; just plots the last entry.

(defvar *personal-avoi-buttons* nil)
(defvar *my-avoi-buttons* nil) ; This is set in the users my-avoi-buttons file.

(defun iavoi (&aux f)
  (setq *avoiplot* t) ; note that this is never set off!
  (if (file-exists? 
       (setq f (format nil "/tmp/~a.jeff.foo" (system::getenv "USER"))))
      (delete-file f))
  (tk::tkconnect)
  (tk::button '.rs :text "Raw Signal" :command '(favoi-from-click-file 'basic))
  (tk::button '.lr :text "Linear Residuals" 
	      :command '(favoi-from-click-file 'resid))
  (tk::button '.rf :text "Raw FFT" :command '(favoi-from-click-file 'fft))
  (tk::button '.cf :text "C.W. F-Score" :command '(favoi-from-click-file 'f))
  (tk::button '.cp :text "C.W. %Change" :command '(favoi-from-click-file 'pc))
  (tk::button '.dp :text "!EXCLUDE!" :command '(exclude-clicked-voi))
  (tk::frame '.fwin)
  (tk::label '.fwinl :text "Window")
  (tk::entry '.fwine :textvariable '*fft-window-width* :width 5)
  (tk::pack '.fwinl '.fwine :in '.fwin :side 'left)
  ;; Load up the user's buttons, if any, and set them up.
  (setq *personal-avoi-buttons* ())
  (let ((mabfilename 
	 (pfn *user-top-path* "macros/my-avoi-buttons")))
    (if (file-exists? mabfilename)
	(progn
	  (load mabfilename)
	  (if (null *my-avoi-buttons*)
	      (abort-command "You have a macros/my-avoi-buttons file but it apparently doesn't setq *my-avoi-buttons*"))
	  (setq *personal-avoi-buttons*
		(mapcar #'convert-personal-avoi-button *my-avoi-buttons*))
	  )))
  ;; And end it all by bringing up the window.
  (apply #'tk::pack (append '(.rs .lr .rf .cf .cp .fwin .dp)
			    *personal-avoi-buttons*))
  )

(defun convert-personal-avoi-button (spec)
  (case (first spec)
     (button
      (let ((bname (gentemp ".PB")))
	(tk::button bname 
		    :text (second spec) 
		    :command (third spec)
		    )
	bname))
     (input
      (let ((fname (gentemp ".WF"))
	    (ename (gentemp ".WE"))
	    (lname (gentemp ".WL"))
	    )
	(tk::frame fname)
	(tk::label lname :text (second spec))
	(tk::entry ename 
		   :textvariable (third spec) 
		   :width (or (fourth spec) 5)
		   )
	(tk::pack lname ename :in fname :side 'left)
	fname))
     (t (abort-command "Your my-avoi-buttons file has a weird entry: '~a' in it!"
		       (first spec)))
     ))

;;; Split a signal into four equal parts and run a balanced two-way
;;; anova on the four parts.  Each part is composed of the max half
;;; and the min half of the signal.  WARNING: This makes sure that
;;; there are an even number of items in the signal, otherwise ist
;;; simply arbitrarily deletes the first one!!  Note that this really
;;; ought to be a repeated measures anova, which it isn't!

(defun anova4 (signal)
  (if (not (= (truncate (/ (length signal) 2)) (/ (length signal) 2)))
      (abort-command (format nil "Barf! Odd length signal (~a vals) cdr'ed!~%" 
			     (length signal))))
  (let* ((vs (split-into-4 signal))
         ;; It's quite hard to get straight what we want to be which
	 ;; value here.  The group is the uppers v. lowers, and the m
	 ;; is the measure (left v. right half).
	 (g1m1 (first vs))
	 (g1m2 (second vs))
	 (g2m1 (third vs))
	 (g2m2 (fourth vs)))
    (if (not (= (length g1m1) (length g1m2) (length g2m1) (length g2m2)))
	(format t "Cell sizes aren't the same: ~a ~a ~a ~a; some data will be lost!~%"
		(length g1m1) (length g1m2) (length g2m1) (length g2m2)))
    (append (list :g1m1 g1m1 :g1m2 g1m2 :g2m1 g2m1 :g2m2 g2m2)
	    (anova2r (mapcar #'list g1m1 g1m2) 
		     (mapcar #'list g2m1 g2m2)
		     ))
    ))

;;; This is intended to take apart the oscillating signal, as: (10 5 9
;;; 4 8 3) -> (10 9 8) (5 4 3) It bases the split upon the regression
;;; equation through the points.  First we compute the regression and
;;; collect everything above v. below the regression line.  Then we
;;; split each of those in half.  

(defun split-into-4 (trace)
  (let* ((n (length trace))
	 (treg (regress (ncount n) trace))
	 (tresids (getf treg :resids))
	 tops bottoms ; these get set below
	 )
    ;; The sign of the residuals will tell us which side of the line
    ;; each item falls on.
    (mapcar #'(lambda (r v) (if (> r 0) (push v tops) (push v bottoms)))
	    tresids trace)
    ;; But they are reversed!
    (setq tops (reverse tops))
    (setq bottoms (reverse bottoms))
    (let ((ts (split-in-half tops))
	  (bs (split-in-half bottoms)))
      (append ts bs))
    ))

;;; Warning, if the length isn't even this is going to short the first group.

(defun split-in-half (l &aux a)
  (dotimes (n (truncate (/ (length l) 2)))
    (push (pop l) a))
  (list (reverse a) l))

;;; --- The avoi load/append wizard looks through the log for stat
;;; avoi and tempate commands trying to help out in what args you want
;;; to give to the avoi command, and then calls either avoi-load... or
;;; append... as apropos.  We walk thru all the AVOI entries in the
;;; log and try to find ones that seem likely to load, then offer them
;;; to the user. 

(defun avoi-wizard (load/append &aux k tplist)
 (let ((avois (mapcar #'cdddr (find-rlog-entries 'avoi)))
       (tplcache (mapcar #'cdddr (find-rlog-entries 'template)))
       (label-entries (mapcar #'cdddr (find-rlog-entries 'label-voi-set)))
       )
   (if avois
       (prog nil
	 (format t "Select from these by the number on the left:~%")
	 (format t "~5tThreshold~15tcount~22tmask from~40tcondition masked~60tlabels~%")
	 (format t "~5t---------~15t-----~22t---------~40t----------------~60t------~%")
	 (setq k 1)
	 (dolist (avoi avois)
	   (let* ((threshold (getf avoi 'threshold))
		  (count (getf avoi 'count))
		  (mask-formed-from (getf avoi 'mask-formed-from))
		  )
	     (if (and (file-exists? (getf avoi 'saved-in))

		      ;; test that it hasn't already been shown (was done twice)
		      (not (find (list 'ignore threshold count 
				       mask-formed-from 'ignore)
				 tplist :test #'(lambda (a b) 
						  (equal (first-n 3 (cdr a))
							 (first-n 3 (cdr b))
							 ))
				 )) ; not
		      )
		 ;; Show the entry with and without signal info.
		 (prog (tpl-conds-listed labels)
		   ;; The loner avoi table (no template)
		   (format t "~a:~5t ~a~15t ~a~22t ~a~40,3tno signal info~60t~a~%"
			   k threshold count mask-formed-from 
			   (setq labels 
				 (find-voi-set-labels label-entries
			           threshold count mask-formed-from nil
				   'silently))
			   )
		   (push (list k threshold count mask-formed-from nil labels) 
			 tplist)
		   (incf k)
		   ;; There can be multiple tpls from different conditions
		   ;; on the same t/c/from set.  We have to check here that
		   ;; we don't report it twice here as well.  This is all
		   ;; SO MESSY; there has to be better way!
		   (dolist (tpl (find-matching-template-entries
				 threshold count mask-formed-from
				 tplcache))
		     (let* ((condition (getf tpl 'condition))
			    (tplfile (rlog-dir (format nil "tpl.~a.mean.lsp" 
						       (getf tpl 'saved-in))))
			    labels
			    )
		       (if (and (file-exists? tplfile)
				(not (member condition tpl-conds-listed)))
			   (progn
			     (push condition tpl-conds-listed)
			     ;; and its paired template info
			     (format t "~a:~5t ~a~15t ~a~22t ~a~40,3t ~a~60t~a~%"
				     k threshold count mask-formed-from condition
				     (setq labels 
					   (find-voi-set-labels label-entries
					     threshold count mask-formed-from
					     condition 'silently))
				     )
			     ;; Note that the args here get passed, through apply,
			     ;; below to either load or to append the avoi data.
			     (push (list k threshold count mask-formed-from 
					 condition labels)
				   tplist)
			     (incf k))
		   ))))) ; if let* dolist progn if
	     )) ; let* dolist
	 (let ((sel (prompt-for-number "Selection" 'abort)))
	   (if (eq 'abort sel) (abort-command "Nothing selected."))
	   (apply (case load/append 
			(load #'load-avoi-file)
			(append #'append-additional-tpl-file)
			;; if we're not doing either a load or append,
			;; just return the selected set info.
			(t (let ((vs (cdr (assoc sel tplist))))
			     (return (values (first vs) ; threshold
					     (second vs) ; count
					     (third vs) ; mff
					     (fourth vs) ; cond
					     (fifth vs) ; labels
					     )))
			   )
			)
		  (cdr (assoc sel tplist)))
	   (clear-compute-cache)
	   )
	 ) ; prog
    ;; else clause
    (abort-command "No valid templates were found in the log index.~%")
    )) ; if let
 )

(defun find-matching-template-entries (threshold count mask-formed-from 
						 tplcache &aux r)
  (dolist (l tplcache)
    (if (and (equal threshold (getf l 'threshold))
	     (equal count (getf l 'count))
	     (equal mask-formed-from (getf l 'mask-formed-from)))
	(push l r)))
  r)

;;; --- Region labeling maintains a database in the aux directory
;;; called "avoi-regions.brain.segment" which lets you give names to
;;; arbitrary regions of the brain and then associates vois with those
;;; region labels.  The region table is just a big list of lists, each
;;; one of which is a name and two points which define the bounds of
;;; the region in scanner coords (which is what we get from mriview).
;;; As: (... (frontal (-54.456 12.345 5.345) (-43.456 -12.45 34.56))
;;; ...)  There are a number of commands that work with these regions.

(defvar *avoi-regions* ())

(defun avoi-region-labeling (args)
  (case (pop args)
    ((save) (save-region-table))
    ((add new n a)
     (add-new-avoi-region (pop args)))
    ((list l)
     (list-rois-and-regions))
    ((d delete)
     (delete-region-label (pop args)))
    ((s sum summarize summary report r)
     (report-particles-by-region))
    (t (abort-command "Missing avoi region command.  See help for avoi."))
    ))

;;; Report generation is really the final purpose of all this avoi
;;; junk.  We want to indicate how many particles are in each region.
;;; Unfortunately, at the moment, since regions aren't recursively
;;; defined, we need to look at all combinations (or at least all
;;; combinations in which particles exist) of region labels.

(setq *vvol* 3.45)

(defun report-particles-by-region ()
  (if *excluded-vois* 
      (format t "(~a vois are excluded!)~%" (length *excluded-vois*)))
  (format t "(WARNING: total.v in cubic mm assumes ~a cubic mm voxels!)~%" 
	  (p2 *vvol*))
  (report-loaded-avoi-info)
  (dolist (report-entry (collect-particles-by-region))
    (if (not (zerop (length (cdr report-entry))))
	(format t "~a: n=~a, t.voxels: ~a, m.voxels: ~a, t.volume in cmm: ~a~%" 
		(car report-entry) 
		(length (cdr report-entry))
		(p2 (sum (mapcar #'voi-volume (cdr report-entry))))
		(p2 (mean (mapcar #'voi-volume (cdr report-entry))))
		(p2 (* *vvol* (sum (mapcar #'voi-volume (cdr report-entry)))))
		)
      )))

(defun collect-particles-by-region (&aux combined-regions particles-and-regions)
  (setq combined-regions 
	(mapcar #'list (all-sublists (mapcar #'car *avoi-regions*))))
  ;; This is sort of complex and probably too messy. It runs through every
  ;; vois and every possible combination of regions.  We want to assign vois
  ;; to every region that it is in, but not to regions that it is not in.  This
  ;; entails checking that the voi's region description accord with the 
  ;; region combination, which is done in the subfn.
  (dolist (v *vois*)
    (let ((vreg (get-region-labels-from-scanner-loc (voi-scenter v))))
      (dolist (creg combined-regions)
        (if (vreg-accords-with-creg vreg (car creg)) (pushnew v (cdr creg)))
	)))
  combined-regions
  )

;;; If the creg is more specific than the vreg, we do NOT add it in.
;;; This means that there are descriptors in the creg that are NOT in
;;; the vreg.  Basically, all creg descriptors have to be in the vreg,
;;; but not vice versa.

(defun vreg-accords-with-creg (vreg creg)
  (dolist (cd creg t)
    (if (not (member cd vreg)) (return nil))))

;;; Various region defn and editing fns.

(defun delete-region-label (l)
  (let ((rl (assoc l *avoi-regions*)))
    (if rl (if (yes-or-no-p "Really delete ~a?" l)
	       (setq *avoi-regions* (remove rl *avoi-regions*)))
      (abort-command "No such region exists.")
      )
    )
  (save-region-table)
  )

;;; Choosing the bounding box for the region involves entering two
;;; points.  I briefly did this heuristically, but decided that my
;;; heuristic, which involved finding the last and most distant
;;; non-identical (within some limit) button presses, was, to put it
;;; midly, moronic.  Therefore, I now just ask the user to point to
;;; one end and then the other.  There are various methods, set by
;;; the *region-spec-method* pref.

(defun add-new-avoi-region (name)
  (if (assoc name *avoi-regions*)
      (progn (format t "There's a region by that name already.~%")
	     (if (yes-or-no-p "Replace it?")
		 (setq *avoi-regions*
		       (remove (assoc name *avoi-regions*) *avoi-regions*))
	         (abort-command "Aborted.  No changes.")))
    )

  (case *region-spec-method*
    ((d diagonal) (add-region-by-click name))
    ((v vicinity) (add-region-by-vicinity name))
    ((r range) (add-region-by-box name))
    (t (abort-command "Your *region-spec-method* is mis-set; use the prefs command to fix it."))
    ) ; case
  (save-region-table)
  )

(defun add-region-by-click (name &aux p1 p2)
  (if (file-exists? (/tmp "jeff.foo"))
      (delete-file (/tmp "jeff.foo")))
  (prompt-for-string "Indicate the first corner of the region and press return.")
  (setq p1 (get-last-click))
  (prompt-for-string "Indicate the opposite corner of the region and press return.")
  (setq p2 (get-last-click))
  (push (list name p1 p2) *avoi-regions*)
  (format t "Added ~a." (car *avoi-regions*))
  )

(defun add-region-by-box (name &aux p1 p2)
  (if (file-exists? (/tmp "jeff.foo"))
      (delete-file (/tmp "jeff.foo")))
  (let ((xp (get-click-pair "first (X)"))
	(yp (get-click-pair "second (Y)"))
	(zp (get-click-pair "third (Z)"))
	)
    (setq p1 (list (first (first xp))
		   (second (first yp))
		   (third (first zp))))
    (setq p2 (list (first (second xp))
		   (second (second yp))
		   (third (second zp))))
    (push (list name p1 p2) *avoi-regions*)
    (format t "Added ~a." (car *avoi-regions*))
    )
  )

(defun get-click-pair (which &aux p1 p2)
  (format t "~%--------------~%  Entering the ~a coordinate range...~%~%" which)
  (prompt-for-string "Select one end of this range and press return.")
  (setq p1 (get-last-click))
  (prompt-for-string "Select the other end of this range and press return.")
  (setq p2 (get-last-click))
  (format t "Got: ~a ~a.~%~%" p1 p2)
  (list p1 p2)
  )

(defun add-region-by-vicinity (name &aux c p1 p2)
  (if (file-exists? (/tmp "jeff.foo"))
      (delete-file (/tmp "jeff.foo")))
  (prompt-for-string "Indicate the center of the region and press return.")
  (setq c (get-last-click))
  (prompt-for-string "Indicate the edge of the region and press return.")
  (setq p1 (get-last-click))
  ;; The second point is symmetric about the center from the first.
  (setq p2 (find-symmetric-point p1 c))
  (push (list name p1 p2) *avoi-regions*)
  (format t "Added ~a." (car *avoi-regions*))
  )

(defun find-symmetric-point (p1 c)
  (withloc p1x p1y p1z p1
    (withloc cx cy cz c
      (list (- cx (- p1x cx))
	    (- cy (- p1y cy))
	    (- cz (- p1z cz))
	    ))))

(defun save-region-table ()
  (with-open-file (f (region-database-filename)
		     :direction :output
		     :if-exists :supersede)
		  (format f "(setq *avoi-regions* '~a)" *avoi-regions*))
  )

(defun region-database-filename ()
  (/aux "avoi-regions" 
	(study-brain-id *s*)
	(segment-name *g*)))

;;; Run through the voi list and report in which region each particle lives.

(defun list-rois-and-regions ()
  (dolist (r *avoi-regions*)
    (format t "~a includes ~a through ~a~%" (car r) (p2 (second r)) (p2 (third r)))
    )
  (dolist (v *vois*)
    (describe-voi v))
  (if *excluded-vois*
      (progn
	(format t "*** Excluded vois are:~%")
	(dolist (v *excluded-vois*)
		(describe-voi v))
	))
  )

(defun describe-voi (v)
  (let* ((voiloc (voi-scenter v))
	 (labels (get-region-labels-from-scanner-loc voiloc))
	 )
    (format t "#~a at ~a lies in ~a~%" (voi-id v) (p2 (voi-scenter v))
	    (or labels "none of the known regions"))
    ))

(defun get-region-labels-from-scanner-loc (voiloc &aux labels)
  (withloc x y z voiloc
    (dolist (r *avoi-regions*)
       ;; The point is in the region if it lies between each or the dimension limits
       (withloc x1 y1 z1 (second r)
         (withloc x2 y2 z2 (third r)
	   (if (and (lies-between x x1 x2)
		    (lies-between y y1 y2)
		    (lies-between z z1 z2))
	       (push (car r) labels))
	   ))
       )
    )
  labels
  )

(defun lies-between (v v1 v2)
  (or (and (< v1 v2) (> v v1) (< v v2))
      (and (> v1 v2) (< v v1) (> v v2))
      ))

;;; This is a different take in the database problem.  In this case we
;;; have a table entered on the side from various papers, and we
;;; choose the nearest label from that table.  This unfortunately
;;; requires you to enter the t.coords because the viewer doesn't give
;;; them to me.

(defvar *tu-distance-limit* 5.0) ; things father than this aren't reported.

(defun report-nearest-tc-label (args)
  (or *vois* (abort-command "You have to load a voi set to do this."))
  (cond ((eq 3 (length args))
	 (rntclxyz args *tu-distance-limit*))
	((or (null args) (eq 'all (car args)))
	 (mapcar 
	  #'(lambda (voi)
	      (format t "~%~%#~a:~%" (voi-id voi))
	      (let ((tcenter (voi-tcenter voi)))
		(if tcenter
		    (rntclxyz tcenter
			      (or (cadr args)
				  *tu-distance-limit*))
		  (format t "   has no talairach center.~%")
		  ))
	      )
	  *vois*)
	 )
	(t (abort-command "Bad 'avoi tc' args.
Form is: avoi tc { ['all' [minlimit]] | x y z }
See '?avoi'."))
	))

(defun rntclxyz (xyz limit)
  (let ((locs (find-nearest-tc-labels xyz limit)))
    (if locs
        (dolist (loc locs)
	  (format t "= ~a~%  (~atu from ~a)~%"
	          (second loc) (p2 (first loc)) (third loc))
	  )
      (progn (format t "Nothing found within ~atu; Adding 5 and retrying.~%" limit)
	     (rntclxyz xyz (incf limit 5)))
      )
    )
  )

;;; The tcbase contains two kinds of entries: base entries look like:
;;; ...((tx ty tz) "name" citenumber)...
;;; Citation entries look like:
;;; ...(citenumber "Citation")...

(defvar *tcbase* ()) ; the file setqs this.
(defvar *tccites* ()) ; the file setqs this.

(defun find-nearest-tc-labels (tc limit &aux close-tc-entries close-distances)

  ;; load the database in case it's not already
  (if (null *tcbase*) (load (homefile "tcbase.lsp")))

  (withloc tx ty tz tc
    (dolist (tc-entry *tcbase*)
      (withloc ex ey ez (car tc-entry)
	(let ((distance (xyz-distance tx ty tz ex ey ez)))
	     (if (> limit distance)
		 (progn (push distance close-distances)
			(push tc-entry close-tc-entries)
			)
	     ))
      )) ; withloc dolist
    ) ; withloc
  
  ;; Report each along with their unpacked citations, sorted by nearness.
  (sort (mapcar #'(lambda (entry distance) 
		    (list distance 
			  (second entry)
			  (cadr (assoc (third entry) *tccites*))))
		close-tc-entries close-distances)
	#'(lambda (a b) (< (car a) (car b))))
  )

;;; --- Statistics by region label.  Here we group the data from a
;;; whole region for voi stats instead of just one particle.  The
;;; trick, though, is that we want to average the RESULTS of the stats
;;; (that is, post-stat), NOT the voi signals pre-stat.  Since
;;; process-voi does the stats, this is a little tricky.  What happens
;;; is that the group of traces gets packaged into a new, qseudo trace
;;; where the actual trace data read as: (group (nv1 d11 d12...) (nv2
;;; d21 d22...) ...)  Process-voi knows how to deal with these.  Nvi
;;; is the number of voxels in the data.  This is used in averaging to
;;; weight the results.

(defun find-nvois-or-region-tag (l)
  (if (null l)
      (group-vois (find-voi-numbers-from-labels l) 'all)
    (if (dolist (i l t) (if (not (numberp i)) (return nil)))
	(group-vois l l)
      (group-vois (find-voi-numbers-from-labels l) l))
    )
  )

(defun find-voi-numbers-from-labels (labels &aux vs)
  (dolist (l labels)
    (if (not (assoc l *avoi-regions*))
        (abort-command "Some of the labels you gave are illegal")))
  (dolist (v *vois*)
    (let ((rls (get-region-labels-from-scanner-loc (voi-scenter v))))
      (if (or (null labels) (region-names-overlap? labels rls))
	  (push (voi-id v) vs))))
  vs)

;;; We need to decide if the voi region label list is within the bounding
;;; region label list.

(defun region-names-overlap? (bounds voilabs)
  (dolist (b bounds t)
    (if (not (member b voilabs)) (return nil))))

;;; Put together the information in *vois* with the information in
;;; *tpl* for that voi where the voi is given by number.

(defun gather-voi-data (voin)
  (list (find-voi-by-id voin)
	(find-trace-for-voi voin)))

;;; Group together the data from a list of particles, making it look
;;; like just one.  Everything but the data itself is averaged, so that
;;; the locations, etc, are means.  

(defun group-vois (vois label &aux sums means)
 (let* ((voidata (mapcar #'gather-voi-data vois))
	(voistructs (mapcar #'car voidata)))
   (list
    ;; Create a bogus meta-voi made up from the individuals.
    (make-voi :id (format nil "~a (n=~a)" label (length vois)) ; made up name
	      :volume (mean (mapcar #'voi-volume voistructs))
;	      :mean (mean (mapcar #'voi-mean voistructs))
;	      :peak (mean (mapcar #'voi-peak voistructs))
;	      :icenter (lmean (mapcar #'voi-icenter voistructs))
	      :scenter (lmean (mapcar #'voi-scenter voistructs))
	     )
    ;; And the data as a group, along with the voi id and number of
    ;; voxels in the voi.  These are used in various graph identification
    ;; and other functions by callers who get groups as input.
    (cons 'group (mapcar #'(lambda (vd) (cons (cons (voi-id (car vd))
						    (voi-volume (car vd)))
					      (second vd) ; the data itself
					      ))
			 voidata))
   )
 ))

;;; Saving the voi table back usually only makes sense after you've included
;;; or excluded stuff.

(defun resave-voi-table ()
  (let ((fn (car *loaded-voi-info*)))
    (format t "This will destroy the voi table '~a';~%" fn)
    (if (yes-or-no-p "Are you sure that you want to do this?")
	(progn
 	 (with-open-file (f fn :direction :output :if-exists :supersede)
  	   (format f "(setq *vois* '(~%")
	   (dolist (v *vois*) (format f "~a~%" v))
	   (format f "))~%")
	   (format f "(setq *excluded-vois* '(~%")
	   (dolist (v *excluded-vois*) (format f "~a~%" v))
	   (format f "))~%")
	   )
	 (format t "Done!~%")
	 (log! 'avoi-resave "voi-filename ~a" fn)
	 )
      (format t "Resave aborted!")
      )
    ))
      
;;; For log listing:

(defun summarize-avoi-resave-log-entry (rec)
  (format t "~a~%" rec))

;;; --- Avoi note taking puts a remark into the notes and log which
;;; can be used later on during analysis to identify specifically
;;; indicated voi sets.  The note added to the log includes the id of
;;; the voi set.  (Normal notes do not go into the log.index!)

(defun avoi-note ()
  (let ((account (read-lines-until-period "Voi Set Notes >")))
    (log! 'avoi-note "voi-filename ~a note ~s" (car *loaded-voi-info*) account)
    (save-note `("voi notes regarding" ,(car *loaded-voi-info*) ,@account))
    ))

;;; For log listing:

(defun summarize-avoi-note-log-entry (rec)
  (format t "~a~%" rec))

;;; --- Labeling (or "calling" as it's called because you say 'call
;;; voi <label>' prompts for a voi set (like 'avoi l') and then enters
;;; a log entry indicating that the voi set has the given name.  The
;;; label table is built by the 'avoi l' processing.  As with labeling
;;; results, this permits the relationship between voi sets and labels
;;; to be many to many.  Also, since the label information is
;;; maintained in the log index and is rebuilt each time, it relies
;;; upon the PROPERTIES of the voi set which is actually probably okay
;;; since voi sets SHOULDN'T really change labels if they are
;;; recreated.

(defun label-voi-set (label)
  (multiple-value-bind (threshold count mask-formed-from condition)
		       (avoi-wizard nil)
    (if threshold
	(log! 'label-voi-set "threshold ~a count ~a mask-formed-from ~a condition ~a label ~a"
	      threshold count mask-formed-from condition label)
      )))

(defun summarize-label-voi-set-log-entry (entry)
  (print entry))

;;; This has to go through each log entry and collect up the added and
;;; deleted labels for this particular voi set.  This is a real mess
;;; and should be done in some entirely different manner, like with a
;;; database of voi sets or something.  Ugh.

(defun find-voi-set-labels (label-entries threshold count 
			    mask-formed-from condition 
			    &optional (silently nil)
			    &aux labels)
  (dolist (l label-entries)
    (let ((lt (getf l 'threshold))
	  (lk (getf l 'count))
	  (lm (getf l 'mask-formed-from))
	  (lc (getf l 'condition))
	  (label (getf l 'label))
	  )
      (if (and (eq threshold lt)
	       (eq count lk)
	       (eq mask-formed-from lm)
	       (eq condition lc))
	  (let* ((label-string (symbol-name label))
		 (+-char (aref label-string 0))
		 (real-label (read-from-string 
			      (string-left-trim "+-" label-string)))
		 )
	    (if (eq #\- +-char)
		(if (member real-label labels)
		    (setq labels (remove real-label labels))
		  (if (not silently)
		      (abort-command "The label ~a wasn't assigned to this voi set." real-label)
		    )
		  )
	      (pushnew real-label labels)
	      ) ; if eq...
	    ) ; let*
	) ; if and 
      )) ; let dolist
  labels
  )

;;; For recovering results that have a particular label.  This is
;;; really messy because it has to replicate a lot of the label
;;; processing done in find-voi-set-labels.  We have to kind of trace
;;; labels through the log history-- well, only the label of interest,
;;; but it's still a mess! (Double ugh!)

(defun find-labeled-voi-sets (target-label &aux table)
  (dolist (ls (really-find-rlog-entries 'label-voi-set))
    (let* ((l (nthcdr 3 ls))
	   (lt (getf l 'threshold))
	   (lk (getf l 'count))
	   (lm (getf l 'mask-formed-from))
	   (lc (getf l 'condition))
	   (ll (getf l 'label))
	   (label-string (symbol-name ll))
	   (+-char (aref label-string 0))
	   (real-label (read-from-string 
			(string-left-trim "+-" label-string)))
	   )
      (if (eq real-label target-label)
	  (if (eq #\- +-char)
	      (setq table (remove (list lt lk lm lc) table :test #'equal))
	    (pushnew (list lt lk lm lc) table :test #'equal)))
      ))
  table)

(defun load-voi-set-by-label (label &aux set)
  (let ((sets (find-labeled-voi-sets label)))
    (cond ((null sets)
	   (abort-command "No voi set has the label ~a!" label))
	  ((not (null (cdr sets)))
	   (abort-command "The label ~a refers to more than one voi set!" label))
	  (t (setq set (first sets))
	     (load-avoi-file (first set)
			     (second set)
			     (third set)
			     (fourth set)
			     nil))
	  )))
