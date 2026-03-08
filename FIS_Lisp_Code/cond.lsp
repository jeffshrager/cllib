;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; --- This is the condition manager.  Its job is to maintain the
;;; condition graph and translate into and out of it.  Basically, the
;;; condition graph translates names given to conditions by users into
;;; the numbers of conditions in the outfile.  There are two sets of
;;; things called condition names.  The "basic" condition names are
;;; those given to the conditions by the designer of the study.  These
;;; are largely irrelevant to FIS (although they form the default
;;; condition graph.  The graph contains virtual condition names that
;;; tell FIS how condition names are composed together to form new
;;; condition names.  For instance, let's say that the basic names are:
;;; Trial 1 = A, Trials 2 = B, Trial 3 = C, and just to make it
;;; interesting, Trials 4, 5, and 6 are all called D.  The initial
;;; condition graph would look like:
;;;  ((a 1) (b 2) (c 3) (d 4 5 6))
;;; Now, the user might want to composed together some of these conditions
;;; into new structures, so, maybe a b and c go together into a "first"
;;; condition: (...(first a b c) ...).  Etc.  Whenever a condition
;;; is required by FIS, it gets translated down to the numerical trials
;;; to which it refers by the condition manager.  By the means, users
;;; can create lots of meaningful reorganizations of their data.

;;; --- The condition graph is kept in *cg* as a list, just as above.
;;; This isn't the most efficient representation, but it'll do.  It is
;;; also represented in two places: the brain directory, and the
;;; user's local context directories.  Each time you create a new
;;; context, a cg is created by copying over the study cg.  

(defvar *cg* ())

;;; Changes that the user makes to the cg are saved back in his or her
;;; local cg, NOT in the study cg.  The way this works is QUITE messy
;;; (see extract-user-conditions).  The birth conditions include both
;;; the things in the mriheader AND those that the mr tech might have
;;; entered in a study-global fis.conditions file.  These are
;;; collectively stored here in case of a resave, at which point they
;;; are subtracted out, as above.

(defvar *birth-conditions* ())

;;; This builds or rebuilds the condition graph (if it is changed).
;;; Getting the condition graph together is a little complicated.  It
;;; is divided into a bunch of parts; some of which are on the study
;;; directory, some of which is in the segment directory, and some of
;;; which are in the user's context.  We always build the basic
;;; condition graph from the mriheader file of the out directory.
;;; Then we overlay that with a pecking order of more specialized cg's
;;; from these other places.  Overlays are checked for *simple*
;;; problems, but not for things like infinite loops.

(defun build-condition-graph ()
  (setq *cg* ())
  (if (file-exists? (pfn (invert-output-dir *g*) "MriHeader"))
      (get-mriheader-conditions)
      (format t "(Someone seems to have removed the /out directory.~% Can't load the MriHeader file to build the condition graph.~% It's gonna be leaf-less!  Sorry.)~%"))
  ;; Install the condition graph from the study itself.
  (unify-condition-graph (segment-condition-graph *g*))
  ;; And record all of the above in order to distinguish them from the user's.
  (setq *birth-conditions* (copy-tree *cg*)) ; for extraction on resaving
  ;; Now get the user's own stuff.
  (unify-condition-graph-from-file 
   (pfn (segment-path *g*) "fis.conditions"))
  (unify-condition-graph-from-file 
   (result-set-file "fis.conditions"))
  )

(defun get-mriheader-conditions ()
  (let ((mhlines (read-lines-from-file (pfn (invert-output-dir *g*) "MriHeader"))))
    (dolist (mhline mhlines)
      (let ((entry (parse-string mhline)))
	(if (and (> (length (car entry)) 5)
		 (string-equal "trial" (subseq (car entry) 0 5))
		 (not (eq #\s (aref (car entry) 5)))
		 )
	    (push (list (read-from-string (third entry))
			(read-from-string (subseq (car entry) 5)))
		  *cg*
		  ))
	))))

;;; Combine a read cg into the internal *cg* and check for ambiguous
;;; names.

(defun unify-condition-graph-from-file (file &aux newcg)
  (if (file-exists? file)
      (progn
	(format t "Getting condition entries: ~a.~%" file)
	(with-open-file (f file :direction :input)
	  (setq newcg (read f)))
	(unify-condition-graph newcg))
    ))

(defun unify-condition-graph (newcg)
  ;; Put the two together and warn about shadowing.
  (dolist (newentry newcg)
    (let ((oldentry (assoc (car newentry) *cg*)))
      (if oldentry
	  (progn ; yell and replace the old with the new
	    (format t "** Warning: condition ~a is being replaced!~%" 
		    (car newentry))
	    (rplacd oldentry (cdr newentry)))
	(push newentry *cg*) ; else just push it on top.
	)))
  )

;;; --- Turn a condition or list of them into the trial numbers that they
;;; represent.  If a name doesn't expand to anything, it is simply 
;;; returned itself. You have to be a little careful about which of these
;;; you call because reduce-condition will take a list but will assume that
;;; it is a SINGLE CONDITION made up of multiple other conditions and will
;;; compose the results together!

(defun reduce-condition-list (cl)
  (mapcar #'reduce-condition cl))

(defun reduce-condition (c)
  (remove-duplicates (rcl2 c)))

;;; A lovely classic recursive defn.

(defun rcl2 (cl)
  (cond ((null cl) ())
	((numberp cl) (list cl))
	((atom cl)
	 (let ((expansion (cdr (assoc cl *cg*))))
	   (if expansion (rcl2 expansion) (list cl))))
	(t (append (rcl2 (car cl))
		   (rcl2 (cdr cl))))
	))

;;; --- Condition selection, by call or user interaction.  This 
;;; tries to translate the conditions to the trial number list, as above,
;;; if possible.  We need to check in two places: the trial
;;; conditions, which are basic, and the condition graph (*cg*).  What
;;; this returns is an alist of the name given by the user, and then
;;; the translation of that name. So, you might get something like: 
;;;   (mycond 1 2 3 4)  ; mycond translates to trials 1, 2, 3, and 4.
;;;   (nocond nocond) ; there is no translation for nocond.
;;; Given conds must be a list, and what you get back is a list of the
;;; above elements.

(defun select-conds (study segment &optional given-conds)
  (mapcar #'check-translation
    (mapcar #'translate-condition 
	    (or given-conds (read-conditions)))))

(defun read-conditions () 
  (format t "Enter a list of conditions. Type ? for the list of possibilities,~%")
  (format t "                              or ! to abort the command!~%")
  (let ((in (mapcar #'read-from-string (parse-string (prompt-for-string ">")))))
    (if (eq '! (car in))
	(abort-command "Aborted!")
        (if (eq '? (car in))
	    (progn (format t "The possible conditions are: ")
		   (dolist (c *cg*) 
                     (format t "~a, " (car c)))
		   (format t "~%")
		   (read-conditions))
	    in
	  )))
  )

;;; All this has to do, I think, is to make the desired alist where the car is
;;; the given name and the cdr the translation.

(defun translate-condition (c)
  (cons c (reduce-condition c)))

;;; This eithers works or aborts.  I'm not sure that the testing is complete.
;;; It might want to also check to see that you end up with all numbers in the
;;; translation.

(defun check-translation (c)
  (if (and (atom (cdr c)) (eq (car c) (cdr c)))
      (abort-command (format nil "Couldn't translate the condition called ~a."
			     (car c)))
      c
    ))

;;; --- Code to enter new links in the condition graph.  This is
;;; called by the condition (cond) command and saves the results back
;;; into the results set's conditions file or, if we're superuser,
;;; into the study's conditions file.  With no args this just prints
;;; the condition graph.

(defun condition-cmd (new-name old-names)
  (if new-name (add-new-condition-link new-name old-names)
    (display-condition-graph)))

(defun add-new-condition-link (new-name old-names)
  (ensure-uniqueness-of-new-name new-name)
  (mapcar #'ensure-existance-of-old-name old-names)
  (push (cons new-name old-names) *cg*)
  (save-condition-graph)
  )

;;; For now, displaying the condition graph just means printing it.

(defun display-condition-graph ()
  (dolist (c *cg*)
    (format t "Trial #~a is condition \"~a\"~%"
	    (second c) (first c))
    ))

;;; A new name can't be the same as an existing new name, or else it's
;;; meant to override it; but check to be sure and then remove the
;;; previous entry.

(defun ensure-uniqueness-of-new-name (new-name)
  (let ((old-value (assoc new-name *cg*)))
    (if old-value
	(progn (format t "The given condition name already exists: ~a~%" old-value)
	       (if (yes-or-no-p "Would you like to replace it with the new entry?")
		   (setq *cg* (remove (assoc new-name *cg*) *cg*))
		 (abort-command "Aborted new condition entry.  Nothing's changed."))
	       ))))
      
;;; When a cg table entry is given, the items linked to should already exists.
;;; If not, we warn and let the user decide whether to add the link anyhow.

(defun ensure-existance-of-old-name (old-name)
  (if (numberp old-name) t ; numbers are automatically passed.  
                           ; (Should really check someday for a legit trial #!)
    (let ((old-value (assoc old-name *cg*)))
      (if old-value t ; if it's there, okay.
	  (progn ; otherwise complain
	    (format t "The condition name ~a isn't in the condition graph.~%" old-name)
	    (if (yes-or-no-p "Is that okay (otherwise the command will abort)?")
		t
	      (abort-command "Nothing changes!")
	      ) ) ) ) ) )

;;; When we save back the user's condition info we have to separate
;;; out the birth condition entries from the cg and then everything 
;;; else is assumed to be private.

(defun extract-user-conditions (&aux (cs (copy-tree *cg*)))
  (dolist (bc *birth-conditions*)
    (setq cs (remove bc cs :test #'equal)))
  cs
  )

;;; Resave the condition graph in the appropriate place.  If we're
;;; superuser we try to put it out into the study's conditions table,
;;; otherwise we put it into the user's fis.conditions table.  <<For
;;; now it only save the user's private form because I haven't
;;; figured out quite how to extract the birth conditions from the
;;; graph as well as the user's>>

(defun save-condition-graph ()
  (if (eq *mode* 'superuser)
      (format t "Wanring!  Changes can't be made to the study fis.conditions table yet.  You'll have to hand edit them.  Sorry.~%")
      (if (yes-or-no-p "Do you want to rewrite the condition graph with this cahnge?")
	  (with-open-file (f (result-set-file "fis.conditions")
			     :direction :output
			     :if-exists :supersede)
			  (print (extract-user-conditions) f))
	)))

;;; This is wrong for anything but a flat cond. graph!!  BUG!

(defun find-trials-matching-cond (cond &aux r)
  (dolist (c *cg* r)
    (if (eq cond (car c))
        (push (cadr c) r))))


