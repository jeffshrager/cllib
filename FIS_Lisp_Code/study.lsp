;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- Describe a study here.  There are also some tools here for
;;; selecting various parts of a study and implementation of commands
;;; for loading, saving, and searching the study database.

;;; --- Study creation.

(defun input-study (&aux bid usestd)
  (setq usestd (yes-or-no-p "Use standard study directories?"))
  (setq *s* 
	(make-study
	 :subject-name (prompt-for-string "Enter the subject name: ")
	 :brain-id (setq bid (prompt-for-string "Enter brain id: "))
	 :brain-dir (get-string-w-default "Enter brain directory "
		      (pfn *topdata-path* bid) usestd)
	 :pfile-dir (get-string-w-default 
		      "Enter pfile directory " "pfiles" usestd)
	 :structural-dir (get-string-w-default
			   "Enter structural directory " "structs" usestd)
	 :segments (get-segments (pfn *topdata-path* bid))
	 :fisversion *fis-version*
	 )))

;;; --- Segment prep

(defvar *trialinfo* ())

(defun get-segments (toppath &aux segments (segnum 0))
  (setq *trialinfo* (get-trialinfo toppath))
  (let ((segnames (get-segment-names)))
    (dolist (segname segnames (reverse segments))
      (push (make-a-segment segname (incf segnum)) segments)
      ))
  )

(defun get-segment-names (&aux names)
  (dolist (tr *trialinfo*)
    (let ((name (second tr)))
      (if (not (member name names))
	  (push name names))))
  names)

;;; The association btwn pfiles and trails/conditions is described in
;;; a file called fis.trials in the brain directory.  Each line has
;;; the name of a pfile followed by the segment into which the pfile
;;; belongs, followed by either the word "map" or the birth condition
;;; name for the trial, followed by any number of atoms and
;;; strings. Each subsequent atom becomes an additional trial
;;; descriptor for the trail, which will eventually go into the
;;; condition graph, and strings are ignored as comments.  Any line
;;; beginning with a ; is ignored.  In the case of EPI, the first item
;;; is a string that indicated the NAME of the epi subdirectory in
;;; brain/EPI that contains the epi images.  (The pfile name may also
;;; be "no-pfile" in which case we just setup the segments and don't do
;;; pfile assignment.)

(defun get-trialinfo (toppath)
  (if (file-exists? (pfn toppath "fis.trials"))
      (gti2 (pfn toppath "fis.trials"))
      (abort-command "There should be an fis.trials file!~%FIS Assumes that you'll set up by hand!"))
  )

(defun gti2 (file)
    (mapcar #'multi-read-from-string (read-lines-from-file file)))

;;; << Needs to be fixed to include EPI. >>

(defun make-a-segment (name number)
  (format t "~%---Segment ~a (~a):~%" name number)
  (setq scan-type (decide-segment-scan-type name))
  (make-segment
   :number number
   :name name
   :dir (format nil "~a" name)
   :scan-type scan-type
   ;; In the case of epi, there is no pfile directory, but this contains
   ;; the location of the EPI images, which will get linked into the 
   ;; functionals directory.
   :pfile-dir (if (eq 'spiral scan-type)
		  "pfiles"
		  (prompt-for-directory "Enter the location of the EPI images: "))
   :inplanes-dir "inplanes"
   :volume-dir "volume"
   :results-dir "results"
   :functional-slices-dir "func"
   :invert-output-dir "out"
   :prescription (get-prescription)
   :trials (get-trials name)
   :condition-graph (make-segment-condition-graph name)
   )
  )

;;; This gets the segment condition graph from the segment info.  To the
;;; extent that such exists.  Each line in the fis.trials file looks like:
;;;   <file> <segmentname> <[map]|birthconditionname> <"string"|cond>*
;;; What we di here is turn the cond parts into pointers to the birth
;;; condition names in the standard condition graph form.  (See cond.lsp)

(defun make-segment-condition-graph (segname &aux cg)
  (let ((trials (get-info-entries-for-trial-no-map segname)))
    (dolist (trial trials)
      (let ((birthname (third trial)))
	(dolist (addcond (cdddr trial))
          (if (not (stringp addcond))
	      (let ((previous (assoc addcond cg)))
		(if previous
		    (pushnew birthname (cdr previous))
		    (push (list addcond birthname) cg))
		)
	    )
	  ))))
  cg)

;;; << Needs to be fixed to include EPI. >>

(defun decide-segment-scan-type (name)
  'spiral)

;;; --- Prescription prep.

(defun get-prescription (&aux fov)
  (make-prescription
   :fov (setq fov (prompt-for-number "Enter field of view" 24))
   :param-file *default-parm-file*
   :slices (prompt-for-number "Enter the number of slices" nil)
   :samples (prompt-for-number "Enter the number of samples per slice" 32)
   :period (prompt-for-number "Enter the period" 8)
   ))

;;; --- Trial prep.

;;; There is a complexity here in that this is general so that
;;; different pfiles could have a different map, but in reality
;;; there's usually one map file per segment, um, or less, actually.
;;; The person who puts together the fis.trials database has to be
;;; sure to REPLICATE MAPS for each segment that needs to use the same
;;; mapfile or else things will get totally confused.  Um, this
;;; demands that there is only one map per segment.... is that right?
;;; We need to rethink the association between maps and pfiles and
;;; segments.  Maps really go with pfiles, not with segments.

;;; << Needs to be fixed to include EPI. >>

(defun get-trials (segname &aux trials (tk 0))
  (let ((mapfile (get-segment-mapfile-info segname))
	(tis (get-info-entries-for-trial-no-map segname)))
    (dolist (trial tis)
      (push (make-trial
	     :number (incf tk)
	     :condition (third trial)
	     :mapfile (car mapfile)
	     :pfile (first trial)
	     )
	    trials)))
  ;; make sure that they go in in the right order (altough others really
  ;; shouldn't depend upon this!)
  (reverse trials))

(defun get-info-entries-for-trial-no-map (name &aux trials)
  (mapcan #'(lambda (entry) (if (and (eq name (second entry))
				     (not (eq 'map (third entry))))
				(list entry)))
	  *trialinfo*))

(defun get-segment-mapfile-info (name)
  (car (mapcan #'(lambda (entry) (if (and (eq name (second entry))
				     (eq 'map (third entry)))
				(list entry)))
	  *trialinfo*)))

;;; --- Study saving and loading utils.

;;; A study is just a streamed copy of the study structure.  In practice,
;;; an expert user may wish to create a .lsp file for the study which
;;; makes all the relevant structures, and just load of through the
;;; lisp loader.  This quick-and-dirty load/store will eventually be
;;; internally called by a fancy study editor.

;;; Saving the study is a bit of a heuristic process.  If the argument
;;; is a string then we use it as a hard pathname.  If it's an atom,
;;; then we assume that the user wants to save it in the default study
;;; directory under this brain, as "fis.study".  In anycase, if there's a
;;; *study-file-path* we check that our guess is congruent with that.  If
;;; there's no argument, then we use that default.

(defun save-study (filename)
  (cond ((null filename)
	 (if *study-file-path*
	     (save-in *study-file-path*)
	     (abort-command "Since you didn't load this study, you have to name it in the save command.~%")))
	((stringp filename)
	 (ss2 filename))
	(t (ss2 (pfn *topdata-path* (string-downcase filename) "fis.study")))
	))

(defun ss2 (name)
  (if *study-file-path* 
      (if (string-equal *study-file-path* name)
	  (save-in name)
	  (progn
	    (format t "The name you gave, ~a, disagrees with the loaded path: ~a~%" 
		    name *study-file-path*)
	    (if (yes-or-no-p "Save it anyway?")
		(progn (save-in name)
		       (setq *study-file-path* name)
		       (format t "Default path reset to ~a.~%" name))
	        (abort-command "Save aborted.~%"))
	    ))
      (if (yes-or-no-p (format nil "Save in ~a?" name))
	  (progn (save-in name)
		 (setq *study-file-path* name)
		 (format t "Default path reset to ~a.~%" name))
	  (abort-command "Save aborted.~%"))
      ))

(defun save-in (filename)
  (if (file-exists? filename)
      (if (yes-or-no-p "~a exists, overwrite it? " filename)
	  (si2 filename))
      (si2 filename)))

;;; Here's where the actual saving work is done.

(defun si2 (filename)
  (format t "Saving in ~a.~%" filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
		  (print *s* stream)))

;;; Load tries to find the named study in a file called "fis.study"
;;; under the study name; If it can't, then it uses the name given as
;;; a local pathname.  Regardless, the global *study-file-path* gets
;;; set so that you can resave it afterwards.

(defun load-study (studyname)
  (setq *loaded-study-name* studyname)
  (cond ((try-loading (pfn *topdata-path* (string-downcase studyname) "fis.study")))
	((try-loading studyname))
	((try-loading (string-downcase studyname)))
	(t (format t "Couldn't find the study file anyplace.~%"))
	))

(defun try-loading (filename)
  (if (file-exists? filename)
      (progn 
	(setq *s* (with-open-file (stream filename :direction :input)
				  (read stream)))
	(format t "Loading from ~a.~%" filename)
	(setq *study-file-path* filename)
	(describe-study *s*)
	;; Also set this hidden global to the first segment, just for debugging.
	(setq seg (car (study-segments *s*)))
	t)
    nil))

;;; Functions for description.

(defun describe-study (&optional (study *s*))
  (format t "--- Study ~a, Subject: ~a, Date: ~a, Operator: ~a~%"
	  (study-brain-id study)
	  (study-subject-name study)
	  (study-date study)
	  (study-mr-operator study))
  (format t "(Created by FIS version ~a.)~%" (study-fisversion study))
  (describe-outboard2 "Study" (pfn (study-brain-dir *s*) "fis.description"))
  (if (superuser?)
      (format t "Brain in ~a, Pfiles in ~a, Stucts in ~a~%"
	      (study-brain-dir study)
	      (study-pfile-dir study)
	      (study-structural-dir study))
    )

  (format t "--- Segments are: ")
  (dolist (segment (study-segments study))
    (format t "~a, " (segment-name segment)))
  (format t "~%")
  
  ;; Set up advice hints.
  (setq *hint* (list 'desc *s*))

  )

(defun describe-result-set ()
  (format t "--- The selected segment is ~a:~%" (result-set-segment-name *r*))
  (describe-outboard2 "Result Set" (result-set-file "fis.description"))
  (let ((p (segment-prescription *g*)))
    (format t "--- Prescription is ~a slices, skip ~a, fov = ~a.~%"
	    (prescription-slices p)
	    (prescription-skip p)
	    (prescription-fov p)
	    ))

  ;; This code to display the condition names isn't needed anylonger because
  ;; the condition graph code overrides it.  There's a complexity here
  ;; wherein superuser might need to see/change the birth conditions,
  ;; but s/he can use the recondition command to do that.
 
  (format t "(Use the 'cond' command to see the condition graph.)~%")
#|(format t "~%Trials:~%")
  (dolist (trial (segment-trials *g*))
    (format t "  Trial ~a is condition ~a~%"
	    (trial-number trial)
	    (trial-condition trial)
	    ))
  |#

  ;; List the status of the results directory so that we can see
  ;; what things have been analyzed.  Also, reconstruction status.

  (format t "--- This set has analyzed results as follows:~%~%")
  (dolist (c (understand-existing-results :use-cache nil))
	  (display-result c))

  (format t "~%~%")
  )

;;; Some tools to edit parts of the study structure.

(defun change-segment-names (&optional (study *s*))
  (dolist (segment (study-segments study))
    (setf (segment-name segment)
	  (read-from-string (prompt-for-string "Enter new name for the segment now called: ~a  " (segment-name segment)))
	  )))

;;; Utils to check whether this is an epi or spiral segment.

(defun epi-segment (segment)
  (eq 'epi (segment-scan-type segment)))

(defun spiral-segment (segment)
  (eq 'spiral (segment-scan-type segment)))

;;; --- Implementation of study database search commands.

;;; To find studies that match a particular word or sequence of words
;;; use grep (-li) to get the names of files in the default directory
;;; whose database files (those beginning with 'fis.') contain the
;;; words.  This works on multiple words through intersection of
;;; repeated greps if you have more than one word.  If no args are
;;; given, report an error.  Problem is that you can't list the
;;; descriptions of all the studies as there are too many of them!

(defvar *find-top-path* ())

(defun find-studies (keylist)
  (if keylist 
      (study-finder keylist *topdata-path* "studies")
    (abort-command "Find needs keyword args to work.  Can't list all the studies.  Sorry.")))

(defun study-finder (keylist top-path type)
  (setq *find-top-path* top-path) ; this is a poor hack instead of propogating it.
  (let ((names (intersect-filenames (mapcar #'collect-grep-hits keylist))))
    (if names
	(report-names names type)
        (abort-command (format nil "No ~a matching the given keys were found.~%" type)))))

(defun report-names (names type)
  (format t "These ~a match on all of the given keywords:~%" type)
  (dolist (n names)
    (format t "~a: ~a~%" n (get-first-line-of-description n))))

(defun get-first-line-of-description (n)
  (let ((fn (pfn *find-top-path* n "fis.description")))
    (if (file-exists? fn)
	(with-open-file (f fn :direction :input) (read-line f nil ""))
        "There is no description file for this study.")))

(defun collect-grep-hits (key)
 ;;; Ignore errors doesn't exist in gcl!!!
; (ignore-errors 
   (system (format nil "grep -sli ~a ~a > ~a"
	    key
	    (pfn *find-top-path* "*/fis.*")
	    (/tmp "fis.keyhits")
	    ))
   (read-lines-from-file (/tmp "fis.keyhits"))
;  )
  )

(defun intersect-filenames (l* &aux carry)
  (setq carry (pop l*))
  (mapcar #'(lambda (l) (setq carry (intersection carry l :test #'string-equal)))
	  l*)
  (remove-duplicates (mapcar #'strip-to-database-name carry) :test #'string-equal))

;;; "/data/foo/fis"... -> "foo"

(defun strip-to-database-name (fullname)
  (second (reverse (parse-string fullname :break-char #\/))))

;;; --- Functions to check and select segments, either by call or interactively.

;;; Select a named or numbered segment.  If none is given, or the indicated
;;; segment doesn't exist,prompt for one.

(defun select-segment (study segment-number &aux thesegment)
  (study-guard)
  (let ((segments (study-segments study)))
    (cond ((and (numberp segment-number)
		(setq thesegment (find segment-number segments 
				    :test #'(lambda (n segment)
					      (= n (segment-number segment))))))
	   thesegment)
	  ((null segment-number)
	   (select-segment-interactively segments study))
	  ((and (atom segment-number)
		(setq thesegment (find segment-number segments 
				    :test #'(lambda (n segment)
					      (eq n (segment-name segment))))))
	   thesegment)
	  (t (select-segment-interactively segments study))
	  )))

(defun select-segment-interactively (segments study)
  (let ((segment-ids (mapcar #'(lambda (segment) (list (segment-number segment)
					      (segment-name segment))) segments)))
    (format t "Select from these segments by name or number: ~a~%" segment-ids)
    (let ((selection (read-from-string (prompt-for-string "Or type NONE to skip it altogether: "))))
      (cond ((eq 'none selection) (abort-command))
	    ((or (numberp selection) (atom selection))
	     (select-segment study selection))
	    (t (break "Huh?"))
	    ))))

;;; If the describe command was given with args, then we print out the
;;; study or result set description file.  The first arg has to be
;;; either study (s study, etc.) or result-set (c context r result,
;;; etc.) and then we do the right thing with the arg.  In the case of 
;;; studies, we have to do some heuristic case-hacking.

(defun describe-outboard (type name)
  (case type
    ;; In case of the study we list both the usual stuff and the segments.
    ((s study st)
          ;; This is heuristic.  If the /data/... is in uppercase, we lose!
     (let ((location (pfn *topdata-path* (string-downcase (symbol-name name)))))
       (describe-outboard2 "Study" (pfn location "fis.description"))
       (list-study-segments location)))
       
    ((r c context result results result-set)
     (describe-outboard2 "Result Set"
			 (pfn *results-top-path* name "fis.description")))
    ((z series)
     (describe-outboard2 "Series" 
			 (pfn *series-top-path* name "fis.description")))
    (t (abort-command "Find must be followed by a type ('study or 'context) and an identifier."))
    ))

(defun describe-outboard2 (heading file)
  (if (file-exists? file)
      (progn
	(format t "--- ~a Description: ~%" heading)
	(system! "cat ~a" file)
	(format t "~%")
	)
    (format t "There is no description file for this ~a.~%" heading)))

;;; This gives us the segments in a study without actually loading it.

(defun list-study-segments (path)
  (let ((fis.study-file (pfn path "fis.study")))
    (if (file-exists? fis.study-file)
        (progn
	  (format t "--- Segments are: ")
	  (dolist (segment (get-study-segments-direct fis.study-file ))
            (format t "~a, " (segment-name segment)))
	  (format t "~%")
	  ))))

(defun get-study-segments-direct (file)
  (with-open-file (f file) (study-segments (read f))))
