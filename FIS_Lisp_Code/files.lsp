;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                      Psychological Software Tools, Inc.
;;;                      Jeff Shrager

;;; --- These are the file-name creators and utils used throughout the system.

;;; Chooses where to get the binaries for the tools from (those that
;;; aren't in our home location, anyhow.  Note that the name of the
;;; tool, if it's given here, needs to be quoted in order to preserve
;;; its case for lus-ix.

(setq *default-bin* "/users/mri/bin")
(setq *test-bin* "/users/mri/alphabin")

(defun bin (&optional tool)
  (let ((bin (if *dbg* *test-bin* *default-bin*)))
    (if tool
	(pfn bin tool)
        bin)))

;;; Need to add more forms to this alist if you're ever gonna have
;;; more than four items.  This ought to be done more generally someday.  Duh!

(defvar *pathformsalist*
  '((1 . "~a") (2 . "~a/~a") (3 . "~a/~a/~a") (4 . "~a/~a/~a/~a")))

(defun path-from-names (&rest names)
  (let ((form (cdr (assoc (length names) *pathformsalist*))))
    (if form 
	(apply #'format (cons nil (cons form names)))
        (break "path-from-names got a bogus argument: ~a" names)
	)))

;;; This is just a shorthand for the above.

(defmacro pfn (&rest args)
  `(path-from-names ,@args))
	   
;;; This checks that the named dir exists and lets the user fix up if
;;; necessary.

(defun prompt-for-directory (fmt &rest args &aux dir)
  (loop
    (setq dir (apply #'prompt-for-string (list fmt args)))
    (if (probe-file dir)
	(progn
	  (format t "~a contains ~a files.~%" dir 
		  (length (understand-directory dir)))
	  (if (yes-or-no-p "Does that seem correct?")
	      (return dir)
	      (format t "Try again.~%")
	      )))
    ))

;;; Simple util to read all the lines out of a named file as strings.

(defun read-lines-from-file (file &aux i r)
  (with-open-file (s file :direction :input)
    (loop (setq i (read-line s nil))
          (if (null i) (return (reverse r)))
	  (push i r))))

;;; Various paths that we're always getting.  These are only partly
;;; installed throughout the code so far, and there is probably a
;;; better set of generalizations.  Note that some of these have an
;;; optional filename; Be aware of places that it could be used instead
;;; of another (caller level) call to pfn.  Also, note that we're pretty
;;; much bound to the *s* version of things at this point!

;;; Also, be aware that some functions, esp. e.g., in study.lsp and
;;; sortfiles.lsp, have to use the logner pfn form because they are
;;; creating the structures that these functions require!

(defun segment-path (segment &optional filename)
  (if filename 
      (pfn (study-brain-dir *s*)
	   (segment-dir segment)
	   filename)
      (pfn (study-brain-dir *s*)
	   (segment-dir segment))))

(defun result-set-file (filename)
  (pfn (result-set-path *r*) filename))

(defun result-dir (&optional filename)
  (results-dir filename))
(defun results-dir (&optional filename)
  (let ((sp (pfn (result-set-path *r*) "results")))
    (if filename (pfn sp filename) sp)))

(defun series-file (filename)
  (pfn (series-path *r*) filename))

(defun series-dir (&optional filename)
  (let ((sp (series-path *z*)))
    (if filename (pfn sp filename) sp)))

(defun inplanes-dir (&optional segment)
  (or segment (setq segment *g*))
  (segment-path segment (segment-inplanes-dir segment)))

(defun volume-dir (&optional segment)
  (or segment (setq segment *g*))
  (segment-path segment (segment-volume-dir segment)))

(defun invert-output-dir (&optional segment)
  (or segment (setq segment *g*))
  (segment-path segment (segment-invert-output-dir segment)))

;;; Log directory processing is a little messy because there are a
;;; number of different logs: brain-level segment, result set, and
;;; series.  At some point maybe these can all be unified, but for the
;;; time being, it's all done in separate fns called when appropriate,
;;; and the decision of which to use is up to the caller.

(defun rlog-dir (&optional filename)
  (if filename
      (pfn (result-set-path *r*) "log" filename)
        (pfn (result-set-path *r*) "log")))

(defun zlog-dir (&optional filename)
  (if filename
      (pfn (series-path *z*) "log" filename)
    (pfn (series-path *z*) "log")))

(defun appropriate-log-dir (&optional filename)
  (if *z* (zlog-dir filename)
    (if *r* (rlog-dir filename)
      (if *g* (slog-dir *g* filename)
	(abort-command "Someone asked for a log file but since there's
nothing loaded, I can't figure out where the log is!")
	))))

;;; log-dir is for the study segment
(defun slog-dir (&optional segment filename)
  (or segment (setq segment *g*))
  (if filename
      (pfn (segment-path segment) "log" filename)
        (pfn (segment-path segment) "log")))

(defun /tmp (name)
   (format nil "/tmp/~a.~a" (system::getenv "LOGNAME") name))

;;; Aux files are used for information that relates to multiple
;;; results series, or studies.  Rather than have a subdir in aux for
;;; every little thing, aux files alway have a type ID (e.g., "pal."
;;; "brain.") and then whatever identifying info is needed.  Of course, to
;;; the /aux fn, these are all just names.

(defun /aux (&rest names)
  (pfn *user-top-path* "aux"
       (dot-together-names names)))

(defun dot-together-names (names &aux r)
  (setq r (pop names))
  (dolist (n names)
    (setq r (format nil "~a.~a" r n)))
  r))

;;;

(defun rlog-dir (&optional filename)
  (if filename
      (pfn (result-set-path *r*) "log" filename)
        (pfn (result-set-path *r*) "log")))

;;; For back compatibility, if there's no palette directory, use
;;; the global palette default.

(defun pal-dir-or-default ()
  (let ((pd (pfn (result-set-path *r*) "pals")))
    (if (file-exists? pd) pd *palette-path*)))

(defun homepath ()
  (let ((p (pathname-directory (user-homedir-pathname))))
    (format nil "/~A" (pfn (second p) (third p)))))

;;; This is a hack, but GCL seems to have no other way to get the pwd.

(defun find-current-directory ()
  (string-right-trim "/" (directory-namestring (car (directory "*")))))

;;; --- Utils to create dirs and links.  These only ask for
;;; confirmation in superuser mode.

(defun check-and-link-in (newfile oldfile &key dont-ask)
  (if (file-exists? newfile)
      (format t "~a already exists.  No linking performed.~%" newfile)
    (if (or (not (superuser?))
	    dont-ask 
	    (yes-or-no-p "~a~%   needs to be linked to ~a.~%   Do it? " 
			 newfile oldfile))
	(system! (format nil "ln -s ~a ~a" oldfile newfile))
      )))

(defun check-and-create-directory (path &optional silent-check?)
  (if (file-exists? path)
      (or silent-check? (format t "Checked existance of ~a...ok.~%" path))
    (if (or (not (superuser?))
	    (yes-or-no-p "Need to make ~a; Okay? " path))
	(system! (format nil "mkdir ~a" path))
      )
     ))

;;; --- Dumps a list of strings to the indicated text file.

(defun dump-strings-to-file (file strings)
  (with-open-file (f file :direction :output :if-exists :supersede)
    (dolist (s strings)
      (format f "~a~%" s))))

;;; --- rnvol duplicates the shell rnvol command, except that it is a
;;; little simpler because we are already in a lisp env.

;;; In the vol world the .mri and .dat files come in pairs, BUT you
;;; CANNOT JUST RENAME THEM TOGETHER as the .mri file contains the
;;; name of the .dat file that it goes with.  This tool looks through
;;; all the .mri files and resets their .dat association so that they
;;; point to the apropriate .dat file.  This is easier than it looks;
;;; all you need do is to make the volume.filename entry be the same
;;; as the .mri file's own name, just assuming that there's a .dat
;;; file to go along with it.... oh, okay, I guess I ought to check
;;; that little detail.

(defun rnvol (dir &optional (verbose nil))
  (if verbose (format t "~%Repairing .mri file .dat pointers in dir \"~a\"~%" dir))
  (dolist (vol.mri (directory (format nil "~a/*.mri" dir)))
    (setq vol.mri (file-namestring vol.mri))
    (if verbose (format t "Trying to repair ~a~%" vol.mri))
     (repair-vol-header dir 
       (subseq vol.mri 0 (- (length vol.mri) 4))
       verbose
     )
    ))

(defun die! (fmt &rest args)
  (apply #'format (cons t (cons fmt args)))
  )
	
(defun repair-vol-header (dir vol verbose)
 (if (not (file-exists? (format nil "~a/~a.mri" dir vol)))
     (die! "Tried to reset a volume header for \"~a\", but the .mri file doesn't appear to exist!~%" vol))
 (if (not (file-exists? (format nil "~a/~a.dat" dir vol)))
     (die! "Tried to reset a volume header for \"~a\", but the .dat file doesn't appear to exist!~%" vol))
 (with-open-file (n "/tmp/volhax" :direction :output :if-exists :supersede)
  (with-open-file (f (format nil "~a/~a.mri" dir vol) :direction :input)
   (prog (line gotit)
     loop
         (setq line (read-line f nil nil))
	 (if (null line) 
	     (return 
	      (if gotit 
		  (if verbose (format t "~a.mri volume.filename has been reset.~%" vol))
		  (format t "Didn't find a volume.filename in ~a.mri.~%" vol)
		  )))
	 (if (and (> (length line) 20)
		  (string-equal "volume.filename = "
				(subseq line 0 18))
		  )
	     (progn (setq gotit t)
		    (format n "volume.filename = ~a.dat~%" vol))
	   (format n "~a~%" line)
	   )
	 
	 (go loop)
	 )
   )) ; with-open-file(2)
 (system! "mv /tmp/volhax ~a/~a.mri" dir vol)
 )

;;; At startup this fn is called to check and create each of the main
;;; directories required by fis.

(defun ensure-user-fis-dirs ()
  (check-and-create-directory *user-top-path* t)
  (check-and-create-directory (pfn *user-top-path* "results") t)
  (check-and-create-directory (pfn *user-top-path* "series") t)
  (check-and-create-directory (pfn *user-top-path* "aux") t)
  )

;;; Unix cares about case but we don't, therefore, we need a way to
;;; equate caseless with cased names.  Usually this is only important
;;; for filenames, so this looks for the file that matches a given
;;; name in the directory and returns that name.  This just returns the
;;; first match it finds.

(defun case-blind-find-file (path filename)
  (let ((f* (directory path))
	(filestring (symbol-name filename)))
    (dolist (f f* nil)
      (if (string-equal (string-downcase (pathname-name f))
			(string-downcase filename))
	  (return (pathname-name f))))))