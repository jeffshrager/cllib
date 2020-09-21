#+(version>= 5.0)
(in-package :afferent)
;;; -*- Mode: Lisp -*-

;;; Boring lisp utilities for Afferent.

;;; Copyright (C) 1996, 1997 Afferent Systems, Inc.  All Rights Reserved.

;;; Started 2/4/96 David Chapman.



;;; Compress is like APL compress.  The map is a list of T/NIL
;;; indicating where elements of the target list should be
;;; taken/ignored.  If we run out of EITHER list or map, the tail of
;;; the list is lost. ??? Maybe this isn't right?  Should it error
;;; out?

(defun compress (list map)
  (loop for element in list 
      as key in map
      when key
      collect element))

;;; This was originally called 1/0-boolean but the GCL parser tries to read 1/0 as a rational and blows out.
(declaim (inline 1-0-boolean))
(defun 1-0-boolean (t-nil-boolean) (if t-nil-boolean 1 0))

;;; I would have called this parse-integer-barf...
(defun parse-integer! (string &optional message)
  (or (parse-integer string :junk-allowed t)
      (barf "Bad integer"
	    (if message 
		(format nil "Got \"~a\" where an integer was expected.  ~a" string message)
		(format nil "Got \"~a\" where an integer was expected." string)
		)
	    )
      ))

;;; Like getf, except better.
(defun getv (symbol plist)
  ; (declaim (values value found?))
  (loop with tail = plist
	until (null tail)
	do
	(when (eq symbol (first tail))
	  (return (values (second tail) t)))
	(setq tail (cddr tail))
	finally
	(return (values nil nil))))

(defun circular-list (item)
  (let ((list (list item)))
    (setf (cdr list) list)
    list))

;;; %%% Could avoid cdring down the list twice
(defun delete-nth (index list)
  (if (zerop index)
      (cdr list)
      (progn (rplacd (nthcdr (1- index) list)
		     (nthcdr (1+ index) list))
	     list)))

; destructive
(defun insert-before (list new before)
  (if (null before)
      (nconc list (list new))
      (do ((rest list (cdr rest))
	   (last nil rest))
	  ((null rest)
	   (error "insert-before: ~A not in ~A" before list))
	(if (eq before (car rest))
	    (if last
		(progn (rplacd last (cons new rest))
		       (return list))
	      (return (cons new list)))))))

(defun insert-after (list new after)
  (if (null after)
      (cons new list)
      (do ((rest list (cdr rest)))
	  ((null rest)
	   (error "insert-after: ~A not in ~A" after list))
	(if (eq after (car rest))
	    (progn (rplacd rest (cons new (cdr rest)))
		   (return list))))))

;;; why isn't this in CL?
(defun insert-at (list new position)
  (let ((new-tail (cons new (nthcdr position list))))
    (if (= position 0)
	new-tail
      (progn (rplacd (nthcdr (- position 1) list) new-tail)
	     list))))

(defun delete-property (property plist)
  ;; (declare (values new-plist old-value foundp))
  (cond ((eq (first plist) property)
	 (values (cddr plist) (second plist) t))
	(t
	 (loop with previous = nil
	       for sublist on plist
	       for thing = (first sublist)
	       do
	       (when (eq thing property)
		 ;; bash plist to delete :RETURN and type
		 (setf (cdr previous) (cddr sublist))
		 (return (values plist (second sublist) t)))
	       (setq previous sublist)
	       finally
	       (return (values plist nil nil))))))

;;; Internal function for PLIST-EQUAL
(declaim (inline plist-equal1))
(defun plist-equal1 (plist1 plist2)
  (loop with tail = plist1
	until (null tail)
	always (equal (getf plist2 (first tail)) (second tail))
	do (setq tail (cddr tail))))

;;; This mods out two issues: the difference between an explicit NIL value and no value,
;;; and the order of the items in the plists.
(defun plist-equal (plist1 plist2)
  (and (plist-equal1 plist1 plist2)
       (plist-equal1 plist2 plist1)))

(declaim (inline plist-equal1))
(defun plist-equal1-null-string-matches-nil (plist1 plist2)
  (loop with tail = plist1
	until (null tail)
	always (equal-or-null-string-matches-nil (getf plist2 (first tail)) (second tail))
	do (setq tail (cddr tail))))

;;; Splits a list into a list of lists where the sublists are at most n elements long
(defun divide-list! (l n)
  (let* ((breakpoint (nthcdr (1- n) l))
	 (tail (cdr breakpoint))
	 )
    (if tail
	(progn
	  (setf (cdr breakpoint) nil)
	  (cons l (divide-list! tail n)))
      (list l))))

(declaim (inline equal-or-null-string-matches-nil))
(defun equal-or-null-string-matches-nil (thing1 thing2)
  (or (and (null thing1) (stringp thing2) (zerop (length thing2)))
      (and (null thing2) (stringp thing1) (zerop (length thing1)))
      (equal thing1 thing2)))

;;; This is like PLIST-EQUAL, except that it allows "" to match NIL.
(defun plist-equal-null-string-matches-nil (plist1 plist2)
  (and (plist-equal1-null-string-matches-nil plist1 plist2)
       (plist-equal1-null-string-matches-nil plist2 plist1)))

;;; Why isn't this in CL??
;;; %%% Maybe optimize the case of test=EQ as a special case, to avoid funcall overhead
(defun bag-difference (b1 b2 &key (test #'eq))
  (let ((copy (copy-list b1)))
    (loop for thing in b2
	  do
	  (cond ((funcall test thing (first copy))
		 (pop copy))
		(t
		 (block inner
		   (loop for sublist on copy
			 when (funcall test thing (second sublist))
			 do
			 (setf (cdr sublist) (cddr sublist))
			 (return-from inner))))))
    copy))

(declaim (inline subbagp))
(defun subbagp (b1 b2 &key (test #'eq))
  (and (<= (length b1) (length b2))				 ; try to fail quickly
       (null (bag-difference b1 b2 :test test))))

(declaim (inline bag-equal))
(defun bag-equal (b1 b2 &key (test #'eq))
  (and (= (length b1) (length b2))
       (null (bag-difference b1 b2 :test test))
       (null (bag-difference b2 b1 :test test))))

;;; Why isn't this in CL??
(defun set-equal (s1 s2 &key (test #'eq))
  (and (= (length s1) (length s2))				 ; %%% Does subsetp do this check?
       ;; %%% There may be a faster algorithm -- e.g. by sorting or something
       (and (subsetp s1 s2 :test test) (subsetp s2 s1 :test test))))

;;; I thought something like this was in CL, but I can't find it.
(defun nsublis-sequence (alist sequence)
  (loop for i from 0 below (length sequence)
	do
	(loop for (old . new) in alist
	      when (eql (elt sequence i) old)
	      do
	      (setf (elt sequence i) new)
	      (return)))
  sequence)	

#+acl3.0 
(defun ignore (&rest ignore)
  ignore
  :ignored)
(defvar ignore) ;;; binding this and not using it doesn't cause a warning, because it's special.
(defvar ignore1)
(defvar ignore2)
(defvar ignore3)
(defvar ignore4)

;;; Pass this in to tcl for ignored commands
(defun ignore-cmd (&rest ignore)
  (declare (ignore ignore))
  :ignore)

;;; String completion, for completing reader
;;;
;;; First value is the longest common completion of STRING in STRINGs.
;;; Second value is boolean: is this a complete completion?
;;; 
;;; NNN Case insensitive by design.  Might be wrong thing and/or want this to be an option.
(defun complete (string strings)
  (loop with length = (length string)
	with match = nil
	with match-length
	with complete? = nil
	for other in strings
	for other-length = (length other)
	;; :END has bogus behavior wrt lengths, so we need this stupid extra test
	when (and (>= other-length length) (string-equal string other :end2 length))
	do
	(cond (match
	       ;; find the common prefix of the possible completions
	       (setq match (setq match (subseq other
					       0
					       (let ((min (min other-length match-length)))
						 (loop for i from length to (1- min)
						       when (not (eql (aref match i) (aref other i)))
						       do
						       (setq complete? nil)
						       (return i)
						       finally
						       (return min)))))))
	      (t 
	       (setq match other
		     complete? t)))
	(setq match-length (length match))
	finally
	(return (values match complete?))))

;;; Tiresome utility, required only because the GCL format implementation is not up to spec.
(defun zero-pad-integer (integer places)
  (let* ((string (princ-to-string integer))
	 (zeros (- places (length string))))
    (when (minusp zeros)
      (error "Can't pad ~d to ~d places, it's already too long!"
	     integer places))
    ;; %%% This could probably be LOTS more efficient!
    (with-output-to-string (stream)
      (dotimes (ignore zeros) (format stream "0"))
      (princ string stream))))

(declaim (inline coerce-string-to-integer))
(defun coerce-string-to-integer (thing)
  (etypecase thing
    (integer thing)
    (string (parse-integer thing))))

(defun printhash (hashtable)
  (maphash #'(lambda (key value) (format t "~&~s: ~s" key value)) hashtable))

(defun copy-hash-table (old)
  (let ((new (make-hash-table :test (hash-table-test old))))
    (loop for key being the hash-keys of old
	  using (hash-value value)
	  do
	  (setf (gethash key new) value))
    new))

(defun mexp ()
  (loop (format t "~%-> ")
	(let ((form (read))
	      next)
	  (when (member form '(nil :q :r)) (return))
	  (loop (setq next (macroexpand-1 form))
		(when (equal next form) (return))
		(pprint next)
		(setq form next)))))

;;;  Ensure that a macro variable is only expanded once.
(defmacro once-only (vars &body body)
  (let ((gensym-var (gensym))
        (run-time-vars (gensym))
        (run-time-vals (gensym))
        (expand-time-val-forms ()))
    (dolist (var vars)
      (push `(if (or (symbolp ,var)
                     (numberp ,var)
                     (and (listp ,var)
			  (member (car ,var) '(quote function))))
                 ,var
                 (let ((,gensym-var (gensym)))
                   (push ,gensym-var ,run-time-vars)
                   (push ,var ,run-time-vals)
                   ,gensym-var))
            expand-time-val-forms))    
    `(let* (,run-time-vars
            ,run-time-vals
            (wrapped-body
              ((lambda ,vars . ,body) . ,(reverse expand-time-val-forms))))
       `((lambda ,(nreverse ,run-time-vars)  ,wrapped-body)
         . ,(nreverse ,run-time-vals)))))

(defun print-delimited-list (list delimiter &optional (stream t) (item-format-string-or-function "~a"))
  ;; %%% Probably hideously inefficient
  (loop for sublist on list
	do
	(etypecase item-format-string-or-function
	  (string (format stream item-format-string-or-function (first sublist)))
	  (function (funcall item-format-string-or-function (first sublist) stream)))
	(unless (null (rest sublist))
	  (princ delimiter stream))))

(defun print-delimited-list-to-string (list delimiter &optional (item-format-string-or-function "~a"))
  ;; %%% Probably hideously inefficient
  (with-output-to-string (stream)
    (print-delimited-list list delimiter stream item-format-string-or-function)))

;;; NULLS-TOO? means that *every* delimiter instance separates two components, even if there is nothing
;;; between them.  Otherwise, null strings are ignored. Conversion function specifies a function
;;; to be applied to the string before it is pushed onto the result list.
(defun parse-delimited-string (string delimiters &key (nulls-too? t) (conversion-function nil))
  (let ((result '())
	(start (if nulls-too? 0 nil))
	(pointer 0)
	(length (length string)))
    (loop (when (= pointer length)
	    (when start
	      (push (if (= start 0)
			(if conversion-function
			    ;; optimize SUBSEQ call out of this case, which happens to be important
			    ;; for PARSE-PRECURSORS-SPEC.
			    (funcall conversion-function string)
			    string)
			(if conversion-function
			    (funcall conversion-function (subseq string start pointer))
			    (subseq string start pointer)))
		    result))
	    (return (nreverse result)))
	  (cond ((member (aref string pointer) delimiters)
		 (when start
		   (if (not (null conversion-function))
		       (push (funcall conversion-function (subseq string start pointer)) result)
		       (push (subseq string start pointer) result)))
		 (setq start (if nulls-too? (1+ pointer) nil)))
		(t
		 (unless start
		   (setq start pointer))))
	  (incf pointer))))

(declaim (inline whitespace-parse-list))
(defun whitespace-parse-list (string)
  ;; We *don't* want a result component for every space!  Ergo the NIL argument here.
  (parse-delimited-string string '(#\space #\tab #\newline #\return) :nulls-too? nil))

(declaim (inline whitespace-parse-numbers-list))
(defun whitespace-parse-numbers-list (string)
  ;; We *don't* want a result component for every space!  Ergo the NIL argument here.
  (parse-delimited-string string '(#\space #\tab #\newline #\return) 
			  :nulls-too? nil
			  :conversion-function #'read-from-string))

(declaim (inline whitespace-parse-numbers-list))
(defun whitespace-parse-integers-list (string)
  ;; We *don't* want a result component for every space!  Ergo the NIL argument here.
  (parse-delimited-string string '(#\space #\tab #\newline #\return) 
			  :nulls-too? nil
			  :conversion-function #'parse-integer))

;;; Split a string up into lines.
(declaim (inline newline-explode-string))
(defun newline-explode-string (string)
  (parse-delimited-string string '(#\newline)))

;;; For sorted lists of names, which don't want to use string-lessp, because it thinks 
;;; "10" is before "2"; puts acid2 before acid10, and so on.  More generally: divides
;;; names into a trailing digit string and a "stringy part" which is everything up 
;;; to that; string-lessp compares stringy parts, and considers numeric parts only if
;;; those are equal.
;;; 
;;; Allows integers as args (as well as strings).  The sleep and cheezy 
;;; way of doing that would be to turn numeric args into strings, but that would cons,
;;; so we don't.
(defun name< (name1 name2)
  (let* ((stringp1 (stringp name1))
	 (last-non-digit1 (and stringp1 (position-if-not #'digit-char-p name1 :from-end t)))
	 (last-non-digit1+1 (and last-non-digit1 (1+ last-non-digit1)))
	 (stringp2 (stringp name2))
	 (last-non-digit2 (and stringp2 (position-if-not #'digit-char-p name2 :from-end t)))
	 (last-non-digit2+1 (and last-non-digit2 (1+ last-non-digit2))))
    (cond ((and (not last-non-digit1+1) last-non-digit2+1)
	   ;; numbers (and strings of digits) come before anything with non-digits
	   t)
	  ((and last-non-digit1+1 (not last-non-digit2+1))
	   nil)
	  ((not last-non-digit1+1)
	   ;; in this case we know both are numbers or digit strings 
	   ;; or blank. The :junk-allowed and -1 stuff handles the case of blank strings
	   (when stringp1 (setq name1 (or (parse-integer name1 :junk-allowed t) -1)))
	   (when stringp2 (setq	name2 (or (parse-integer name2 :junk-allowed t) -1)))
	   (< name1 name2))
	  ;; In the remaining cases, both guys have stringy parts (and therefore
	  ;; neither could have been passed in as a number).
	  ((string-equal name1 name2 :end1 last-non-digit1+1 :end2 last-non-digit2+1)
	   ;; stringy parts are same, compare numeric parts
	   (let ((number1 (parse-integer name1 :start last-non-digit1+1 :junk-allowed t))
		 (number2 (parse-integer name2 :start last-non-digit2+1 :junk-allowed t)))
	     (cond ((and (null number1) (null number2))
		    nil)		; => (STRING= NAME1 NAME2)
		   ((null number1)
		    t)
		   ((null number2)
		    nil)
		   (t
		    (< number1 number2)))))
	  (t
	   ;; different stringy parts; that takes precedence over numeric part
	   (string-lessp name1 name2 :end1 last-non-digit1+1 :end2 last-non-digit2+1)))))

;;; %%% Plausibly there should be a destructive version of this.
(defun name-sort (list)
  (sort (copy-list list) #'name<))

;;; Call on a list of symbols that are names for something.
(defun name-sort-symbols (list)
  (sort (copy-list list)
	#'(lambda (a b)
	    (name< (~a a) (~a b)))))

(defun name-sort-objects (list)
  (sort (copy-list list) #'(lambda (o1 o2) (name< (name o1) (name o2)))))

;;; Produce slashed name for an object 
(defun object->cname (object)
  (if (eq object *top-folder*)
      ""
    (string-append (object->cname (folder object)) "\\" (name object))))

;;; Convert slashed name into object
(defun cname->object (string &optional (from *top-folder*))
  (if (char= #\\ (char string 0))
      (setq string (subseq string 1)	;lets "absolute" pathnames be used..
	    from *top-folder*))
  (let* ((slashpos (position #\\ string))
	 (nextname (subseq string 0 slashpos))
	 (nextthing (folder-child from nextname)))
    (if (null slashpos)
	nextthing
      (cname->object (subseq string (+ 1 slashpos))
		     nextthing))))


;;; Time and date hacking.

;;; VERY IMPORTANT: Be sure that anything you do in here is Y2K compliant!
;;;
;;; Afferent can get in very serious legal trouble if we aren't.

;;; Boy, it's such a shame that files are so expensive, or the following 16 lines
;;; could be in a separate file. Maybe after the IPO...

;;; Franz-supplied patch for bug in this:
#+acl3.0
(in-package :acl) 
#+acl3.0
(defun encode-universal-time (sec min hour date month year &optional zone)
  (setq year (check-year year) month (check-arg month 1 13 "Month")
	date (check-arg date 1 (i1+ (days-in-month month year)) "Date")
	hour (check-arg hour 0 24 "Hours") min (check-arg min 0 60 "Minutes")
	sec (check-arg sec 0 60 "Seconds"))
  (let ((ut (encode-universal-time-aux sec min hour date month year)))
    (+ ut
       (* 3600 
	  (if (null zone)
	      (multiple-value-bind (zone d-s-t-p)
		  (get-time-zone-info)
		(- zone (or d-s-t-p 0)))
	      (check-arg zone -12 13 "Time-zone"))))))
#+acl3.0
(in-package :user)

;;; Timestamp less about 98 years (to reduce the number to an 8-digit
;;; int for databases -- specificially Oracle) who insist upon
;;; limiting integers to 9 digits!  WWW In the year 2029 this will
;;; roll over 1E9 and Oracle users, if Oracle is still in business at
;;; that time (god forbid!) will once again be screwed!  Doesn't
;;; matter, because I'll be dead, or at least retired... and I'll be
;;; laughing all the way to hell!  Nya Nya Nya Nya Nya!  WWW If you
;;; change this number, it has to be even to exactly one year so that
;;; time calculations come out right!
;;;
(defconstant *afferent-timestamp-offset* 3090528000)
(defun afferent-timestamp ()
  (- (get-universal-time) *afferent-timestamp-offset*))

(defun month-string (month-number)
  (ecase month-number
    (1 "Jan")
    (2 "Feb")
    (3 "Mar")
    (4 "Apr")
    (5 "May")
    (6 "Jun")
    (7 "Jul")
    (8 "Aug")
    (9 "Sep")
    (10 "Oct")
    (11 "Nov")
    (12 "Dec")))

;;; This is somewhat idiosyncratic; supports just the functionality wanted for the synthesis manager.
;;;
;;; Could add hair for cuteness.
;;;
;;; Boy, the way we tell time is illogical.
(defun time-format (universal-time stream)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time universal-time)
    ;; now
    (multiple-value-bind (nsecond nminute nhour ndate nmonth nyear)
	(decode-universal-time (get-universal-time))
      second nsecond nminute nhour	; bnu
      (let* ((mod-12-hour (mod hour 12))
	     (12-normalized-hour (if (zerop mod-12-hour) 12 mod-12-hour))
	     (day-string (cond ((and (= date ndate) (= month nmonth) (= year nyear))
				;; today, implicitly
				"")
			       ((and (= date (1+ ndate)) (= month nmonth) (= year nyear))
				"tomorrow")
			       ((and (= date (1- ndate)) (= month nmonth) (= year nyear))
				"yesterday")
			       (t
				;; FFF Do Euro dates, or, better print month name
				(format nil "on ~d ~a ~d" date (month-string month) year)))))
	(format stream "~d:~a ~a ~a"
		12-normalized-hour
		(zero-pad-integer minute 2)
		(12-hour-suffix hour minute)
		day-string)))))

(defun 12-hour-suffix (hour minute)
  (cond ((= hour 0) (if (= minute 0) "midnight" "am"))
	((< hour 12) "am")
	((= hour 12) (if (= minute 0) "noon" "pm"))
	(t "pm")))

;;; Returns integer, in seconds (ergo is compatible with GET-UNIVERSAL-TIME).
;;; ERRORP can be T or a continuation, which is applied to the error arguments.
;;; NULL-STRING-CONTINUATION is called if non-null and input is null mod whitespace.
(defun parse-duration (string &optional (errorp t) null-string-continuation)
  (let* ((trimmed (trim-whitespace string))
	 (number-end (or (position-if-not #'(lambda (char) (or (digit-char-p char) (eql char #\.)))
					  trimmed)
			 (length trimmed))))
    (macrolet ((do-error (&rest error-arguments)
		 `(cond ((eq errorp t)
			 (error ,@error-arguments))
		   (errorp
		    (funcall errorp ,@error-arguments))
		   (t
		    nil))))
      (cond ((and (string= trimmed "") null-string-continuation)
	     (funcall null-string-continuation))
	    (t
	     (let* ((units-string (trim-whitespace (subseq trimmed number-end)))
		    (units (selector units-string string=
				     (("s" "sec" "secs" "second" "seconds") 1)
				     (("m" "min" "mins" "minute" "minutes") 60)
				     (("h" "hour" "hours") 3600)
				     (("d" "day" "days") #.(* 3600 24))
				     (("w" "wk" "wks" "week" "weeks") #.(* 3600 24 7))
				     ;; for the benefit of duration string "0"
				     ("" 0)
				     ;; don't need to bother with larger units; this is for reaction durations only
				     (otherwise
				      (do-error "I can't parse ~s as a duration: invalid time units ~s."
				        string units-string))))
		    (number (read-from-string (subseq trimmed 0 number-end) nil :eof)))
	       (cond ((not (numberp number))
		      (do-error "Can't parse duration ~s: doesn't start with a number" string))
		     ((null units)
		      ;; errorp must be null to be here
		      nil)
		     ((zerop units)
		      (if (zerop number)
			  ;; "0" is legal as a unitless duration string
			  0
			  (do-error "Can't parse duration ~s: no units supplied" string)))
		     (t
		      ;; round to nearest second
		      (round (* number units))))))))))

(defun get-time (&optional (utime (get-universal-time)))
  (multiple-value-bind
    (second minute hour date month year dow dstp tz)
    (decode-universal-time (round utime))
    (list :second second
	  :minute minute
	  :hour hour
	  :date date
	  :month month
	  :year year
	  :dow dow
	  :dstp dstp
	  :tz tz)
    ))

(defun time-as-int (&optional (utime (get-universal-time)))
  (let ((time (get-time utime)))
  (+ (* (getf time :hour) 3600)
     (getf time :second)
     (* 60 (getf time :minute)))))

(defun pretty-time (&optional time-as-int)
  (or time-as-int (setq time-as-int (time-as-int)))
  (let* ((r (mod time-as-int 3600))
	 (h (truncate (/ time-as-int 3600)))
	 (s (mod r 60))
	 (m (truncate (/ r 60))))
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))
    
(defun date-as-int (&optional (utime (get-universal-time)))
  (let ((time (get-time utime)))
    (read-from-string
     (format nil "~4d~2,'0d~2,'0d" 
	         (getf time :year)
		 (getf time :month)
		 (getf time :date))
     )))

(defun pretty-timestamp (&optional (utime (get-universal-time)))
  (format nil "~a ~a" (pretty-date utime) (pretty-time (time-as-int utime))))

(defun short-printable-time (&optional time-as-int)
  (or time-as-int (setq time-as-int (time-as-int)))
  (let* ((r (mod time-as-int 3600))
	 (h (truncate (/ time-as-int 3600)))
	 (s (mod r 60))
	 (m (truncate (/ r 60))))
    (format nil "~2,'0d~2,'0d~2,'0d" h m s)))
    
;;; Uploaded libraries from 2.1 contained "Unknown..."  in their date
;;; fields, so that if you tried to export the products from the
;;; library, this would blow up.  Now it passes everything that's not
;;; a number through as is.  This shouldn't be a problem for any other
;;; calls (since they'd have blown up before :)

(defun pretty-date (&optional (utime (get-universal-time)))
  (cond ((numberp utime)
	 ;; Convert timestamps to utimes, semi-heuristically (assumes we don't have any dates early in the 20th
	 ;; century, which seems fine...)
	 (when (and utime (< utime *afferent-timestamp-offset*))
	   (setq utime (+ utime *afferent-timestamp-offset*)))
	 (let ((time (get-time utime)))
	   ;; international format -- do NOT want mm/dd/yy
	   ;; FFF IDBS wants us to check the Windows configuration settings for date format.
	   (format nil "~a ~a ~a"
		   (getf time :date)
		   (month-string (getf time :month))
		   ;; no y2k here!  This is 4 digits.
		   (getf time :year)
		   )))
	(t utime)))


;;; for internal releases it's nice to know when your Afferent was built.
;;; this can/should be NIL for real releases.
(defparameter *pretty-build-time* (pretty-timestamp))

;;; Gives us the number of days between two universal times.
(defun utime-delta-days (utime1 utime2)
  (truncate (/ (abs (- utime1 utime2)) 86400.0)))



;;; ACL/Win has a bug in DELETE-FILE, that it is a no-op on files you don't have write access to.  Ugh.
;;; Logically this redefinition belongs in utils.lsp, but since it calls BARF, which is a macro,
;;; it has to come after tkutils.lsp.
;;;
;;; ???  Maybe this is fixed in ACL 5.0?
#+acl3.0
(eval-when (compile load eval)
  (shadow "DELETE-FILE"))
#+acl3.0
(defun delete-file (file)
  (cl::delete-file file)
  (when (probe-file file)
    (barf "Can't delete"
	  "Can't delete the file ~a; maybe it is in use, or Read-Only, or you don't have write access?"
	  file)))



;;; %%% Made unnecessarily slow by CL braindamage.  
(defun copy-file-to-stream (filename out-stream)
  (with-open-file (in-stream filename)
    (let (line)
      (loop (setq line (read-line in-stream nil :eof))
	    (when (eq line :eof) (return-from copy-file-to-stream :done))
	    (format out-stream "~a~%" line)))))



(defun forgiving-read (stream)
  (report-and-ignore-errors (read stream)))



(defun exit-lisp ()
  (quit :emergency-stop))



;;; License utilities; in this file for image file obscurification. 
;;; See note in license.lsp (which is not part of the loaded system.)

;;; This is the key version number.
(defparameter *current-alkane-format* 6)

(defparameter *alkane-format*		; license header format (not counting seed & CRC)
  '((10 numeric version)
    (31 numeric day)
    (12 numeric month)
    (10 numeric year)
    ;; just three chars of each of these
    (3 string licensee)
    (3 string site)
    (63 numeric nmodules)
    ;; remainder of the data is module license info
    ))

;;; Strings have to contain only these chars, and can thus be
;;; compressed into just 6 bits.  
(defparameter *alkanechars* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ /.,-#_=+()@$&*:")

(defun alkane-expiration-date (alkane)
  (let ((day (extract-alkane-slot-value alkane 'day))
	(month (extract-alkane-slot-value alkane 'month))
	(year (extract-alkane-slot-value alkane 'year)))
    (unless (and (<= day 31)
		 (<= month 12))
      (aromatic-barf "Incorrect license key"
		     "Your license key doesn't seem to be in the right format."))
    (encode-universal-time 0 0 12 day month (+ 1997 year))))

(defun find-alkane-position-and-length (slot-name)
  (loop for slot in *alkane-format*
	with pos = 0
	until (equal slot-name (third slot))
	do (incf pos (ecase (second slot)
		       (numeric (1+ (truncate (log (first slot)) (log 2))))
		       (string (* 6 (first slot)))))
	finally (return (cons pos
			      (ecase (second slot)
				(numeric (1+ (truncate (log (first slot)) (log 2))))
				(string (* 6 (first slot))))))))

(defun extract-alkane-slot-value (alkane slot-name)
  (setq alkane (string-upcase alkane))
  (let* ((allbits (deacidify-alkane-to-allbits alkane))
	 (slot-info (find slot-name *alkane-format*
			  :test #'(lambda (name entry) (equal name (third entry)))))
	 (p/n (find-alkane-position-and-length slot-name))
	 (pos (car p/n))
	 (n (cdr p/n))
	 (bits (subseq allbits pos (+ pos n)))
	 )
    (case (second slot-info)
      (string (6char-encode bits))
      (numeric (bits-to-number bits)))))

(defun bits-to-number (bits &aux (n 0))
  ;; Causes: Dropped a byte?
  (if (member nil bits)
      (aromatic-barf "Incorrect license key" "Your license key is incorrect."))
  (loop for i in bits
	do (setq n (+ (* n 2) i))
	finally (return n)))

(defvar *aromatic-barf-p* t)

(defun aromatic-barf (title msg)
  (cond ((eq t *aromatic-barf-p*) ; used by license check code to pass results back, so have to check t explicitly
	 (inform-dialog title msg)
	 (throw :aromatic-error nil))
	(t
	 (format t "Aromatic error: ~a~%" msg)
	 (setq *aromatic-barf-p* msg))
	))

;;; UUU This should be 6char-enacidify or something
(defun 6char-encode (&rest bitsets)
  (let* ((allbits (mapcan #'(lambda (i) i) bitsets))
	 (allchars
	  (loop until (null allbits)
		collect
		(let ((6bits (loop for i from 1 to 6
				   collect (if allbits (pop allbits) 0))))
		  (6bits-to-char 6bits)
		  ))
	   ))
    (coerce allchars 'string)))
  
;;; Various fns for grabbing infor from the universal date in the varient.

(defun alkane-exp-day (variant)
  (nth 3 (decode-universal-time-to-list (variant-expiration-date variant))))
(defun alkane-exp-month (variant)
  (nth 4 (decode-universal-time-to-list (variant-expiration-date variant))))
(defun alkane-exp-year (variant)
  (nth 5 (decode-universal-time-to-list (variant-expiration-date variant))))
  
;;; Protects for nil universal times, returning a very large future date in this case.
;;; 2004 is sacred, based on the number of bits used to encode years.
(defconstant *distant-future* '(0 0 12 31 12 2004))
(defun decode-universal-time-to-list (utime)
  (cond (utime
	 (multiple-value-bind (second minute hour day month year)
	     (decode-universal-time utime 0) ; UUU 0 to deal with ACL timezone lossage
	   (list second minute hour day month year)))
	(t
	 *distant-future*)))

(defun xorbits (b1* b2*)
  (loop for b1 in b1*
	for b2 in b2*
	collect (logxor b1 b2)))

(defparameter *5bit-chars* "23456789-ABCDEFGHJKMNPQRSTUVWXYZ")

(defun 5bits-to-char (bits &aux (n 0))
  (loop for i in bits
	do (setq n (+ (* n 2) i))
	finally (return (aref *5bit-chars* n))))

(defun 6bits-to-char (bits &aux (n 0))
  (loop for i in bits
	with kclen = (length *alkanechars*)
	do 
	(setq n (+ (* n 2) i))
	;; Causes: Messed up the first char (encryption code).
	(when (> n kclen)
	  (aromatic-barf "Incorrect license key" "The license key is incorrect."))
	finally (return (aref *alkanechars* n))))

(defun char-to-5bits (char)
  (bitify 31 (position char *5bit-chars* :test #'equal)))

;;; The hairful decrypter.
(defun deacidify-alkane-to-allbits (whole-alkane)
  (when (< (length whole-alkane) 2)
    (aromatic-barf "Incorrect license key" "Your license key is invalid."))
  (let* ((crc (char-to-5bits (aref whole-alkane 0)))
	 (seed (bits-to-number (char-to-5bits (aref whole-alkane 1))))
	 (randomizer (make-randomizer seed))
	 (alkane (subseq whole-alkane 2))
	 (newcrc (list 0 0 0 0 0))
	 (result 
	  (loop for char across alkane
		for map = (bitify 31 (random-integer randomizer 31))
		do (setq newcrc (xorbits newcrc (xorbits map (char-to-5bits char))))
		append (xorbits map (char-to-5bits char))))
	 )
    (when (not (equal crc newcrc))
      (aromatic-barf "Incorrect license key" "You have an incorrect license key."))
    result))

;;; Bitification gives us back a bit string corresponding to the item.
;;; Numbers have to provide a MAX, which is rounded up to the next
;;; highest log2.  Strings get six bits per char.  The whole thing is
;;; left justified, that is, when we drop off the end, the trailing
;;; bits get put into the high order part of the last char.

;;; Turn things into bits.  The type is either the maximum value, or a
;;; type (e.g., 'string)

(defun bitify (type item)
  (cond ((numberp type)
	 (bitify-number type item))
	((eq 'string type)
	 (bitify-string item))
	))

(defun bitify-number (max n)
  (unless n
    (aromatic-barf "Incorrect license key" "Some part of the license key is incorrect."))
  (reverse ; some loop thingy can save us from this, but it's not a big deal...
   (loop for bit from 0 to (/ (log max) (log 2))
	 collect (if (logbitp bit n) 1 0)
	 )))

(defun bitify-string (string)
  (let ((upstring (string-upcase string))
	(n-alkanes (length *alkanechars*))
	)
    (loop for charpos from 0 to (1- (length string))
	  as charcode =  (position (aref upstring charpos) *alkanechars* :test #'equal)
	  do (when (null charcode)
	       (break "A bad char (~s) was given in a string to bitify-string." (aref upstring charpos)))
	  append (bitify-number n-alkanes charcode)
	  )
    ))

(defun decode-module-bits (allbits)
  (declare (special *afferent-modules*))
  (loop until (null allbits)
	as module-number = (bits-to-number (loop for b from 1 to 6
						 collect (pop allbits)))
	as count = (bits-to-number (loop for b from 1 to 6 
					 collect (pop allbits)))
	collect (cons (nth module-number *afferent-modules*) count)))

(defun alkane-modules (alkane)
  (setq alkane (string-upcase alkane))
  (let* ((nmodules (extract-alkane-slot-value alkane 'nmodules))
	 (last-p/n (find-alkane-position-and-length 'nmodules))
	 (module-start (+ (car last-p/n) (cdr last-p/n))))
    (decode-module-bits (subseq (deacidify-alkane-to-allbits alkane)
				module-start
				(+ module-start (* nmodules 12))))))

;;; UUU What does this do?  Why does it use PROG?
(defun compress-out-chars (string string-of-chars-to-skip)
  (prog (limit position result)
        (setq result "" limit (length string) position 0)
   loop (cond ((= position limit)
	       (return result))
	      ((position (aref string position) string-of-chars-to-skip))
	      (t (setq result (format nil "~a~a" result (aref string position)))))
        (incf position)
        (go loop)))



;;; Replacement for FUNCALL/EVAL/APPLY, to be used in ACL runtime, which doesn't have them.
;;;
;;; NB also that (FUNCALL '(lambda ...) ...) invokes COMPILE, which
;;; is also missing in the ACL runtime.  So you have to use RUNTIME-FUNCALL
;;; for anything that is to be done at runtime.

(defun runtime-funcall (function &rest arguments)
  (runtime-apply function arguments))

(defun runtime-apply (function arguments)
  (cond ((functionp function)
	 (apply function arguments))
	((consp function)
	 (unless (eq (first function) 'lambda)
	   (error "Can't apply list ~s" function))
	 ;; PROGV => dynamic binding, which isn't quite right, but good enough for gov't work
	 (progv (second function) arguments
	   (loop for form in (rest (rest function))
		 for result = (runtime-eval form)
		 finally (return result))))
	(t
	 (error "Can't apply ~a ~s" (type-of function) function))))

;;; FFF Only enough here so far to get the current job done.
(defun runtime-eval (form)
  (cond ((constantp form)
	 (cond ((null form)
		nil)
	       ((listp form)
		(unless (eq (car form) 'quote)
		  (error "I'm confused -- since when is ~s a constant?" form))
	        (second form))
	       (t
		form)))
	((consp form)
	 (let ((car (car form)))
	   (cond ((eq car 'setq)
		  (set (second form) (runtime-eval (third form))))
		 (t
		  (let ((args (mapcar #'runtime-eval (rest form))))
		    (unless (symbolp car)
		      (error "What's this ~s thing doing in the CAR of a form?" (car form)))
		    (apply (symbol-function car) args))))))
	((symbolp form)
	 (symbol-value form))
	(t
	 (error "Don't know how to evaluate ~s ~s" (type-of form) form))))

;;; Give us everything but the . and .. subdirectories.  (Actually,
;;; it'll skip anything with a . in the first char.  Takes a path or
;;; string and assumes that it's a directory.

(defun filename-only (path-or-namestring)
  (let ((whole-string 
	 (cond ((stringp path-or-namestring) path-or-namestring)
	       (t (namestring path-or-namestring)))))
    (subseq whole-string (1+ (position #\\ whole-string :from-end t)))))	   
(defun directory-only (path-or-namestring)
  (let ((whole-string 
	 (cond ((stringp path-or-namestring) path-or-namestring)
	       (t (namestring path-or-namestring)))))
    (subseq whole-string 0 (position #\\ whole-string :from-end t))))	   

(defun subdirectories (path)
  (cond ((stringp path)
	 (loop for directory in (directory (string-append (string-right-trim "\\" path) "\\*\\"))
	       if (not (char-equal #\. (aref (car (last (pathname-directory directory))) 0)))
	       collect directory))
	(t (subdirectories (directory-only path)))))


(defun delete-if-exists (file)
  (when (probe-file file)
    (delete-file file)))

;;; +++ flush return-max, use multiple values instead
(defun extreme (list test &key (key #'identity) (return-max nil))
  (and list
       (let* ((best (car list))
              (max (funcall key best)))
         (dolist (other (cdr list) (if return-max max best))
           (let ((score (funcall key other)))
	     (when (funcall test score max)
               (setq best other max score)))))))

; +++ key arguments are slow
(defun extremes (list test &key (key #'identity))
  (if list
    (let* ((best (list (car list)))
           (max (funcall key (car best))))
      (dolist (other (cdr list) (values best max))
        (let ((score (funcall key other)))
          (if (funcall test score max)
            (setq best (list other) max score)
            (if (funcall test max score)
              nil
              (push other best))))))
    (values nil most-negative-fixnum)))

(defun maximize (list &key (key #'identity) (return-max nil))
  (declare (inline extreme))            ; not that this does anything
  (extreme list #'> :key key :return-max return-max))

(defun minimize (list &key (key #'identity) (return-max nil))
  (declare (inline extreme))            ; not that this does anything
  (extreme list #'< :key key :return-max return-max))

(defun maximums (list &key (key #'identity))
  (declare (inline extremes))            ; not that this does anything
  (extremes list #'> :key key))

(defun minimums (list &key (key #'identity))
  (declare (inline extremes))            ; not that this does anything
  (extremes list #'< :key key))

(defun closest (value list key)
  (minimize list :key #'(lambda (elt) (abs (- value (funcall key elt))))))

; +++ key args are slow
(defun filter (predicate list &key key &aux wheat)
  "Return only the elements of list meeting PREDICATE"
  (dolist (elt list (nreverse wheat))
    (when (funcall predicate (if key (funcall key elt) elt))
      (push elt wheat))))

(defun filter-out (predicate list &key key &aux wheat)
  "Return only the elements of list not meeting PREDICATE"
  (dolist (elt list (nreverse wheat))
    (unless (funcall predicate (if key (funcall key elt) elt))
      (push elt wheat))))

(defun split-list (predicate list)
  "Returns two lists extracted from list based on PREDICATE."
  (let ((wheat '()) (chaff '()))
    (dolist (elt list (values wheat chaff))
      (if (funcall predicate elt)
	  (push elt wheat) (push elt chaff)))))

(defun maptree (fcn tree)
  (if (listp tree)
      (mapcar #'(lambda (elt) (maptree fcn elt)) tree)
    (funcall fcn tree)))

; Define a memoized function.  The function should be a function in the mathematical sense
; (a mapping with no state dependencies).  It can't take &rest, &optional, or &key args.
; Comparision of new args to old is by EQUAL.  Redefining the function resets
; the cache.  
; +++ handle declarations in body
; +++ destructuring-bind is not CL
(defmacro def-memoized-function (name arglist &body body)
  (let ((ht (make-hash-table :test #'equal)))
    `(defun ,name (&rest args)
       (declare (dynamic-extent args))
       (multiple-value-bind (val found)
	   (gethash args ,ht)
	 (if found 
           val
           (setf (gethash (copy-list args) ,ht)
                 (block ,name
                   (destructuring-bind ,arglist args
                     ,@body))))))))

;;; For debugging only (could be conditionalized)
(defun ht-contents (ht)
  (let ((result nil))
    (maphash #'(lambda (key value)
                 (push (list key value) result))
           ht)
    result))

(defun ensure-list (l) (if (listp l) l (list l)))

;;; Filter the source list, removing elements that are in the bag already.
(defun filter-matches (source bag &key (source-key 'identity) (bag-key 'identity)
				       (test 'equal))
  (let ((final-list nil))
    (dolist (elt source (nreverse final-list))
      (unless (find (funcall source-key elt) bag :key bag-key :test test)
	(push elt final-list)))))

(defun cons-end (obj lis) (nconc lis (list obj)))

;;; returns the last element of a list.
(defun last1 (lis) (car (last lis)))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t
		    (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;;; This used to be in acl 3. Alternates list items and delimiters, returning a 
;;; string. Make sure to put spaces in "delimiter"
(defun list-to-delimited-string (lis delimiter)
  (if (null lis) ""
    (with-output-to-string (stream)
      (loop for item-i from 0 to (- (length lis) 2)
	  do (format stream "~a~a" (nth item-i lis) delimiter)
	  finally (format stream "~a" (last1 lis))))))

;;; Turns a list of items into a comma-separated string of items. "A, B, ..., and Z"
(defun list-to-comma-string (lis &key (format-arg "~a"))
  (cond ((null lis) "")
	((= 1 (length lis))
	 (format nil format-arg (first lis)))
	((= 2 (length lis))
	 (format nil 
		 (format nil "~a and ~a" format-arg format-arg)
		 (first lis) (second lis)))
	(t
	 (with-output-to-string (stream)
	   (loop for item-i from 0 to (- (length lis) 2)
	       do (format stream 
			  (format nil "~a, " format-arg)
			  (nth item-i lis))
	       finally (format stream 
			       (format nil "and ~a" format-arg)
			       (last1 lis)))))))

(defun bit-vector->hex-string (bit-vector)
  (pack-bit-vector bit-vector 4 #'(lambda (byte) (digit-char byte 16))))

(defun hex-string->bit-vector (string &optional bit-vector-length)
  (unpack-bit-vector string 4 #'(lambda (ch) (digit-char-p ch 16)) bit-vector-length))

;;; unix base-64
(defun bit-vector->base64-string (bit-vector)
  (pack-bit-vector bit-vector 6 #'(lambda (byte) (code-char (+ byte 32)))))

(defun base64-string->bit-vector (string &optional bit-vector-length)
  (unpack-bit-vector string 6 #'(lambda (ch) (- (char-code ch) 32)) bit-vector-length))

;;; novel "safe64" coding: 64 innocuous characters that won't freak out SQL or anyone else.

(defparameter *safe64-chars* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-")
(defparameter *safe64-bytes* (loop
				 with a = (make-array 256 :initial-element "invalid safe64 character")
				 for ch across *safe64-chars*
				 for code = (char-code ch)
				 for byte from 0
				 do (setf (aref a code) byte)
				 finally (return a)
				    ))
					       
(defun byte->safe64 (byte)
  (aref *safe64-chars* byte))

(defun safe64->byte (char)
  (aref *safe64-bytes* (char-code char)))

(defun bit-vector->safe64-string (bit-vector)
  (pack-bit-vector bit-vector 6 #'byte->safe64))

(defun safe64-string->bit-vector (string &optional bit-vector-length)
  (unpack-bit-vector string 6 #'safe64->byte bit-vector-length))

;;; like bit-vector->hex-string but packs all 8 bits into the result
(defun bit-vector->byte-string (bit-vector)
  (pack-bit-vector bit-vector 8 #'code-char))

(defun byte-string->bit-vector (string &optional bit-vector-length)
  (unpack-bit-vector string 8 #'char-code bit-vector-length))

;;; pack a bit vector into a hex string or a byte string or whatever.
;;; If the bit vector doesn't "fill up" the bytes of the packed representation,
;;; it's right justified: #*10000 packs in hex as "10", not (say) "80".
(defun pack-bit-vector (bit-vector bits-per-byte byte->char)
  (loop 
      with vector-length = (length bit-vector)
      with string-length = (ceiling vector-length bits-per-byte)
      with n-bits-short = (- (* string-length bits-per-byte) vector-length)
      with string = (make-string string-length)
      with string-index = 0
      with byte = 0
      with bits-in-byte = n-bits-short
      for bit across bit-vector
      do (setf byte (logior (ash byte 1) bit))
	 (incf bits-in-byte)
      when (= bits-in-byte bits-per-byte)
      do (setf (char string string-index) (funcall byte->char byte)
	       byte 0
	       bits-in-byte 0
	       )
	 (incf string-index)
      finally
	(return string)))

;;; unpack a hex string or byte string or whatever into a bit vector.
;;; In case the bit vector was an odd size that didn't "fill up" the 
;;; packed string, you can specify the true bit vector length.
;;; So, in hex "10" would normally yield #*00010000, but if you specified that
;;; it was to be 5 bits you'd get #*10000.
(defun unpack-bit-vector (string bits-per-byte char->byte &optional bit-vector-length)
  (loop
      with string-length = (length string)
      with bit-vector-length = (or bit-vector-length
				   (* string-length bits-per-byte))
      with a = (make-array bit-vector-length :element-type 'bit :initial-element 0)
      with bit-index = (1- bit-vector-length)
      for char-index from (1- string-length) downto 0
      for byte = (funcall char->byte (char string char-index))
      do (loop
	     for counter below bits-per-byte
	     until (minusp bit-index)
	     do (setf (sbit a bit-index) (logand byte 1))
		(setf byte (ash byte -1))
		(decf bit-index)
		)
      finally (return a)
	      )
  )

(defun search-and-replace (victim pattern replacement &key (test #'eql))
  (let ((pos (search pattern victim :test test))
	(type (etypecase victim
		(list 'list)
		(string 'string)
		(vector 'vector)))
	)
    (if pos
	(concatenate type
	  (subseq victim 0 pos)
	  replacement 
	  (subseq victim (+ pos (length pattern))))
      victim)))

(defun search-and-replace-all (victim pattern replacement &key (test #'eql))
  (loop
      for seq = victim then new-seq
      for new-seq = (search-and-replace seq pattern replacement :test test)
      until (eq seq new-seq)
      finally (return seq)))

(defun search-and-replace-alist (victim pattern-replacement-alist &key (test #'eql))
  (loop
      for seq = victim then next-seq
      for (pattern . replacement) in pattern-replacement-alist
      for next-seq = (search-and-replace-all seq pattern replacement :test test)
      finally (return seq)))


;;; These seem not to be anywhere else.
(defdwim-class rect () 
  (top left bottom right))

(defmethod make-rect (&key top left bottom right)
  (make-instance 'rect
    :top top
    :left left 
    :bottom bottom
    :right right))

(defmethod sect-rect ((r1 rect) (r2 rect))
  (make-rect :top (max (top r1) (top r2))
	     :left (max (left r1) (left r2))
	     :bottom (min (bottom r1) (bottom r2))
	     :right (min (right r1) (right r2))))

(defmethod union-rect ((r1 rect) (r2 rect))
  (make-rect :top (min (top r1) (top r2))
	     :left (min (left r1) (left r2))
	     :bottom (max (bottom r1) (bottom r2))
	     :right (max (right r1) (right r2))))

(defmethod empty-rect-p ((r rect))
  (or (null r)
      (<= (bottom r) (top r))
      (<= (right r) (left r))))

(defmethod pt-in-rect (x y (r rect))
  (and (value-in-range x (left r) (right r))
       (value-in-range y (bottom r) (top r))))


(defun nth-position (n item seq &key (test #'eql)) ;; Shd add other keys here
  (loop with start = 0 
      for pos = (position item seq :start start :test test)
      until (or (= (decf n) 0) (null pos))
      do
	(setq start (1+ pos))
      finally
	(return (when (= n 0) pos))))

(defun ~a (&rest things)
  (format nil "~{~A~}" things))

