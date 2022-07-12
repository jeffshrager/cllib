;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; A little macro that lets us type in a single string over multiple lines
;;; without line wrap or extension beyond the edge of the window.
;;; So we can do:
;;; (one-string  "This is the first part of a long string, "
;;;               and this is the second part of a long string, "
;;;               and this is the last part of a long string.")
;;; and the ONE-STRING macro will replace this by a single string of
;;; the obvious content.
;;; It will also do things like
;;; (one-string "ABC" 'foobar #\Y) --> "ABCFOOBARY")
;;; (one-string "abc" "def" a-variable "ghi" "jkl") -->
;;;    (concatenate 'string "abcdef" a-variable "ghijkl")
;;; Cute, eh?

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun quoted-symbol-p (q)
  "Is a list of the form (quote <symbol>) ?"
  (and (listp q) (eql (length q) 2) 
       (eq 'quote (first q)) (symbolp (second q))))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro one-string (&rest string-designators)
  "Creates a single string (if its arguments are all constant strings)
   or a form that creates a single string (if some arguments are variables)"
  (flet ((string-designator-p (x)
           (or (stringp x) (characterp x) (quoted-symbol-p x)))
         (to-string (x)
           (cond
            ((stringp x) x)
            ((characterp x) (string x))
            ((quoted-symbol-p x) (string (second x))))))
    (cond
     ((every 'stringp string-designators)
      (apply 'concatenate 'string string-designators))
     ((every #'string-designator-p string-designators)
      (apply 'concatenate 
        'string (mapcar #'to-string string-designators)))
     (t
      (let ((reversed-form (reverse '(concatenate 'string)))
            (merged-constant-strings ""))
        (dolist (x string-designators)
          (if (not (string-designator-p x))
              (progn
                (when (> (length merged-constant-strings) 0)
                  (push merged-constant-strings reversed-form)
                  (setq merged-constant-strings ""))
                (push x reversed-form))
            (setq merged-constant-strings
                  (concatenate 'string 
                    merged-constant-strings (to-string x)))
            ))
        (when (> (length merged-constant-strings) 0)
          (push merged-constant-strings reversed-form))
        (reverse reversed-form)
        )))))

(defmacro one-string-sp (&rest string-designators)
  "Inserts spaces after every argument except the last, and calls ONE-STRING"
  (let ((sp (string #\Space)))
    `(one-string 
      ,@(loop for remaining-strings on string-designators 
              as s = (first remaining-strings)
              nconc 
              (if (cdr remaining-strings) (list s sp) (list s))
              ))))

(defmacro one-string-nl (&rest string-designators)
  "Inserts newlines after every argument, and calls ONE-STRING"
  (let ((nl (string #\Newline)))
    `(one-string ,@(loop for s in string-designators nconc (list s nl)))
    ))

(defmacro one-string-nli (&rest string-designators)
  "Inserts newlines after every argument except the last, and calls ONE-STRING"
  (let ((nl (string #\Newline)))
    `(one-string
      ,@(loop for strings on string-designators 
              as s = (first strings)
              nconc 
             (if (cdr strings) (list s nl) (list s))))))
)

;;; ----------------------- Various Utils -----------------------

(defun dbfile (name) (format nil "~a/~a" *db-path* name))
(defun trialdbfile (name) (dbfile (format nil "trialdb/~a" name)))
(defun thesaurusfile (name) (dbfile (format nil "Thesaurus/~a" name)))
(defun pdqfile (name) (dbfile (format nil "pdq/~a" name)))
(defun resultsfile (name) (dbfile (format nil "results/~a-~a" *run-tag* name)))

(defun rename-dbfile-to-tagged-bkup (file 
				     &optional (bkupfile (dbfile (format nil "bkup/~a-~a" 
									 (human-readable-timestamp) 
									 (substitute #\_ #\/ file)))))
  (if (probe-file file)
      (progn 
	(format t "Renaming ~a to ~a~%" file bkupfile)
	(rename-file file bkupfile))
    (format t "No such file as ~a to backup.~%" file)))

;;; Log to both *log* and to t -- good for reporting. WWW THIS WILL
;;; DOUBLE COMPUTE THE ARGS !!!

(defmacro formatlt (&rest args)
  `(progn
     (format *log* ,@args)
     (format t ,@args)))

(defmacro log-note (&rest strings)
  `(format *log* "~a" (one-string-nl "------------------------" (human-readable-timestamp) "::" ,@strings "------------------------" )))

(defun stream-filename (stream)
  (format nil "~a" (pathname stream)))

(defun ascii-char+[]? (char)
  (ascii-char? char :also-allow-codes '(32 45 91 93)))

(defun ascii-char? (char &key (also-allow-codes '(32 45)))
  (let ((code (char-code char)))
    (or (member code also-allow-codes) ; space dash
	(and (>= code 97) (<= code 122)) ;; a-Z
	(and (>= code 65) (<= code 90)) ;; A-Z
	(and (>= code 48) (<= code 57))) ;; 0-9
    ))

(defun suck-in-unitary-lispdb (file)
  (with-open-file (i file) (read i)))

(defun xml-from-file->lisp (file)
  (clean-xml (cxml:parse-file file (cxml:make-whitespace-normalizer (cxml-xmls:make-xmls-builder)))))

(defun clean-xml (xml)
  (loop for entry in (destringify-tree xml)
	when (not (null entry))
	collect entry))

(defun destringify-tree (tree)
  "Walk a tree and turn all the strings into symbols (or numbers)."
  (cond ((null tree) tree)
	((stringp tree) 
	 (setq tree (string-trim " 	
" tree))
	 (if (not (zerop (length tree)))
	     (cond ((char-equal #\~ (aref tree 0))
		    `(not ,(read-from-string (subseq tree 1))))
		   (t tree))))
	((atom tree) tree)
	((and (listp tree)
	      (equal "value" (car tree))
	      (equal '(("dataType" "string")) (second tree))
	      )
	 (third tree))
	(t (cons (destringify-tree (car tree))
		 (destringify-tree (cdr tree))))))

(defun remstring (target from)
  (loop with len = (length target)
	as start = (search target from :test #'char-equal)
        until (null start)
	finally (return from)
	do (setq from (format nil "~a~a" (subseq from 0 start) (subseq from (+ start len))))))

(defun replace-string (string target replacement)
  (loop with len = (length target)
	with start-at = 0
	as pos = (search target string :test #'char-equal :start2 start-at)
        until (null pos)
	finally (return string)
	do (setq string (format nil "~a~a~a" (subseq string 0 pos) replacement (subseq string (+ pos len))))
	(setq start-at (+ pos len))))

(defun downcase-tree (tree)
  (cond ((null tree) nil)
	((stringp tree) (string-downcase tree))
	((listp tree)
	 (cons (downcase-tree (car tree))
	       (downcase-tree (cdr tree))))
	(t tree)))
 
(defun shorten-string (s &optional (len 80))
  (if (> (length s) len)
      (format nil "~a..." (subseq s 0 (- len 3)))))

(defun remdups (l &key (test #'equal))
  (loop for o+ on l
	unless (find (car o+) (cdr o+) :test test)
	collect (car o+)))
  
(defun all-contiguous-sublists (set &key (length-lower-limit 1) (length-upper-limit 4))
  (loop for n from length-lower-limit to (min (length set) length-upper-limit)
	append (contiguous-sublists-of-length n set)))

(defun contiguous-sublists-of-length (n set)
  (loop for set+ on set
	when (>= (length set+) n)
	collect (first-n n set+)))

;;; Another fn that should be built-in! UUU
(defun first-n (n set)
  (loop for i below n as e in set collect e))

;;; This should be built into Lisp! I've been carrying around versions
;;; of Mike's forever!
(defun string-split (string &key (delimiter #\space))
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring (i)
             (push (subseq string last i) substrings)))
      (dotimes (i length)
        (when (eq (char string i) delimiter)
          (add-substring i)
          (setq last (1+ i))))
      (add-substring length)
      (nreverse substrings)
      )))

(defparameter *junk-chars* "()-. ,;:!	[]{}/+\\")

(defun trim-word (w) ;; used to be "clean-word" watch for old uses!!!
  (string-trim *junk-chars* (string-downcase w)))

;;; Dig-out is used to extract stuff from arbitrarily deep inside
;;; trees. You provide a tree, and one or a list of search forms. Each
;;; search form can be either a string, in which case it is assumed to
;;; head the target form (by #'string-equal), or a one argument
;;; function that is applied to the tree each step of the
;;; search. (I.e., if the form is a string, that's equivalent to
;;; saying #'(lambda (tree) (string-equal (car tree) "string")) For
;;; convenience we wrap this whole operating at each level in an
;;; ingnore-errors. The multiple forms are evaluated in sequence,
;;; allowing you to pull out sub-elements.  Note the order of
;;; extraction is outside in, so you would access the outer levels
;;; first, in the natural way (but not the lisp functional semantics
;;; way!) This can be a little confusing bcs if you find an outer key,
;;; it can shadow an inner one.

#| Usage:

(defparameter x '(("f" 0) (("a" 1 ((("b" 2) ((("c" 3) ("d" 4 (((("e" 5)) ((((("f" 6)))))))) 
			      ((("g" 7 (("h" 8)))))((("e" 25)) ((((("f" 26))))))))) 
		   ("i" 9))) ((("j" (("c" 13) 
				     ("d" 14 (((("e" 15)) ((((("f" 16)))))))) ((("g" 17 (("h" 18)))))) 10))))))

(defun test ()
  (pprint (dig-out x '("d" "f")))
  (pprint (dig-out x (list "d" #'(lambda (form) (string-equal (car form) "f")))))
  (pprint (dig-out x (list #'(lambda (form) (string-equal (car form) "d")) "f")))
  (pprint (dig-out x (list #'(lambda (form) (string-equal (car form) "d")) #'(lambda (form) (string-equal (car form) "f")))))
  )

> (test)
Results:
(("f" 6) ("f" 16))
(("f" 6) ("f" 16))
(("f" 6) ("f" 16))
(("f" 6) ("f" 16))

|#

(defun dig-out (tree forms)
  (cond ((null forms) (list tree))
	((atom tree) nil)
	((and (stringp (car forms))
	      (ignore-errors (string-equal (car forms) (car tree))))
	 (dig-out tree (cdr forms)))
	((and (listp (car forms))
	      (ignore-errors (equal (car forms) (car tree))))
	 (dig-out tree (cdr forms)))
	((and (functionp (car forms))
	      (ignore-errors (funcall (car forms) tree)))
	 (dig-out tree (cdr forms)))
	(t (append (dig-out (car tree) forms)
		   (dig-out (cdr tree) forms)))))

(defun dig-out-all (header tree)
  (dig-out tree (list header)))
(defun dig-out-first (header tree)
  (first (dig-out-all header tree)))
(defun dig* (header tree)
  (dig-out tree (list #'(lambda (form) (string-equal header (caar form))))))

#| Old versions:

(defun dig-out-first (header tree)
  (cond ((null tree) nil)
	((listp tree)
	 (cond ((equal header (car tree)) tree)
	       (t (or (dig-out-first header (car tree))
		      (dig-out-first header (cdr tree))))))))

(defun dig-out-all (header tree)
  (cond ((null tree) nil)
	((listp tree)
	 (cond ((equal header (car tree)) 
		(cons tree
		      (dig-out-all header (cdr tree))))
	       (t (append (dig-out-all header (car tree))
			  (dig-out-all header (cdr tree))))))))
;;; This version is special for the XML coming from google
;;; spreadsheets. It pulls from lists that look like: ((header ...)
;;; body)

(defun dig* (header tree)
  (cond ((null tree) nil)
	((listp tree)
	 (cond ((and (listp (car tree)) (equal header (caar tree)))
		(cons tree
		      (dig* header (cdr tree))))
	       (t (append (dig* header (car tree))
			  (dig* header (cdr tree))))))))

|#

(defun pptbl (tbl &optional (n 10))
  (loop for k being the hash-keys of tbl
	using (hash-value v)
	as i below n
	do (pprint k) (pprint v)))

;;; ----------------------- BioBike Utils -----------------------

(defun nullstring? (x) (and (stringp x) (zerop (length x))))



(defmacro formatt (format-string &rest format-args)
  "Shorthand for (format t ...) and it indents better"
  `(format t ,format-string ,@format-args))
(defmacro formatn (format-string &rest format-args)
  "Shorthand for (format nil ...) and it indents better"
  `(format nil ,format-string ,@format-args))
(defun cformatt (format-string &rest format-args)
  #.(one-string-nl
     "Writes FORMAT-STRING to standard ouput, first prepending ';; ' "
     "and postpending a newline.  (The 'c' is for 'comment'.)")
  (apply 'format t (one-string "~&;; " format-string "~%") format-args))
(defun ierror (format-string &rest format-args)
  "Signals an error whose string message begins with 'Internal error. '"
  (apply 'error (one-string "Internal error. " format-string) format-args))
(defun terpprint (form &optional (p *standard-output*))
  "Pretty prints FORM and then prints a #\Newline"
  (pprint form p) (terpri p))

;;; Mike Travers' string split doesn't copy the string, 
;;; and so is pretty efficient!
;;; BE FOREWARNED ABOUT POSSIBLE SIDE-EFFECT CONFUSION!

(defun mt-string-split 
       (string &optional (delimiter #\space) (method :new-strings))
  #.(one-string-nl 
     "Returns a list of strings which are the substrings of STRING "
     "separated by DELIMITER."
     "DELIMITER must be either a character or a string of length 1."
     "When METHOD is :IN-PLACE the strings returned"
     "share 'string space' with the orginal string STRING -- modifying any"
     "character in one of the substrings will modify the original string,"
     "and modifying a character in the original string may modify one of the"
     "substrings. Also, the substrings returned are not of type SIMPLE-STRING,"
     "since they are really displaced arrays."
     "When METHOD is :NEW-STRINGS (the default) the strings returned are"
     "newly created strings which do not share content with the original"
     "input string.  Also, the substrings returned are of type SIMPLE-STRING."
     "Use :IN-PLACE when you want to avoid creating long substrings"
     "which are copies of subparts of the original string."
     "Use :NEW-STRINGS when the substrings are short and/or the input string"
     "is not very long (or if you need to modify the returned strings and"
     "do not want the input string modified).")
  (when (and (stringp delimiter) (= 1 (length delimiter)))
    (setq delimiter (char delimiter 0)))
  (ecase method
    (:new-strings
     (simple-string-split string delimiter))
    (:in-place
     (let ((substrings '())
           (length (length string))
           (string-char-type (array-element-type string))
           (last 0))
       (flet ((add-substring (i)
                (push (make-array (- i last)
                                  :element-type string-char-type
                                  :displaced-to string
                                  :displaced-index-offset last)
                      substrings)))
         (dotimes (i length)
           (when (eq (char string i) delimiter)
             (add-substring i)
             (setq last (1+ i))))
         (add-substring length)
         (nreverse substrings)
         )))))

(defun simple-string-split (string &optional (delimiter #\space))
  #.(one-string-nl
     "Returns a list of simple strings which are the substrings of STRING"
     "separated by DELIMITER.  These substrings are true copies, and may"
     "therefore be modified without affecting the original string STRING."
     "This function is most efficient if STRING is of type simple-string.")
  (cond
   ((not (simple-string-p string))
    (mapcar
     (lambda (s) (coerce s 'simple-string))
     (mt-string-split string delimiter)
     ))
   (t (simple-string-split-fast string delimiter))
   ))
    
(defun simple-string-split-fast (sstring delimiter)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string sstring) (character delimiter))
  (let* ((len (the fixnum (length sstring)))
         (result (list nil))
         (ptr result)
         (start 0))
    (declare (fixnum len start))
    (block exit
      (cond
       ((zerop len) 
        (return-from exit (list "")))
       ((= len 1) 
        (return-from exit
          (if (char= (schar sstring 0) delimiter)
              (list "" "")
            (list (copy-seq sstring))
            ))))
      (if (char= (schar sstring 0) delimiter)
          (progn (setq start 1) (setf (cdr ptr) (list "")) (pop ptr))
        (setq start 0))
      (loop for j fixnum from 1 below len 
            as ch = (schar sstring j)
            do
            (when (char= ch delimiter) 
              (let* ((size (the fixnum (- j start)))
                     (s (make-string size)))
                (declare (fixnum size))
                (declare (simple-string s))
                (loop for k fixnum from 0 
                      for i fixnum from start below j 
                      do
                      (setf (schar s k) (schar sstring i)))
                (setf (cdr ptr) (list s))
                (pop ptr)
                (setq start (the fixnum (1+ j)))
                ))
            finally 
            (let ((last-string (if (= start len) "" (subseq sstring start))))
              (setf (cdr ptr) (list last-string))
              ))
      (cdr result)
      )))          

(defun break-string-into-blocks-of-size (string size)
  #.(one-string-nl
     "If SIZE >= LENGTH(STRING), returns a list of one element, a"
     "copy of the STRING."
     "Otherwise, a list of subsections of the string, each SIZE long"
     "is returned in order, save that the last subsequence may be shorter."
     )
  (declare (fixnum size))
  (let ((len (length string)))
    (if (<= len size) 
        (list (copy-seq string))
      (let ((nblocks (ceiling len size)))
        (declare (fixnum nblocks))
        (loop for j from 0 by size
              for k fixnum from 1 to nblocks
              collect
              (if (= k nblocks)
                  (subseq string j)
                (subseq string j (+ j size))
                ))))))

;;; Inverse of STRING-SPLIT.  Creates a single string from the
;;; list of strings, with the characters of SEP in between.

(defun string-join (string-list &optional (sep #\Space))
  "Concatenates strings together and puts SEP between each joined substring"
  (setq sep (string sep))
  (when (null string-list) (return-from string-join ""))
  (let* ((total-length 0)
         (sep-length (length sep))
         (no-sep (zerop sep-length)))
    (dolist (s string-list) (incf total-length (+ (length s) sep-length)))
    (decf total-length sep-length)
    (let ((result-string (make-string total-length))
          (current-pos 0))
      (dolist (s string-list)
        (replace result-string s :start1 current-pos)
        (incf current-pos (length s))
        (unless (or no-sep (>= current-pos total-length))
          (replace result-string sep :start1 current-pos)
          (incf current-pos sep-length)
          ))
      result-string
      )))

(defun surround (string prefix &optional (suffix prefix))
  #.(one-string-nl
     "Prepends PREFIX to STRING if it is a string and postpends SUFFIX"
     "to STRING if it is a string.  Returns a copy of STRING if any appending"
     "is done otherwise returns STRING itself. If PREFIX or SUFFIX are not"
     "strings but are non-nil an attempt is made to convert them to strings"
     "using (STRING ...")
  (cond
   ((and (null prefix) (null suffix)) string)
   ((null prefix) (one-string string (string suffix)))
   ((null suffix) (one-string (string prefix) string))
   (t (one-string (string prefix) string (string suffix)))
   ))

(defun float-to-mysql-float-string (f)
  "Returns a string acceptable to MYSQL.  E.g, 1.05d-25 -> \"1.05e-25\""
  (if (numberp f) 
      (setq f (float f)) ; In case an integer or something was passed...
    (error "In float-to-mysql-float-string: Non-numeric argument (~s)" f))
  (cond
   ((typep f 'single-float) (format nil "~A" f))
   ((typep f 'double-float) (substitute #\e #\d (format nil "~A" f)))
   ))

(defun ntranslate-string-fast (string from to)
  (declare (simple-string string from to))
  (let ((ls (length string)) (lf (length from)))
    (declare (fixnum ls lf))
    ;; Completely arbitrary test for using hash algorithm.
    (if (and (> lf 10) (> ls 100))
        (let ((ht (make-hash-table :test 'eql)))
          (loop for i fixnum below lf do
                (setf (gethash (schar from i) ht) (schar to i)))
          (loop for i fixnum below ls 
                as translation = (gethash (schar string i) ht)
                when translation
                do (setf (schar string i) translation)
                ))
      (loop for i fixnum below ls
            as pos = (position (schar string i) from)
            when pos
            do (setf (schar string i) (schar to pos)))))
  string)

(defun ntranslate-string (string from to)
  #.(one-string-nl
     "Destructively changes the characters in a string from one set to "
     "another.  For example: (ntranslate-string \"This\" "
     "\"abcdefghijklmnopqrstuvwxyz\" \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\") " 
     "will change the string to THIS and return it. "
     "NOTE THAT THIS ACTUALLY MODIFIES THE ORIGNAL STRING; "
     "If you want to preserve the string, use TRANSLATE-STRING."
     )
  (if (and (simple-string-p string) 
           (simple-string-p from) 
           (simple-string-p to))
      (ntranslate-string-fast string from to)
    (loop for i below (length string)
          as pos = (position (aref string i) from)
          when pos
          do (setf (aref string i) (aref to pos))))
  string)

(defun translate-string (string from to)
  #.(one-string-nl
     "Changes the characters in a string from one set to another. "
     "See the documentation for NTRANSLATE-STRING.")
  (ntranslate-string (copy-seq string) from to))

(defun limited-string (value &optional (limit 100))
  #.(one-string-nl
     "If VALUE is a string longer than LIMIT, returns the initial "
     "subsequence of the string postpended with '...'")
  (cond
   ((not (stringp value)) value)
   ((<= (length value) limit) value)
   (t (one-string (subseq value 0 limit) "..."))
   ))

(defun ellipsis-string (s &optional (limit 100))
  #.(one-string-nl
     "If is a string longer than LIMIT, returns the initial LIMIT-3"
     "characters with '...' postpended.  If LIMIT is 3 or less, it"
     "does not postpend the '...', it just returns the first LIMIT"
     "characters."
     "If the string is less than or equal to LIMIT in size, the"
     "original string is returned.")
  (let ((slen (length s)))
    (cond
     ((<= slen limit) s)
     ((zerop limit) "")
     ((< limit 4) (subseq s 0 limit))
     (t (one-string (subseq s 0 (- limit 3)) "..."))
     )))

(defun maybe-clip-string (string limit &optional (clip-suffix "..."))
  #.(one-string-nl
     "Returns two values."
     "If STRING is <= LIMIT in length, returns STRING and NIL."
     "Otherwise, returns as the first value a string of length LIMIT"
     "whose last characters are CLIP-SUFFIX and whose initial characters"
     "are the initial characters of STRING, and as a second string value"
     "the clipped characters of STRING."
     "(maybe-clip-string \"xyzzyfoozr\" 8 \"***\") -> \"xyzzy***\" \"foozr\" ")
  (let ((len (length string)))
    (if (<= len limit)
        (values string nil)
      (let ((clip-pos (- limit (length clip-suffix))))
        (values
         (one-string (subseq string 0 clip-pos) clip-suffix)
         (subseq string clip-pos)
         )))))

(defun copy-strings-and-add-alphabet-at-position (strings alphabet position)
  (declare (simple-string alphabet) (fixnum position))
  (let* ((alength (length alphabet))
         (replication-factor alength)
         (replications
          (loop for j fixnum from 0 below replication-factor collect
                (mapcar 'copy-seq strings)
                )))
    (declare (fixnum alength replication-factor))
    (loop for replication-set in replications
          for aindex fixnum from 0 below alength
          as char = (schar alphabet aindex) do
          (loop for s in replication-set do 
                (setf (schar (the simple-string s) position) char)))
    (apply 'nconc replications)
    ))

;;; To convert some other type (like a frame), define this method
;;; for the type in question.

(defmethod anything-stringlike-to-string ((x t))
  (anything-stringlike-to-string-or-error x))

(defun anything-stringlike-to-string-or-nil (x)
  (cond
   ((stringp x) x)
   ((characterp x) (string x))
   ((symbolp x) (string x))
   ((numberp x) (format nil "~S" x))
   ((pathnamep x) (namestring x))
   (t nil)
   ))

(defun anything-stringlike-to-string-or-error (x)
  (or (anything-stringlike-to-string-or-nil x)
      (error "Cannot use s+ to convert ~S to string form." x)))

(defun s+ (&rest args)
  #.(one-string-nl
     "First converts all ARGS to a string representation and then concatenates"
     "all the strings together. Uses the generic function"
     "ANYTHING-STRING-LIKE-TO-STRING to do the conversion.  The default method"
     "knows about strings, characters, symbols, numbers, and pathnames."
     "You can define your own method for any other type (such as a frame)."
     "Example: (s+ 1 \"abc\" 'foo #\x) --> \"1abcfoox")
  (cond
   ((null args) "")
   ((null (cdr args)) (anything-stringlike-to-string (first args)))
   ((null (cddr args)) 
    (concatenate
     'string 
     (anything-stringlike-to-string (first args))
     (anything-stringlike-to-string (second args))
     ))
   (t
    (string-join (mapcar 'anything-stringlike-to-string args) "")
    )))

(defun s+join (join &rest args)
  #.(one-string-nl
     "Converts all args to a string representation as with S+,"
     "then joins all the strings together as with STRING-JOIN using"
     "JOIN as STRING-JOIN's second argument.") 
  (setq join (anything-stringlike-to-string-or-error join))
  (string-join (mapcar 'anything-stringlike-to-string args) join)
  )

(defun string+-join (item-list &optional (sep #\Space))
  #.(one-string-nl
     "Like STRING-JOIN, except that that the ITEM-LIST elements"
     "need not be strings.  Each element is converted to a string"
     "if necessary using ANYTHING-STRINGLIKE-TO-STRING, and then"
     "STRING-JOIN is called.")
  (if (every 'stringp item-list)
      (string-join item-list sep)
    (string-join (mapcar 'anything-stringlike-to-string item-list) sep)
    ))

(defun precede-with-header-and-indent (string header &key (indent 2))
  #.(one-string-nl
     "Takes STRING, splits it into lines, adds INDENT spaces to"
     "the beginning of each line, rejoins the prepended lines and"
     "finally concatenates HEADER to the beginning of the rejoined string."
     "If HEADER is NIL, no header is appended, otherwise HEADER must"
     "be a string.")
  (let ((spaces (make-string indent :initial-element #\Space)))
    (s+
     (if header header "")
     (if header (string #\Newline) "")
     spaces
     (string-join
      (mt-string-split string #\Newline)
      (one-string (string #\Newline) spaces)
      ))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace* 
    '(#\Space #\Tab #\Return #\Newline 
              #\Page #\Null #\Linefeed
              #+:MCL #\312
              )))

#| Defined in ccl:
(defun whitespacep (x)
  #.(one-string-nl
     "If X is a character, returns T if X is a whitespace char."
     "If X is a string, returns T if every char of X is a whitespace char."
     "Whitespace characters are: " 
     (format nil "~S" *whitespace*))
  (if (stringp x)
      (every #'fast-whitespacep x)
    (not (null (fast-whitespacep x)))))
|#

(defun fast-whitespacep (char)
  (member char *whitespace* :test #'eql))

(defun center-in (string length)
  #.(one-string-nl
     "Creates a new string with whitespace to the left and right such that"
     "the original string is centered in a field of LENGTH characters"
     "The extra padding character, if any, is to the right"
     )
  (if (<= length (length string))
      string
    (let* ((excess (- length (length string)))
           (left-padding 
            (if (evenp excess) (/ excess 2) (floor excess 2)))
           (centered-string (make-string length :initial-element #\Space)))
      (replace centered-string string :start1 left-padding)
      centered-string
      )))

(defun string-to-lines (string line-width &key (max-lines nil))
  #.(one-string-nl
     "Creates a list of strings each LINE-WIDTH long containing the characters"
     "of STRING.  STRING is split up into words (things separated by spaces)"
     "and words are not split up between lines unless some words are longer"
     "than LINE-WIDTH."
     )
  (flet ((newline () (make-string line-width :initial-element #\Space)))
    (let ((words (mt-string-split string #\Space))
          (current-pos 0)
          (current-line (newline))
          (lines nil))
      (loop for word in words 
            as wordlen = (length word)
            as remaining-width = (- line-width current-pos)
            do
            (cond
             ((< wordlen remaining-width)
              (replace current-line word :start1 current-pos)
              (incf current-pos (1+ wordlen))
              )
             ((= wordlen remaining-width)
              (replace current-line word :start1 current-pos)
              (push current-line lines)
              (setq current-line (newline))
              (setq current-pos 1)
              )
             ((and (> wordlen remaining-width) (< wordlen line-width))
              (push current-line lines)
              (setq current-line (newline))
              (replace current-line word :start1 0)
              (setq current-pos (1+ wordlen))
              )
             ((and (> wordlen remaining-width) (= wordlen line-width))
              (push current-line lines)
              (setq current-line (newline))
              (replace current-line word :start1 0)
              (push current-line lines)
              (setq current-line (newline))
              (setq current-pos 1)
              )
             (t
              (push current-line lines)
              (let ((lines-needed (ceiling wordlen line-width))
                    (last-line-size (mod wordlen line-width)))
                (loop for j from 1 to (1- lines-needed)
                      for wordpos = 0 then (+ wordpos line-width)
                      do 
                      (push (subseq word wordpos (+ wordpos line-width)) lines)
                      finally
                      (if (zerop last-line-size)
                          (progn
                            (push (subseq word wordpos (+ wordpos line-width))
                                  lines)
                            (setq current-line (newline))
                            (setq current-pos 1)
                            )
                        (progn
                          (setq current-line (newline))
                          (replace 
                           current-line word :start2 (+ wordpos line-width))
                          (setq current-pos (1+ last-line-size))
                          ))))))
            finally
            (unless (every (lambda (x) (char-equal x #\Space)) current-line)
              (push current-line lines)
              ))
      (let ((result (reverse lines)))
        (cond
         ((null max-lines) result)
         ((<= (length result) max-lines) result)
         (t (subseq result 0 max-lines))
         )))))
            
           
(defun string-after-first (s char)
  (setq 
   char 
   (cond
    ((characterp char) char)
    ((stringp char) (char char 0))
    (t (error "Invalid CHAR argument, ~S, to STRING-AFTER-FIRST" char))
    ))
  (let ((pos (position char s)))
    (if pos (if (= pos (length s)) "" (subseq s (1+ pos))) "")
    ))

(defun string-before-first (s char)
  (setq 
   char 
   (cond
    ((characterp char) char)
    ((stringp char) (char char 0))
    (t (error "Invalid CHAR argument, ~S, to STRING-BEFORE-FIRST" char))
    ))
  (let ((pos (position char s)))
    (if pos (if (= pos 0) "" (subseq s 0 pos)) (copy-seq s))
    ))

(defun replace-chars 
       (old-string mapping &optional (remove-all-other-non-ascii? t))
  #.(one-string-nl
     "This function takes a string to be replaced and a mapping of characters"
     "to their replacements.  MAPPING is either a list of the form"
     "((bad-code-char replacement-string) (bad-code-char replacement-string)..)"
     "or a vector which maps character codes to replacement strings, or a"
     "hash-table whose keys are characters and whose values are replacement"
     "strings."
     ""
     "If no characters need to be replaced, the"
     "original string is returned if OLD-STRING is a simple-string (otherwise"
     "a copy of OLD-STRING is returned), and the second value is nil."
     "If characters have been replaced (or deleted), a new string is"
     "returned and the second value is T."
     )
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (not (stringp old-string))
    (error "First argument to REPLACE-BAD-CHARS must be a string!"))
  (unless (simple-string-p old-string)
    (setq old-string (coerce old-string 'simple-string)))
  (locally 
    (declare (simple-string old-string))
    (block exit
      (multiple-value-bind (new-length to-be-changed?)
          (bad-chars-info old-string mapping remove-all-other-non-ascii?)
        (when (not to-be-changed?) (return-from exit (values old-string nil)))
        (let ((new-string (make-string new-length)))
          (declare (simple-string new-string))
          (loop 
           for char across old-string
           for pos fixnum from 0
           as code fixnum = (char-code char)
           do
           (cond
            ((etypecase mapping
               (list 
                (loop
                 for (possible-match replacement) in mapping
                 as new-chars-length fixnum = (length replacement)
                 as delta fixnum = (the fixnum (- new-chars-length 1))
                 do
                 (etypecase possible-match
                   (number 
                    (when (= code (the fixnum possible-match))
                      (add-the-new-chars 
                       replacement new-chars-length new-string pos)
                      (incf pos delta)
                      (return t)
                      ))
                   (character 
                    (when (char= char possible-match)
                      (add-the-new-chars
                       replacement new-chars-length new-string pos)
                      (incf pos delta)
                      (return t)
                      ))
                   (simple-string 
                    (when (char= char (schar possible-match 0))
                      (add-the-new-chars
                       replacement new-chars-length new-string pos)
                      (incf pos delta)
                      (return t)
                      ))
                   (string 
                    (when (char= char (char possible-match 0))
                      (add-the-new-chars
                       replacement new-chars-length new-string pos)
                      (incf pos delta)
                      (return t)
                      )))
                 finally (return nil)
                 ))
               ;; assumes hash-table keys are characters
               (hash-table 
                (let ((replacement (gethash char mapping)))
                  (when replacement
                    (let ((new-chars-length (length replacement)))
                      (declare (fixnum new-chars-length))
                      (add-the-new-chars 
                       replacement new-chars-length new-string pos)
                      (incf pos (the fixnum (- new-chars-length 1)))
                      t
                      ))))
               (vector
                (when (< code (length mapping))
                  (let ((replacement (aref mapping code)))
                    (when replacement
                      (let ((new-chars-length (length replacement)))
                        (declare (fixnum new-chars-length))
                        (add-the-new-chars 
                         replacement new-chars-length new-string pos)
                        (incf pos (the fixnum (- new-chars-length 1)))
                        t
                        )))))))
            ((and remove-all-other-non-ascii? (> code 127)) nil)
            (t (setf (schar new-string pos) char))
            ))
          (values new-string to-be-changed?)
          )))))

(defun add-the-new-chars (new-chars new-chars-length new-string pos)
  (declare (fixnum new-chars-length pos))
  (declare (simple-string new-string))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (etypecase new-chars
    (simple-string
     (loop for new-pos fixnum from 0 below new-chars-length
           do
           (setf 
            (schar new-string (the fixnum (+ pos new-pos)))
            (schar new-chars new-pos))
           ))
    (string 
     (add-the-new-chars 
      (coerce new-chars 'simple-string) new-chars-length new-string pos)
     )))
      
(defun bad-chars-info (old-string mapping remove-all-other-non-ascii?)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (simple-string-p old-string)
    (setq old-string (coerce old-string 'simple-string)))
  (locally
    (declare (simple-string old-string))
    (let ((to-be-changed? nil)
          (slen 0))
      (declare (fixnum slen))
      (loop for char across old-string 
            as code fixnum = (char-code char)
            as ilen fixnum = 
            (cond 
             ((etypecase mapping
                (list
                 (loop for (possible-match replacement) in mapping
                       do
                       (etypecase possible-match
                         (number 
                          (when (= code (the fixnum possible-match))
                            (setq to-be-changed? t)
                            (return (length replacement))
                            ))
                         (character 
                          (when (char= char possible-match)
                            (setq to-be-changed? t)
                            (return (length replacement))
                            ))
                         (simple-string 
                          (when (char= char (schar possible-match 0))
                            (setq to-be-changed? t)
                            (return (length replacement))
                            ))
                         (string 
                          (when (char= char (char possible-match 0))
                            (setq to-be-changed? t)
                            (return (length replacement))
                            )))
                       finally (return nil)
                       ))
                (hash-table 
                 (let ((replacement (gethash char mapping)))
                   (when replacement 
                     (setq to-be-changed? t)
                     (length replacement)
                     )))
                (vector
                 (when (< code (length mapping))
                   (let ((replacement (aref mapping code)))
                     (when replacement
                       (setq to-be-changed? t) 
                       (length replacement)
                       ))))))
             ((and remove-all-other-non-ascii? (> code 127))
              (setq to-be-changed? t)
              0
              )
             (t 1)
             )
            do
            (setq slen (the fixnum (+ slen ilen)))
            )
      (values
       slen
       to-be-changed?
       ))))

(defun nth-position (n string char)
  #.(one-string-nl
     "Returns the position of the Nth occurence of CHAR.  If CHAR"
     "does not appear or does not appear N times, NIL is returned.")
  (declare (fixnum n))
  (declare (character char))
  (block exit
    (if (not (simple-string-p string))
        (nth-position n (coerce string 'simple-string) char)
      (locally 
        (declare (simple-string string))
        (declare (optimize (speed 3) (safety 0) (debug 0)))
        (unless (plusp n)
          (error "Nth argument must be a positive integer!"))
        (let ((count 0))
          (declare (fixnum count))
          (loop for ch across string
                for j fixnum from 0
                do
                (when (char= ch char) 
                  (when (= (incf count) n)
                    (return-from exit j)
                    ))
                finally
                (return-from exit nil)
                ))))))
        
(defun sbutlast (string &optional (n 1))
  #.(one-string-nl
     "Returns the string minus its last N (default 1) characters."
     "If there are less than or equal to N characters in the string,"
     "the null string is returned.")
  (let ((len (length string)))
    (if (<= len n) "" (subseq string 0 (- len n)))
    ))

(defun combine-and-cleanup-strings-for-excel (strings &key (char-check-fn #'ascii-char?))
  (loop for string in (remove nil (remdups strings :test #'string-equal))
	with result = ""
	do (setq result (format nil "~a; ~a" result 
				(loop for char across string
				      unless (funcall char-check-fn char)
				      do (setq string (delete char string))
				      finally (return string))))
	finally (return (string-trim "; " result))))

;;; ----------------------- Lat/Long Distance -----------------------

;;; Zipcode table loader

;;; We got this table from
;;; http://www.stat.yale.edu/~jay/230/Week4/Monday/zipcode.csv But it
;;; CSV, which is moronic, so have to load into excel and dump as tsv
;;; and then load. WARNING: Make sure that you set the format of the
;;; first col to Zip code before re-dumping the file!!! WARNING: You
;;; also need to set-buffer-file-coding... unix in emacs. --- f'ing
;;; stupidity!

(defvar *zip7string->data* (make-hash-table :test #'equal))

(defun load-zip-data ()
  (when (zerop (hash-table-count *zip7string->data*))
    (with-open-file 
     (i (pdqfile "zipcode.txt"))
     (read-line i nil nil) ;; skip header
     (loop for line = (read-line i nil nil)
	   as k from 1 by 1
	   until (null line)
	   do (when (zerop (mod k 1000)) (print k))
	   (let ((data (loop for key in '(:Zipcode :ZipCodeType :City :State :LocationType :Lat :Long :Location :Decommisioned)
			     as data in (string-split line :delimiter #\tab)
			     do (case key ((:lat :long) (setq data (ignore-errors (read-from-string data)))))
			     append (list key data))))
	     (setf (gethash (getf data :zipcode) *zip7string->data*) data))))
    (format t "Loaded ~a zipcodes~%" (hash-table-count *zip7string->data*))))

(defun ensure-zip7string (zip)
  (if (and (stringp zip)
	   (= 5 (length zip)))
      zip
    (let ((-pos (position #\- zip)))
      (if (and -pos (= 5 -pos) (= (length zip) 10))
	  (subseq zip 0 5)
	(progn (format t "in ensure-zip7string ~s isn't a valid zip code; returning nil!" zip)
	       nil)))))

(defun zip-distance (za zb)
  (let* ((adata (gethash (ensure-zip7string za) *zip7string->data*))
	 (bdata (gethash (ensure-zip7string zb) *zip7string->data*))
	 )
    (when (and adata bdata)
      (round
       (geodistance (getf adata :lat) (getf adata :long)
		    (getf bdata :lat) (getf bdata :long)
		    :unit :mi)))))
	 
(defun geodistance (lat1 lon1 lat2 lon2 &key (unit :km)) ;; unit can also be :mi
  (labels 
   ((torad (n) (/ (* n 3.1416) 180.0))) 
   (let* ((R 6371) ;; earth radius in km
	  (dLat (torad (- lat2 lat1)))
	  (dLon (torad (- lon2 lon1)))
	  (lat1 (torad lat1))
	  (lat2 (torad lat2))
	  (a (+ (* (sin (/ dlat 2.0))
		   (sin (/ dlat 2.0)))
		(* (sin (/ dlon 2.0))
		   (sin (/ dlon 2.0))
		   (cos lat1)
		   (cos lat2))))
	  (c (* 2.0 (atan (/ (sqrt a) (sqrt (- 1.0 a))))))
	  (km (* r c))
	  )
     (case unit
	   (:km km)
	   (:mi (/ km 1.61))
	   (t (error "GEODISTANCE given bad :unit :~a" unit)))
     )))

;;; -----------------

(defvar *spreadsheet-index* nil)

(defun index-spreadsheets ()
  (unless *spreadsheet-index* 
    (setq *spreadsheet-index*
	  (loop for file in (directory "/Users/jeffshrager/crx/spreadsheets/*" :directories t)
		as dir = (car (last (pathname-directory file)))
		when (= (length dir) 23) ;; Heuristically a spreadsheet directory
		collect (cons (with-open-file 
			       (i (format nil "/Users/jeffshrager/crx/spreadsheets/~a/meta.yml" dir))
			       (loop for line = (read-line i nil nil)
				     when (search ":title: " line)
				     do (return (subseq line 8))))
			      (loop for file in (directory (format nil "/Users/jeffshrager/crx/spreadsheets/~a/*list.xml" dir))
				    as title = (with-open-file 
						(i file)
						(loop for line = (read-line i nil nil)
						      as p1 = (search "<title>" line)
						      as p2 = (search "</title>" line)
						      until (null line)
						      when (and p1 p2)
						      do (return (subseq line (+ p1 7) p2))))
				    collect (list title file)))))))

(defparameter *model-key->ss-name*
  '((:colorectal . "Colorectal Model")
    (:breast . "Breast Model")
    (:lung . "Lung Model")
    (:melanoma . "Melanoma Model")
    ))

(defun get-spreadsheet-file (model sheet-name)
  (index-spreadsheets)
  (let* ((model-ss-name (cdr (assoc model *model-key->ss-name*)))
	 (model-list (cdr (assoc model-ss-name *spreadsheet-index* :test #'string-equal)))
	 (file (second (assoc sheet-name model-list :test #'string-equal))))
    file))

(defun human-readable-timestamp ()
  (multiple-value-bind 
   (sec min hr day mo year)
   (decode-universal-time (get-universal-time))
   (format nil "~a~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mo day hr min sec)))

;;; Cleann up trial XML files, xlating fancy unicode chars into ascii
;;; equivs characters. Then re-compute their Lisp forms.

(defparameter *unicode-to-ascii* ;; Following #\latin_small_letter_a_with_circumflex
  '(
    ((#\U+0080 #\Cent_Sign) "*")
    ((#\U+0080 #\Broken_Bar) "...")
    ((#\U+0080 #\U+0098) "'")
    ((#\U+0080 #\U+0099) "'")
    ((#\U+0080 #\U+0093) "-")
    ((#\U+0080 #\U+009C) "\"")
    ((#\U+0080 #\U+009D) "\"")
    ((#\U+0080 #\U+0094) "-")
    ((#\U+0080 #\U+0092) "degC")
    ((#\U+0080 #\U+009F) "\"")
    ((#\U+0080 #\U+008B) "v[?]")
    ((#\U+0080 #\No-Break_Space) "[sword]")
    ((#\U+0080 #\U+0090) "-")

    ((#\U+0081 #\Superscript_One) "&gt;=")

    ((#\U+0082 #\Not_Sign) "(R)")

    ((#\U+0084 #\U+0083) "degC")
    ((#\U+0084 #\Cent_Sign) "(TM)")

    ((#\U+0085 #\Superscript_One) "x")

    ((#\U+0085 #\No-Break_Space) "1[I]")
    ((#\U+0085 #\Inverted_Exclamation_Mark) "2[II]")
    ((#\U+0085 #\Cent_Sign) "3[III]")
    ((#\U+0085 #\Pound_Sign) "4[IV]")
    ((#\U+0085 #\Currency_Sign) "5[V]")

    ((#\U+0086 #\U+0093) "[down-arrow]")
    ((#\U+0086 #\U+0092) "--&gt;")

    ((#\U+0088 #\U+0092) "-") 
    ((#\U+0088 #\U+009E) "[Infinity]")

    ((#\U+0089 #\U+0088) "~")
    ((#\U+0089 #\Broken_Bar) "&lt;=")
    ((#\U+0089 #\Section_Sign) "&gt;=")
    ((#\U+0089 #\Yen_Sign) "&gt;=")
    ((#\U+0089 #\Currency_Sign) "&lt;=")

    ((#\U+0091 #\No-Break_Space) "(1)")
    ((#\U+0091 #\Inverted_Exclamation_Mark) "(2)")
    ((#\U+0091 #\Cent_Sign) "(3)")
    ((#\U+0091 #\Pound_Sign) "(4)")
    ((#\U+0091 #\Yen_Sign) "(6)")
    ((#\U+0091 #\Broken_Bar) "(7)")
    ((#\U+0091 #\Section_Sign) "(8)")

    ))

(defun xlate-unicode-file-in-place (infile)
  (format t "xlating unicode for ~a~%" infile)
  (with-open-file 
   (i infile)
   (with-open-file
    (o "/tmp/xl.tmp" :direction :output :if-exists :supersede)
    (loop for line = (read-line i nil nil)
	  until (null line)
	  do 
	  (loop with new = ""
		   with skip = 0
		   as pos from 0 by 1
		   as char across line
		   do
		   (cond ((not (zerop skip))
			  (decf skip))
			 ((equal char #\latin_small_letter_a_with_circumflex)
			  (let ((pair (list (aref line (+ pos 1)) (aref line (+ pos 2)))))
			    (setq new (format nil "~a~a" new (second (assoc pair *unicode-to-ascii* :test #'equal))))
			    (setq skip 2)))
			 (t (setq new (format nil "~a~c" new char)))
			 )
		   finally (format o "~a~%" new)
		   ))))
  (let ((holdfile (format nil "~a.xlhold" infile)))
    (when (probe-file holdfile)
      (delete-file holdfile))
    (rename-file infile holdfile)
    (rename-file "/tmp/xl.tmp" infile)
    ))
