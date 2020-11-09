; :cd c:/jshrager/cyclodyn/results/20040927
; (progn (load "chipanal.lisp") (load (compile-file "chipanal.lisp"))) 

;;; !!! This is designed to be compiled and run in the data driectory !!!

(defun test (pclcode resultname)
  (run (format nil "~a.pcl" pclcode)
       (format nil "~a.xls" resultname)
       (format nil "~a.errs" resultname)
       '(:normalize-by-amount-of-rna :do-not-invert :remove-outliers :center-by-overall-mean))
       ))

(defvar *traced-seqs* '("SLL1577" "SLR0476"))

(defvar *errout* t)

(load "c:/jshrager/lib/stats.fasl")
(load "c:/jshrager/lib/utils.fasl")

(defvar *o* t)

#|
Figures out this part from the header:

Example: THIS WILL DIFFER FOR DIFFERENT DOWNLOADS!  THIS IS JUST AN EXAMPLE

(Setq header "SPOT	NAME	GWEIGHT	s23t02cy3	s24t02cy5	s42t06cy320030909	s41t04cy320040903	s26t08cy5	s43t08cy320030909	s09t10cy3	s16t10cy5good	s27t12cy3	s28t12cy5	s44t12cy320030909	sxxt14cy3	sxxt14cy5	s67t14cy320040903	s46t16cy320030909	sxxt18cy5	s68t18cy320040903	s48t20cy320030909	s50t24cy320030909	sxxt24cy5")

|#

;;; At the same time this sets up the normalization: All chips
;;; including and after s40 has 18.7ug whereas all those before have
;;; 12.5ug so the code corrects for this by adjusting the ratios.
;;; (The reference was the same!)  This is used when you set
;;; normalization to: :normalize-by-amount-of-rna

;;; See alb notebook page 20040910 for these factors

(defvar *ugs-rna-normalization-table*
  '(("s23t02cy3" . 1.0)
    ("s24t02cy5" . 1.0)
    ("s42t06cy320030909" . 1.5)
    ("s41t04cy320040903" . 1.5) ; Not in the lab notebook pages???
    ("s26t08cy5" . 1.0)
    ("s43t08cy320030909" . 1.4)
    ("s09t10cy3" . 1.0)
    ("s16t10cy5good" . 1.0)
    ("s27t12cy3" . 1.0)
    ("s28t12cy5" . 1.0)
    ("s44t12cy320030909" . 2.0)
    ("sxxt14cy3" . 1.0)
    ("sxxt14cy5" . 1.0)
    ("s67t14cy320040903" . 1.5)
    ("s46t16cy320030909" . 1.3)
    ("sxxt18cy5" . 1.0)
    ("s68t18cy320040903" . 1.5)
    ("s48t20cy320030909" . 1.5)
    ("s50t24cy320030909" . 1.46)
    ("sxxt24cy5" 1.0)
    ))

(defvar *header*)

(defun build-time-table (infile)
  (let* ((header (with-open-file (i infile) (read-line i)))
	 (table (loop for entry in (setq *header* (string-split #\tab (subseq header (+ 8 (search "GWEIGHT" header)))))
		      as pos from 0 by 1
		      as chipno = (read-from-string (subseq entry 1 3))
		      as code-time = (read-from-string (subseq entry 4 6))
		      as new? = (and (numberp chipno) (> chipno 30))
		      as vtime = (if new? code-time (- code-time 2))
		      collect (list pos chipno (if (= vtime 24) 0 vtime))
		      )))
    table))

(defun run (infilename outfilename logfilename options)
 (with-open-file (*errout* logfilename :direction  :output :if-exists :supersede)
  (let ((outfilename outfilename))
    (format t "Writing: ~a~%" outfilename)
    (with-open-file (*o* outfilename :direction :output :if-exists :supersede)
		    (init infilename)
		    (analyze (print (create-time-map (print (build-time-table infilename) *o*)) *o*)
			     options)
		    ))))

#| Creates this from the above:

'(("t00" 00 (0 cy3)) ; (12 cy3) s50 sucks
  ("t04" 04 (1 cy3))
  ("t06" 06 (2 cy3))
  ("t08" 08 (3 cy3) (4 cy3) )
  ("t10" 10 (5 cy3) )
  ("t12" 12 (6 cy3) (8 cy3))
  ("t14" 14 (7 cy3)) Also sux
  ("t16" 16 (9 cy3))
  ("t18" 18 (10 cy3))
  ("t20" 20 (11 cy3))
  )

Except that the cy3/5 part is just x'ed out because we assume that the
inversion is done at the SMD.
|#

(defun create-time-map (ttable)
  (loop for time from 0 to 23 by 1
	as entries = (loop for (pos nil vtime) in ttable
			   when (= vtime time)
			   collect pos)
	when entries
	collect `(,(format nil "t~2,'0d" time) ,time ,@(mapcar #'(lambda (pos) (list pos 'cyx)) entries))))

(defvar *temp* nil)
(defvar *pcc6803-annotations* nil)
(defvar *pcc6803-annotations-table* (make-hash-table :test #'equal))
(defvar *data-table* (make-hash-table :test #'equal))
(defvar *results-dir* nil)
(defvar *data* nil)
(defvar *suid->seqname* nil)
(defvar *suid->seqname-table* (make-hash-table :test #'equal))

(defvar *SLID->results* (make-hash-table :test #'equal))

;;; When you download the results, you have to select:
;;; (Page 1:)
;;;  Retrieve data by SPOT ...  <<<<<<<<<<<<<<< VERY IMPORTANT SO THAT IT DOES NOT FOLD THE DATA  !!!!!!!!!!!!!!!!
;;;  INVERT THE RESULTS FOR REVERSED DYES!!! (See selected code below at: IIIII)
;;;  Include SUID/LUID/SPOT in the UID column
;;;  And in the annotations at least:
;;;    SUID, and can also use Gene Name, and Systematic Name, if you like.
;;;  And: "Use slide name" for labling
;;; (Page 2:)
;;;  You want the LogBase2 of R/G Normalized mean (or median) ratio (usually this is the default!)
;;;  Select only spots where the flag is 0.
;;;  NO ACTIVE FILTERING! (??)
;;; and turn off all the image options!

(defun load-results-file (file)
      (with-open-file (i file)
		      ;; For some reason there's a second line called "EWEIGHT" so we skip both of them.
		      (loop for l from 1 to 2 do (format t "~a~%" (read-line i nil nil)))
		      (loop for line = (xify (read-line i nil nil))
			    as k from 1 by 1
			    until (null line)
			    do (let ((entry (parse-entry line)))
				 (push (cddr entry) (gethash (second entry) *data-table*)))
			    (when (zerop (mod k 1000)) (format t "~a: ~a~%" k line))
			    )))

;;; Replaces all occurances of TABTAB with TABXTAB until there are no
;;; more, also, if there's a tab at the end, adds an X

(defun xify (line)
      (when line
	(loop as tabtab = (search "		" line)
	      until (null tabtab)
	      do (setq line (format nil "~aX~a" (subseq line 0 (1+ tabtab)) (subseq line (1+ tabtab))))
	      )
	(when (char-equal #\tab (aref line (1- (length line))))
	  (setq line (format nil "~aX" line)))
	line))


;;; The lines come in as: "996*470598 || pknA || protein kinase; PknA*1*.034*.47*.718*-.465" where the * = tab
;;; We need to fish out the second number (470598 in the above example), and dump the rest of that crap, and 
;;; turn everything else into numbers.

(defun parse-entry (line)
      (let* ((parse (string-split #\tab line))
	     (weird-field (second parse))
	     (wf-space-pos (position #\space weird-field))
	     (n2 (subseq weird-field 0 wf-space-pos))
	     )
	(mapcar #'read-from-string 
		`(,(first parse)
		  ,n2
		  ,@(nthcdr 3 parse)))))

(defun init (results-file)
      (unless (boundp '*syn6803-ontology*) 
	(load "syn6803ontology.lsp")
	(print "re-initing *gene->functions*")
	(clrhash *gene->functions*)
	(scft *syn6803-ontology*)
	)
      (unless (and (boundp '*pcc6803-annotations*) *pcc6803-annotations*)
	(format t "Reloading 6803 gene maps...~%")
	(clrhash *pcc6803-annotations-table*)
	(load "cyanogenes.lsp")
	(loop for (gene annotation) in *pcc6803-annotations*
	      do (setf (gethash (read-from-string gene) *pcc6803-annotations-table*) annotation))
	(clrhash *suid->seqname-table*)
	(load "SUIDtoSEQNAME.lsp")
	(loop for (suid SL-number) in *suid->seqname*
	      do (setf (gethash suid *suid->seqname-table*) SL-number))
	)
      (when (zerop (hash-table-count *data-table*))
	(clrhash *data-table*)
	(load-results-file results-file)
	))

(defvar *gene->functions* (make-hash-table :test #'equal))

(defun scft (l &optional path)
      (cond ((and (= 2 (length l))
		  (stringp (first l))
		  (stringp (second l))
		  )
	     (setf (gethash (first l) *gene->functions*)
		   (cons (second l) (remove "all" path :test #'string-equal)))
	     )
	    (t (let ((new-path (cons (first l) path)))
		 (mapcar #'(lambda (i) (scft i new-path)) (cdr l)))
	       )
	    )
      nil)
	
;;; Runs through the whole ontological tree and gets the mean TP values for 
;;; each group of genes below.

(defun analyze-tree (&optional (tree *syn6803-ontology*) (depth 0))
      (loop for i from 0 to (* 2 depth) do (format t " ")) ; Put out indentation spaces.
      (format t "~a: " (car tree))
      (format t "~a~%" (analyze-data (car tree)))
      ;; Go down to the next levels in the tree...
      (loop for entry in (cdr tree)
	    when (and (listp entry)
		      ;; But not when we're going to a specific gene!
		      (not (stringp (second entry)))) 
	    do (analyze-tree entry (1+ depth))))

(defun analyze-data (description)
      (let* ((results (get-results (get-cyano-genes description)))
	     (all-r (remove nil (mapcar #'(lambda (result) (getf (car result) :r)) results)))
	     (all-anova-means (remove nil (mapcar #'(lambda (result) (getf (car result) :MEANYI-)) results))))
	(list :r-mean (mean all-r)
	      :r-std (standard-deviation all-r)
	      :r-srerr (standard-error all-r)
	      :a-means (cross #'mean all-anova-means)
	      :a-std (cross #'standard-deviation all-anova-means)
	      :a-err (cross #'standard-error all-anova-means))))

(defun cross (fn list-of-lists)
      (let* ((len (length (car list-of-lists)))
	     (sums (loop for i from 1 to len collect nil)))
	(loop for list in list-of-lists
	      do 
	      (cond ((not (= len (length list)))
		     (error "In CROSS: ~a isn't of length ~a in ~a" list len list-of-lists))
		    (t (loop for sum+ on sums
			     as v in list
			     do (push v (car sum+))))))
	(loop for sum in sums
	      collect (funcall fn sum))))

(defun get-results (genes)
      (loop for (name . description) in genes
	    collect (gethash (read-from-string name) *SLID->results*)))

(defun get-cyano-genes (target &optional (tree *syn6803-ontology*) extracting?)
      (cond ((and extracting? (listp tree) (stringp (second tree))) ; We've hit a leaf...
	     (list tree))		; give it up (listed for APPENDing)
	    ((and (listp tree) (stringp (second tree)) (string-equal target (car tree))) ; Not extracting, but this is leaf and a hit!
	     (list tree))		; Also give it up!
	    (t (loop for entry in (cdr tree)
		     with extracting! = (or extracting? (string-equal target (car tree)))
		     when (listp entry)
		     append (get-cyano-genes target entry extracting!)))))


;;; The column-descriptor is: {(label hour (column {cy3 | cy5})*)}, as: '(("2pm" 14 (2 cy5))...)
;;; Indicating for each column what time it is, and then whether the target is bound to cy3 or cy5
;;; Since we always get R/G, in which Cy5 (Red) is in the numerator, we need to negate any data that 
;;; reads the other way around.  The correlation target is correlated against each value IN ORDER, so
;;; you have to REORDER your correlation target to match the sample order!

    #| For mass action normalization -- not used at the moment:

;;; These were chosen to have near-zero correlation and low F scores.  (Maybe we want near-zero correlations and high F scores?)
;;; Actually, we should maybe choose ones that are a priori constant.  How can we find those??

(defvar *normalization-genes* '( SLL1373 SLL1336 SLL1509 SLR0857
SLR0270 SLR1795 SLL0533 SLR1857 SLL1912 SLR1639 SLL1394 SLR0294
SLR0147 SLR0250 SLL0192 SLL0424 SLR1318 SLR1807 SSL2789 SSR3409
SLL1929 SLR1933 SLL1524 SLL0595 SLR0665 SSL2153 SLL1769 SLR1259
SLL1553 SLL1890 SLR1721 SLL1169 SLR1455 SLR1794 SLR1718 SLR1644
SLL0396 SLL0377 SLR0513 SLL1546 SLR1416 SLL1606 SLR1619 SLR0325
SLL1921 SLR1923 SLL0047 SLR0813 SLL0088 SLR0652 SLR0449 SLL0428
SLR0448 SLR1062 SLL1703 SLL1350 SLR1184 SLR1579 SLL1376 SLR0583
SLL0645 SLL0394 SLR0744 SLL1271 SLL1263 SLR0386 SLL1738 SLR0216
SSR1391 SLL0664 SMR0002 SLL1625 SLL1713 SSR2067 SLR1317 SLL0738
SLR1755 SLL1796 SLR1053 SLR0711 SML0006 SLR0679 SLL0640 SLR0530
SLL2002 SLR1626 SLR0093 SLL1498 SLL0362))  

|#

(defvar *normalization-genes* '(470893)) ; = SLL1577 = cpcB is used as the normalization target gene.

(defvar *normalization-factors* nil)
(defvar *centering-factors* nil)

;;; Minimum length of a set for which outlier removal will be attempted.  Then,
;;; a max of two outliers will be removed.
(defvar *min-outliner-removal-length* 5)

(defun analyze (collapse-map math-args &aux light-intensities column-labels column-means (*print-pretty* nil))
  (setq column-labels (loop for (name . from) in collapse-map collect name))
  (format *o* "Analysis at: ~a on ~a~%" (pretty-time) (date-as-int))
  (format *o* "collapse-map = ~a~%"  collapse-map)
  (loop for arg in math-args unless (member arg '(:normalize-by-overall-mean :do-not-normalize :invert :do-not-invert :remove-outliers 
									     :center-by-overall-mean 
									     :normalize-by-amount-of-rna :normalize-by-normalization-genes))
	do (error "Invalid math arg: ~a" arg))
  (format *o* "math-args = ~a~%" math-args)

  ;; Calculate Light intensities from times:

  (setq light-intensities
	(loop for (label hour . ignore) in collapse-map
	      ;; 1+ keeps there from being a zero intensity which breaks LOG2
	      collect (log (1+ (round (third (find-light-intensity hour)))) 2)))
  (format t "Light intensities = ~a~%" light-intensities)
  (format *o* "Light intensities are: ~a~%)" light-intensities)
      
  ;; Precaulate normalization factors:
      
  (let (normalization-means normalization-data)
    (flet ((include-results-in-normalization-data (results)
	     (loop for spot in results
		   do (loop for value in spot
			    as sum.n in normalization-data
			    when (numberp value)
			    do 
			    (incf (car sum.n) value)
			    (incf (cdr sum.n))
			    ))))
	  (when (member :center-by-overall-mean math-args)
	    (format *o* "~%Centering by overall means!~%")
	    (setq normalization-data
		  (loop for (label time . cols) in collapse-map
			append (loop for col in cols
				     collect (cons 0.0 0)))) ; (sum . n)
	    ;; Includes ALL genes, not just the *normalization-genes*
	    (loop for gene being the hash-keys of *data-table*
		  do (include-results-in-normalization-data (gethash gene *data-table*)))
	    (format *o* "Overall Means = ~a~%" (setq normalization-means (mapcar #'(lambda (s) (/ (car s) (cdr s))) normalization-data)))
	    (setq *centering-factors* normalization-means)
	    )
	  (when (member :normalize-by-overall-mean math-args)
	    (error ":normalize-by-overall-mean not implemented!"))
	  (when (member :normalize-by-normalization-genes math-args)
	    (format *o* "~%Normalization genes are:~%")
	    (loop for gene in *normalization-genes*
		  do (format *o* "   ~a~%" gene))
	    (setq normalization-data
		  (loop for (label time . cols) in collapse-map
			append (loop for col in cols
					  collect (cons 0.0 0)))) ; (sum . n)
	    (loop for gene in *normalization-genes*
		  do (include-results-in-normalization-data (gethash gene *data-table*)))
	    (format *o* "normalization-data = ~a~%" normalization-data)
	    (format *o* "Means = ~a~%" (setq normalization-means (mapcar #'(lambda (s) (/ (car s) (cdr s))) normalization-data)))
	    (setq *normalization-factors*
		  (loop for target-intensity in light-intensities
			as mean in normalization-means
			;; Since these are logs, norming is additive.
			collect (- target-intensity mean)))
	    )
	  ;; Use the header to create *normalization-factors* from *ugs-rna-normalization-table*
	  (when (member :normalize-by-amount-of-rna math-args)
	    (print "COMPUTING NORMALIZATION BY AMOUNT OF RNA")
	    (setq *normalization-factors* 
		  (loop for entry in *header*
			collect (cdr (assoc entry *ugs-rna-normalization-table* :test #'string-equal))))
	    )
	  ;; In case no one set normalization factors, make them all 1.0
	  (unless *normalization-factors*
	    (setq *normalization-factors* (loop for entry in *header* collect 1.0)))
	  (format *o* "*normalization-factors* = ~a~%" *normalization-factors*)
	  ))

  ;; Finally, analysis:

  (format t "Analyzing...~%")
  (clrhash *SLID->results*)
  (format *o* "Gene~cF*~csstrdf~cssedf~cCatB~cCatA~cLabel" #\tab #\tab #\tab #\tab #\tab #\tab)
  (loop for (label hour . rest) in collapse-map
	do (format *o* "~c~a" #\tab label))
  (loop for (label hour . rest) in collapse-map
	do (format *o* "~cStdErr@~a" #\tab label))
  (format *o* "~c*~c**~ccorr_coef~ccorr_r2~ccorr_p~%" #\tab #\tab #\tab #\tab #\tab)
  (loop for results being the hash-value of *data-table*
	using (hash-key suid)
	as seq-name = (gethash suid *suid->seqname-table*)
	as annotation = (gethash seq-name *pcc6803-annotations-table*)
	;; Everything follows from this calculation, which finally collapses all the data!
	as inverted-results = (combine-results results collapse-map math-args)
	as anova = (protected-anova1 seq-name inverted-results)
	as sstrdf = (when anova (getf anova :sstrdf))
	as ssedf = (when anova (getf anova :ssedf))
	as yi-means = (when anova (getf anova :yi-))
	as f* = (when anova (getf anova :f*))
	;; as significant-at-p0.05? = (when anova (f-score>p-limit? sstrdf ssedf f* *f0.05*))
	do 
	(when (member seq-name *traced-seqs* :test #'string-equal)
	  (*foo* seq-name annotation inverted-results anova))
	(format *o* "~a~c~a~c~a~c~a~c~a~c~a~c~a" 
		seq-name #\tab
		f* #\tab
		sstrdf #\tab
		ssedf #\tab
		(second (gethash (string-downcase seq-name) *gene->functions*)) #\tab
		(first (gethash (string-downcase seq-name) *gene->functions*)) #\tab
		(format nil "~a [~a]" (first (gethash (string-downcase seq-name) *gene->functions*)) seq-name)
		)
	(cond (anova
	       ;; Put out the actual time point values
	       (loop for measures in inverted-results
		     do (format *o* "~c~a" #\tab (protected-mean measures)))
	       ;; Put out the standard errors.
	       (loop as value in inverted-results
		     do (format *o* "~c~a" #\tab (when value (standard-error value))))
	       (format *o* "~c~c" #\tab #\tab)
	       ;; (pretty-hsd anova) ; Hopefully this will put out the right number of tabs regardless of results!
	       ;; At the same time calculate, display, and store these results
	       (setf (gethash seq-name *SLID->results*)
		     (list (calculate-and-display-correlations inverted-results light-intensities)
			   anova))
	       (format *o* "~%")
	       )
	      (t (loop for l in inverted-results
		       do (format *o* "	~a" (protected-mean l)))
		 (format *o* "~%"))
	      ))
  'done
  )

(defun calculate-and-display-correlations (inverted-results correlation-target)
      (let* ((combined-correlates (loop for values in inverted-results 
					as target in correlation-target
					append (loop for v in values
						     collect (cons v target))))
	     (c (correlate (mapcar #'car combined-correlates)
			   (mapcar #'cdr combined-correlates)))
	     (p (getf c :p))
	     (r (getf c :r))
	     (r2 (getf c :r2)))
	(format *o* "~c~a~c~a~c~a" #\tab r #\tab r2 #\tab p)
	c
	))

;;; Protects from null data, which breaks ANOVA1;  Tries to calculate ANOVAs w/o
;;; nil data points, as well as straight.  But if there are less than a 5 full
;;; cells, OR any cell has too few values, we don't calculate!  

(defun protected-anova1 (label results)
      (setq *temp* results)
  ;; Version that replaces entries with too few datapoints with NIL so that it doesn't calculate with them
  ;; !!! THIS SHOULD BE TEMPORARY !!!
      (setq results (loop for entry in results
			  when (>= (length entry) 2)
			  collect entry))
      (let ((non-null-results (remove nil results)))
	(cond ((< (length non-null-results) 5)
	       (format *errout* "protected-anova1 rejected ~a because there are too few filled observations: ~a~%" label results)
	       nil)
          
	      #| ;; Version that break when there's too few datapoints in a time period.
	  ((loop for cell in non-null-results
		 when (< (length cell) 2)
		 do (return t))
	   (format *errout* "protected-anova1 rejected ~a because some observations have too few datum: ~a~%" label results)
	   nil) |#
	      (t (let ((anova (ignore-errors (anova1+tukeyHSD non-null-results))))
		   (or anova
		       (progn (format *errout* "protected-anova1 rejected ~a because for an unknown reason: ~a~%" label results)
			      nil))))
	      )))
		   
;;; Given that the means are labelled a..e, and we know which are the same and which are different,
;;; statistically-speaking, display a shorthand version of this knowledge for the readers' convenience.

(defun pretty-hsd (result)
      (let* ((both (pretty-hsd-1 result))
	     (hsd* (first both))
	     (hsd** (second both)))
	(format *o* "~a" #\tab)
	(when (cdr hsd*) 
	  (format *o* "~a" hsd*)
	  )
	(format *o* "~a" #\tab)
	(when (cdr hsd**)
	  (format *o* "~a" hsd**)
	  )
	))

(defun pretty-hsd-1 (result)
      (let* ((means (getf result :meanyi-)) ; This puts them in a..e order, I hope!
	     (pretty-hsd (pretty-tukeyhsd result))
	     (labels '("a" "b" "c" "d" "e"))
	     )
	(list (pretty-hsd-2 means labels pretty-hsd '*)
	      (pretty-hsd-2 means labels pretty-hsd '**))))

(defun pretty-hsd-2 (means labels phsd target)
      (cons target 
	    (loop for (v1 v2 ***) in phsd
		  as c1 = (get-hsd-code v1 means labels)
		  as c2 = (get-hsd-code v2 means labels)
		  when (eq target ***)
		  collect (format nil "~a~a~a" C1 (<>code v1 v2) c2))))

(defun get-hsd-code (v m* l*)
      (loop for m in m* as l in l* when (= v m) do (return l)))
	
(defun <>code (v1 v2)
      (cond ((> v1 v2) ">")
	    ((< v1 v2) "<")
	    (t "=")))

;;; This is the hard part!  We get a collapse-map, as described above, and a bunch of random data, and
;;; have to turn this into something acceptable to the ANOVA1 procedure, so, for example,
;;; we take: '((-0.791 X X) (-2.599 -0.848 0.396) (1.172 X X) (1.992 3.375 X))
;;; and this header: '(("2pm" 14 (0 cy5) (1 cy3)) ("midnight" 24 (2 cy5)))
;;; and produce this: ((-0.791 -2.599 1.172 1.992) (-0.848 3.375))

;;; NOTE that the third col. is NEGATED! (if we want it to be!) because it's a cy3 
;;; column instead of a cy5 column. (Depends on how you got them from the SMD, see: IIIII notes.)

;;; (Note that we can't actually run NILs, because ANOVA1 will fail.  
;;;  There's an external check for that!)

(defun combine-results (results collapse-map math-args)
  (let ((results
	 (loop for (label time . selectors) in collapse-map
	       collect (loop for (column cy3/cy5) in selectors
			     append (loop for result in results
					  as v = (nth column result)
					  as nf = (nth column *normalization-factors*)
					  as cf = (if *centering-factors* (nth column *centering-factors*) 0.0)
					  when (and v (not (eq 'x v)))
					  collect (- (cond ((member :invert math-args)
							 (/ (if (equal 'cy5 cy3/cy5) v (- v)) nf))
							(t (/ v nf)))
						     cf))))))
    (if (member :remove-outliers math-args)
	(mapcar #'remove-outliers results)
      results)))

;;; Removing outliers means taking the mean and SD of the values and then
;;; pulling out any that are more than 2xSD from the mean.  We only do this 
;;; if there are at least *min-outliner-removal-length* entries in the set
;;; and then we have to have at least three left.

(defun remove-outliers (set)
      (if (< (length set) *min-outliner-removal-length*)
	  set
	(let* ((mean (mean set))
	       (2xsd (* 2 (standard-deviation set)))
	       (new-set (loop for item in set 
			      when (< (abs (- mean item)) 2xsd)
			      collect item)))
	  (if (>= (length new-set) 3)
	      new-set
	    (progn
	      (format t "Outlier removed on ~a failed because the file set would be: ~a~%" set new-set)
	      set)))))

(defun extract-field (n data)
      (loop for d in data collect (nth n d)))

(defun extract-log-mean-ratios (results)
  (let ((ratios (loop for entry in (mapcar #'result-Ratio-of-Means results)
		      when (numberp entry)
		      collect entry)))
    (when ratios
      (mapcar #'(lambda (r) (log r 2)) ratios))))

;;; This figures -- ready? -- the log ratio of the mean of the log ratios for the two observations
;;; incoming results.  (We'd like this to eventually return a mean and standard error but because there's
;;; only one observation so far, we don't have any such statistic.)

(defun mse-compared-median-log-ratios (r1* r2*)
  (when (and r1* r2*)
	(let ((m1 (mse-median-log-ratios r1*))
	      (m2 (mse-median-log-ratios r2*)))
	  (when (and m1 m2)
		(list (- (car m1) (car m2)) ; mean-difference-of-median-log-ratios
		      'no-se
		      m1
		      m2
		      )))))


;;; Note that median-log-ratio is the MEDIAN log ratio.

(defun mse-median-log-ratios (results)
  (let ((ratios (loop for entry in (mapcar #'result-log-ratio results)
		      when (numberp entry)
		      collect entry)))
    (when ratios
	  (cons (mean ratios)
		(standard-error ratios)))))


;;; Figure out the light levels for cyclodyn at specific times.

#| From K8000.frm:

'Double click K8000.vbp to open the VBasic application.
'Adjust AngleOffset and LattitudeAdj (See below)
'F5 to start program
'Use the DA1 slider to set max intensity (all the way to the top).
'To stop: press the square button in the control bar, or select RUN > END

'The AngleOffset and LattitudeAdju have to be set.  These are, now:

'Const AngleOffset As Double = (5.5 / 24) * 2 * Pi
'Const LattitudeAdj As Double = 0

'A.O. represents where you want to start in Radians on the
'sine curve.  An A.O.=0 means when the program runs, it starts
'at the beginning of the light cycle.  A.O.=5.5/24*2*Pi
'means that when you start running the program, the light levels
'will be set 5.5 hours into the light cycle.

'For example,. if the real time is 2pm, and you want to run the
'cyclostat so that it is lighted from 8am-8pm (real time), set
'A.O. = (6/24)*2*Pi

;;; NOTE: This is actually wrong!  If you do this, then the peak is not at noon!, because 
;;; 8am is 6 hours from noon, but *8* hours from 8pm, so it'll actually go 8-to-2-to-8, not
;;; 8-to-12-to-8; You can't go 8-to-12-to-8, but you can go 6-to-12-to-6, which is more
;;; resonable than 8-to-2-to-8, and for that you'd use an offset of 8 from 2, not 6!

'Note that if you stop the program and then later restart it, you
'need to reset A.O. approprtiately.

'L.A. is usedto adjust the total number of daylight hours.
'Right now this is set to 0 (meaning 12 hours of light/day).
'To decrease the number of light hours, set L.A. between 0 and 1.
'(1 means no light at all.)

...

Const AngleOffset As Double = (6 / 24) * 2 * Pi

' LattitudeAdj must be [0,1)
Const LattitudeAdj As Double = 0

...

Const SecsInDay As Double = 86400
'Const SecsInDay As Double = 60
Const Pi As Double = 3.14159265358979
Const AngleOffset As Double = (6 / 24) * 2 * Pi

...

    Dim Angle As Double
    Angle = 2 * Pi * TimeOfDay / SecsInDay
    
    Dim Light As Double
    Light = Sin(Angle + AngleOffset)
        
    ' note slider control is backward
    
    Dim LightOnSignal As Double
    LightOnSignal = Light - LattitudeAdj
    
    Dim ControlIntensity As Double
    If LightOnSignal < 0 Then ControlIntensity = 0 
       Else ControlIntensity = 70 + 170 * ((Light - LattitudeAdj) / (1 - LattitudeAdj)) * ((255 - Slider2.Value) / 255)
    
|#

(defvar *SecsInDay* 86400)
(defvar *LattitudeAdj* 0)
(defvar *Slide2.value* 0)

;;; This has to be reset per experiment.  Here's it's set for the 200207XX experiment,
;;; which was 8am-8pm (light cycle, dark at night, obviously).

;;; Note that you don't put the PROGRAMMED intensity in here -- that's just the thing that you
;;; sent in the program to make it start correctly from the time that you happened to start
;;; it running; Instead, you put the zerop point, as where you want the lamp zero to be, 
;;; e.g., 6am = 6, 8am = 8, etc.  The cycle is always going to be 6 hours to the top, then
;;; 6 down, then 12 of silence.

(defun find-light-intensity (hour &optional (offset 6))
  (k8000-control-intensity (- hour offset)))

(defun k8000-control-intensity (TimeOfDay) ; TimeOfDay is in hours.fractionalhour
  (let* ((AngleOffset 0)
	 (Angle (* 2 Pi (/ (* 3600 TimeOfDay) *secsInDay*)))
	 (Light (Sin (+ Angle AngleOffset)))
	 (LightOnSignal (- Light *LattitudeAdj*)))
    (list angle light 
	  (cond ((> LightOnSignal 0)
		 (+ 70 (* 170 (/ (- Light *LattitudeAdj*) (- 1 *LattitudeAdj*)) (/ (- 255 *Slide2.value*) 255.0))))
		(t 0)))))

(defun plot-light-intensities (offset)
  (loop for hour from 0 to 48
	do (format t "At ~a hour (~2'0d:00) the programmed intensity = ~a~%" hour (if (< hour 25) hour (- hour 24)) (find-light-intensity hour offset))))

;;; Excel fit the actual light intensity to: y = -9.9138(x^2) + 272.95x - 1586.4
;;; Where x is the setting.

(defun lighttest ()
  (format t "time // setting(=x) // -9.9138(x^2) + 272.95x - 1586.4 (excel fit to measured values)~%")
  (loop for time from 0 to 24 by 1
	as (a b setting) = (find-light-intensity time)
	do (format t "~a~c~a~c~a~%"
		   time #\tab 
		   setting #\tab
		   (+ (* -9.9138 (expt time 2)) (* 273 time) -1586.4))))


#| Significance for correlation coefficient:

Table: http://www.gifted.uconn.edu/siegle/research/Correlation/corrchrt.htm

http://www.miracosta.edu/Home/rmorrissette/Chapter08.htm

                           Levels of Significance for a One-Tailed Test 
	.05	.025	.01	.005 
                           Levels of Significance for a Two-Tailed Test 
df	.10	.05	.02	.01	
=====================================
1	.988	.997	.9995	.9999	
2	.900	.950	.980	.990	
3	.805	.878	.934	.959	
4	.729	.811	.882	.917	
5	.669	.754	.833	.874	
6	.622	.707	.789	.834	
7	.582	.666	.750	.798	
8	.549	.632	.716	.765	
9	.521	.602	.685	.735	
10	.497	.576	.658	.708 

-----------------------------

From: http://www2.chass.ncsu.edu/garson/pa765/correl.htm

Z-Score Conversions of Pearson's r
A correlation coefficient can be transformed into a z-score by dividing the correlation plus 1, by the same correlation minus 1; then take the natural log of the result; then divide that result by 2. The end result is Fischer's z-score conversion of Pearson's r. For example, let r = .3. Then follow the formula: Z = ln[(r+1)/r-1)]/2
so in this case

Z = ln(1.3/-.7)/2 = ln(-1.8571)/2 = .6190/2 = .3095 = the value shown in the table below


Table of Z-score conversions for Pearson's r
   r             z'
0.0000        0.0000
0.0100        0.0100
0.0200        0.0200
0.0300        0.0300
0.0400        0.0400
0.0500        0.0500
0.0600        0.0601
0.0700        0.0701
0.0800        0.0802
0.0900        0.0902
0.1000        0.1003
0.1100        0.1104
0.1200        0.1206
0.1300        0.1307
0.1400        0.1409
0.1500        0.1511
0.1600        0.1614
0.1700        0.1717
0.1800        0.1820
0.1900        0.1923
0.2000        0.2027
0.2100        0.2132
0.2200        0.2237
0.2300        0.2342
0.2400        0.2448
0.2500        0.2554
0.2600        0.2661
0.2700        0.2769
0.2800        0.2877
0.2900        0.2986
0.3000        0.3095
0.3100        0.3205
0.3200        0.3316
0.3300        0.3428
0.3400        0.3541
0.3500        0.3654
0.3600        0.3769
0.3700        0.3884
0.3800        0.4001
0.3900        0.4118
0.4000        0.4236
0.4100        0.4356
0.4200        0.4477
0.4300        0.4599
0.4400        0.4722
0.4500        0.4847
0.4600        0.4973
0.4700        0.5101
0.4800        0.5230
0.4900        0.5361
0.5000        0.5493
0.5100        0.5627
0.5200        0.5763
0.5300        0.5901
0.5400        0.6042
0.5500        0.6184
0.5600        0.6328
0.5700        0.6475
0.5800        0.6625
0.5900        0.6777
0.6000        0.6931
0.6100        0.7089
0.6200        0.7250
0.6300        0.7414
0.6400        0.7582
0.6500        0.7753
0.6600        0.7928
0.6700        0.8107
0.6800        0.8291
0.6900        0.8480
0.7000        0.8673
0.7100        0.8872
0.7200        0.9076
0.7300        0.9287
0.7400        0.9505
0.7500        0.9730
0.7600        0.9962
0.7700        1.0203
0.7800        1.0454
0.7900        1.0714
0.8000        1.0986
0.8100        1.1270
0.8200        1.1568
0.8300        1.1881
0.8400        1.2212
0.8500        1.2562
0.8600        1.2933
0.8700        1.3331
0.8800        1.3758
0.8900        1.4219
0.9000        1.4722
0.9100        1.5275
0.9200        1.5890
0.9300        1.6584
0.9400        1.7380
0.9500        1.8318
0.9600        1.9459
0.9700        2.0923
0.9800        2.2976
0.9900        2.6467

-----------------------



|#

#| From: http://helios.bto.ed.ac.uk/bto/statistics/table3.html#Analysis%20of%20Variance%200.05

Analysis of Variance
Values of F, for p = 0.05 Back to one-way ANOVA?
Back to two-way ANOVA? 

A significant difference between treatments is suggested if your calculated F value exceeds the tabulated F value. But this tells you only that you have significant differences between the treatments as a whole. It does not tell you which treatments differ from one another. 

Only some degrees are freedom are shown. If you want an intermediate value, use the next lowest in the table.

df 
for 
replicates                          df for treatments
==                                  =================
	1	2	3	4	5	6	8	12	inf.
1	161	200	216	225	230	234	239	244	254	
2	18.5	19.0	19.2	19.3	19.3	19.3	19.4	19.4	19.5	
3	10.1	9.6	9.3	9.1	9.0	8.9	8.8	8.7	8.5	
4	7.7	6.9	6.6	6.4	6.3	6.2	6.0	5.9	5.6	
5	6.6	5.8	5.4	5.2	5.1	5.0	4.8	4.7	4.4	
6	6.0	5.1	4.8	4.5	4.4	4.3	4.2	4.0	3.7	
7	5.6	4.7	4.4	4.1	4.0	3.9	3.7	3.6	3.2	
8	5.3	4.5	4.1	3.8	3.7	3.6	3.4	3.3	2.9	
9	5.1	4.3	3.9	3.6	3.5	3.4	3.2	3.1	2.7	
10	5.0	4.1	3.7	3.5	3.3	3.2	3.1	2.9	2.5	
11	4.8	4.0	3.6	3.4	3.2	3.1	3.0	2.8	2.4	
12	4.8	3.9	3.5	3.3	3.1	3.0	2.9	2.7	2.3	
13	4.7	3.8	3.4	3.2	3.0	2.9	2.8	2.6	2.2	
14	4.6	3.7	3.3	3.1	3.0	2.9	2.7	2.5	2.1	
15	4.5	3,7	3.3	3.1	2.9	2.8	2.6	2.5	2.1	
16	4.5	3.6	3.2	3.0	2.9	2.7	2.6	2.4	2.0	
17	4.5	3.6	3.2	3.0	2.8	2.7	2.6	2.4	2.0	
18	4.4	3.6	3.2	2.9	2.8	2.7	2.5	2.3	1.9	
19	4.4	3.5	3.1	2.9	2.7	2.6	2.5	2.3	1.9	
20	4.4	3.5	3.1	2.9	2.7	2.6	2.5	2.3	1.8	
21	4.3	3.5	3.1	2.8	2.7	2.6	2.4	2.3	1.8	
22	4.3	3.4	3.1	2.8	2.7	2.6	2.4	2.2	1.8	
23	4.3	3.4	3.0	2.8	2.6	2.5	2.4	2.2	1.8	
24	4.3	3.4	3.0	2.8	2.6	2.5	2.4	2.2	1.7	
25	4.2	3.4	3.0	2.8	2.6	2.5	2.3	2.2	1.7	
26	4.2	3.4	3.0	2.7	2.6	2.5	2.3	2.2	1.7	
27	4.2	3.4	3.0	2.7	2.6	2.5	2.3	2.1	1.7	
28	4.2	3.3	3.0	2.7	2.6	2.4	2.3	2.1	1.7	
29	4.2	3.3	2.9	2.7	2.5	2.4	2.3	2.1	1.6	
30	4.2	3.3	2.9	2.7	2.5	2.4	2.3	2.1	1.6	
40	4.1	3.2	2.8	2.6	2.5	2.3	2.2	2.0	1.5	
60	4.0	3.2	2.8	2.5	2.4	2.3	2.1	1.9	1.4	
120	3.9	3.1	2.7	2.5	2.3	2.2	2.0	1.8	1.3	
inf.	3.8	3.0	2.6	2.4	2.2	2.1	1.9	1.8	1.0	

|#

; :cl catcount.lisp
(defvar *cc-all-tab* (make-hash-table :test #'equal))
(defvar *cc-target-tab* (make-hash-table :test #'equal))

(defun x2catb ()
  (clrhash *cc-all-tab*)
  (clrhash *cc-target-tab*)
  (loop for name in *cc-all*
	do (if (gethash name *cc-all-tab*)
	       (incf (gethash name *cc-all-tab*))
	     (setf (gethash name *cc-all-tab*) 1)))
  (loop for name in *cc-targets*
	do (if (gethash name *cc-target-tab*)
	       (incf (gethash name *cc-target-tab*))
	     (setf (gethash name *cc-target-tab*) 1)))
  (maphash #'(lambda (name x)
	       (let* ((all (gethash name *cc-all-tab*))
		      (rat (/ (float x) all))
		      (x2 (x2-calc all x))
		      (x2sig (x2-sig x2))
		      )
		 (format t "~a	~a	~a	~a	~a	~a~%" name all x rat x2 x2sig)
		 ))
	   *cc-target-tab*))
  )

(defun x2-calc (all x)
  (let ((e (* 0.05 all)))
    (/ (expt (abs (- e x)) 2.0)
       (float e))))
  
#| Chi-square distribution

df	P=0.99	0.95	0.80	0.50	0.20	0.05	0.01
1	0.00	0.00	0.06	0.46	1.64	3.84	6.64	10.83
2	0.02	0.10	0.45	1.39	3.22	5.99	9.21	13.82
3	0.12	0.35	1.01	2.37	4.64	7.82	11.35	16.27

|#

(defun x2-sig (x2)
  (cond ((>= x2 10.83) "***0.001")
	((>= x2 6.64) "**0.01")
	((>= x2 3.84) "*0.05")
	(t "n/s")))

;;; Need to replace this list with the total set from the spreadsheet CatB:

(setq *cc-all* '(
"TCA cycle"
"Ribosomal proteins: synthesis and modification"
"Cobalamin, heme, phycobilin and porphyrin"
"ATP synthase"
"Adaptations and atypical conditions"
"Other"
"Aminoacyl tRNA synthetases and tRNA modification"
"Amino acids and amines"
"NADH dehydrogenase"
"Cytochrome oxidase"
"DNA replication, restriction, modification, recombination, and repair"
"CO2 fixation"
"CO2 fixation"
"NADH dehydrogenase"
"NADH dehydrogenase"
"Ribosomal proteins: synthesis and modification"
"Purine ribonucleotide biosynthesis"
"Thioredoxin, glutaredoxin, and glutathione"
"Cobalamin, heme, phycobilin and porphyrin"
"Photosystem II"
"Purine ribonucleotide biosynthesis"
"Regulatory functions"
"Cobalamin, heme, phycobilin and porphyrin"
"Fatty acid, phospholipid and sterol metabolism"
"Fatty acid, phospholipid and sterol metabolism"
"Protein modification and translation factors"
"Soluble electron carriers"
"Transport and binding proteins"
"Regulatory functions"
"Regulatory functions"
"Regulatory functions"
"Glycolysis"
"Regulatory functions"
"Regulatory functions"
"Other"
"Transport and binding proteins"
"CO2 fixation"
"Other"
"Phycobilisome"
"Photosystem II"
"WD repeat proteins"
"Transport and binding proteins"
"CO2 fixation"
"Other"
"Photosystem II"
"Murein sacculus and peptidoglycan"
"Ribosomal proteins: synthesis and modification"
"Transport and binding proteins"
"Regulatory functions"
"ATP synthase"
"Murein sacculus and peptidoglycan"
"Transport and binding proteins"
"Transport and binding proteins"
"Other"
"Ribosomal proteins: synthesis and modification"
"Protein and peptide secretion"
"Carotenoid"
"Ribosomal proteins: synthesis and modification"
"Protein modification and translation factors"
"Pentose phosphate pathway"
"DNA replication, restriction, modification, recombination, and repair"
"Regulatory functions"
"Cytochrome b6/f complex"
"CO2 fixation"
"Purine ribonucleotide biosynthesis"
"Ribosomal proteins: synthesis and modification"
"ATP synthase"
"Other"
"DNA replication, restriction, modification, recombination, and repair"
"Ribosomal proteins: synthesis and modification"
"Regulatory functions"
"Ribosomal proteins: synthesis and modification"
"Cobalamin, heme, phycobilin and porphyrin"
"Sugars"
"Phycobilisome"
"Other"
"Regulatory functions"
"Ribosomal proteins: synthesis and modification"
"Glutamate family / Nitrogen assimilation"
"NADH dehydrogenase"
"Transport and binding proteins"
"Branched chain family"
"Transport and binding proteins"
"Purine ribonucleotide biosynthesis"
"Adaptations and atypical conditions"
"Branched chain family"
"Cobalamin, heme, phycobilin and porphyrin"
"Phycobilisome"
"Transport and binding proteins"
"Thiamin"
"Ribosomal proteins: synthesis and modification"
"Phycobilisome"
"Fatty acid, phospholipid and sterol metabolism"
"Fatty acid, phospholipid and sterol metabolism"
"Soluble electron carriers"
"Protein modification and translation factors"
"Nucleoproteins"
"CO2 fixation"
"Phycobilisome"
"Cytochrome b6/f complex"
"Regulatory functions"
"CO2 fixation"
"Regulatory functions"
"Photosystem II"
"CO2 fixation"
"CO2 fixation"
"Surface polysaccharides, lipopolysaccharides and antigens"
"Photosystem I"
"Glycolysis"
"Protein modification and translation factors"
"Photosystem II"
"Transport and binding proteins"
"Phycobilisome"
"RNA synthesis, modification, and DNA transcription"
"Regulatory functions"
"Soluble electron carriers"
"DNA replication, restriction, modification, recombination, and repair"
"Ribosomal proteins: synthesis and modification"
"Photosystem II"
"Phycobilisome"
"Aspartate family"
"ATP synthase"
"Branched chain family"
"Pyruvate dehydrogenase"
"RNA synthesis, modification, and DNA transcription"
"Cobalamin, heme, phycobilin and porphyrin"
"Cobalamin, heme, phycobilin and porphyrin"
"Transport and binding proteins"
"Phycobilisome"
"Surface polysaccharides, lipopolysaccharides and antigens"
"Ribosomal proteins: synthesis and modification"
"Regulatory functions"
"Regulatory functions"
"Soluble electron carriers"
"Molybdopterin"
"Other"
"ATP synthase"
"Protein modification and translation factors"
"Protein modification and translation factors"
"Protein and peptide secretion"
"Ribosomal proteins: synthesis and modification"
"Ribosomal proteins: synthesis and modification"
"Soluble electron carriers"
"NADH dehydrogenase"
"ATP synthase"
"Regulatory functions"
"Regulatory functions"
"Regulatory functions"
"Photosystem II"
"Regulatory functions"
"Regulatory functions"
"Fatty acid, phospholipid and sterol metabolism"
"Ribosomal proteins: synthesis and modification"
"Transport and binding proteins"
"Ribosomal proteins: synthesis and modification"
"CO2 fixation"
"RNA synthesis, modification, and DNA transcription"
"Chaperones"
"Transport and binding proteins"
"NADH dehydrogenase"
"Fatty acid, phospholipid and sterol metabolism"
"Regulatory functions"
"Ribosomal proteins: synthesis and modification"
"Degradation of proteins, peptides, and glycopeptides"
"Regulatory functions"
"Transport and binding proteins"
"Aminoacyl tRNA synthetases and tRNA modification"
"Pentose phosphate pathway"
"Regulatory functions"
"Ribosomal proteins: synthesis and modification"
"Pentose phosphate pathway"
"Phosphorus compounds"
"Cell killing"
"CO2 fixation"
"ATP synthase"
"Ribosomal proteins: synthesis and modification"
"Photosystem I"
"Protein modification and translation factors"
"Cobalamin, heme, phycobilin and porphyrin"
"Photosystem II"
"Other"
"Ribosomal proteins: synthesis and modification"
"Surface structures"
"Glutamate family / Nitrogen assimilation"
"Cytochrome oxidase"
"Transport and binding proteins"
"Ribosomal proteins: synthesis and modification"
"Photosystem II"
"Photosystem II"
"Regulatory functions"
"Transport and binding proteins"
"Regulatory functions"
"Pyruvate dehydrogenase"
"RNA synthesis, modification, and DNA transcription"
"Ribosomal proteins: synthesis and modification"
"Phycobilisome"
"Protein modification and translation factors"
"Photosystem I"
"Cell division"
"Ribosomal proteins: synthesis and modification"
"Regulatory functions"
"Chaperones"
"Chaperones"
"Ribosomal proteins: synthesis and modification"
"NADH dehydrogenase"
"Glutamate family / Nitrogen assimilation"
"RNA synthesis, modification, and DNA transcription"
"Ribosomal proteins: synthesis and modification"
"Ribosomal proteins: synthesis and modification"
"Ribosomal proteins: synthesis and modification"
"Photosystem II"
"Fatty acid, phospholipid and sterol metabolism"
"Cytochrome b6/f complex"
"ATP synthase"
"NADH dehydrogenase"
"Photosystem I"
"Ribosomal proteins: synthesis and modification"
"Transport and binding proteins"
"Ribosomal proteins: synthesis and modification"
"Fatty acid, phospholipid and sterol metabolism"
"Aspartate family"
"Thioredoxin, glutaredoxin, and glutathione"
"Photosystem I"
"Chaperones"
"WD repeat proteins"
"Cobalamin, heme, phycobilin and porphyrin"
"Cell killing"
"NADH dehydrogenase"
"Polysaccharides and glycoproteins"
"Cobalamin, heme, phycobilin and porphyrin"
"Cell division"
"Ribosomal proteins: synthesis and modification"
"Transport and binding proteins"
"Serine family / Sulfur assimilation"
"Ribosomal proteins: synthesis and modification"
"RNA synthesis, modification, and DNA transcription"
"Glutamate family / Nitrogen assimilation"
"Thioredoxin, glutaredoxin, and glutathione"
"Branched chain family"
"Transport and binding proteins"
"Regulatory functions"
"Membranes, lipoproteins, and porins"
"Regulatory functions"
"Cell division"
"Purine ribonucleotide biosynthesis"
"Other"
"Photosystem II"
"Transport and binding proteins"
"Phycobilisome"
"Transport and binding proteins"
"CO2 fixation"
"Purine ribonucleotide biosynthesis"
"Regulatory functions"
"Chaperones"
"Aminoacyl tRNA synthetases and tRNA modification"
"Other"
"Thioredoxin, glutaredoxin, and glutathione"
"Chaperones"
"Phycobilisome"
"Nucleoproteins"
"Glutamate family / Nitrogen assimilation"
"Serine family / Sulfur assimilation"
"Regulatory functions"
"Ribosomal proteins: synthesis and modification"
"Regulatory functions"
"Cell division"
"Regulatory functions"
"Phycobilisome"
"Glutamate family / Nitrogen assimilation"
"Cytochrome oxidase"
"Other"
"Cobalamin, heme, phycobilin and porphyrin"
"Chemotaxis"
"Ribosomal proteins: synthesis and modification"
"Regulatory functions"
"Serine family / Sulfur assimilation"
"Regulatory functions"
"Thioredoxin, glutaredoxin, and glutathione"
"Cytochrome b6/f complex"
"Serine family / Sulfur assimilation"
"Transport and binding proteins"
"Branched chain family"
"Soluble electron carriers"
"Ribosomal proteins: synthesis and modification"
"Purine ribonucleotide biosynthesis"
"Polysaccharides and glycoproteins"
"Chaperones"
"Degradation of proteins, peptides, and glycopeptides"
"Hydrogenase"
"Protein and peptide secretion"
"Ribosomal proteins: synthesis and modification"
"Transport and binding proteins"
"Photosystem I"
"Cell killing"
"Phycobilisome"
"Chemotaxis"
"Chaperones"
"Photosystem II"
"Regulatory functions"
"Degradation of proteins, peptides, and glycopeptides"
"Protein and peptide secretion"
"Transport and binding proteins"
"Other"
"Molybdopterin"
"Protein modification and translation factors"
"Branched chain family"
"Pyruvate dehydrogenase"
"Other"
"Other"
"Regulatory functions"
"Glycolate pathway"
"Pentose phosphate pathway"
"Regulatory functions"
"Fatty acid, phospholipid and sterol metabolism"
"Protein and peptide secretion"
"Nicotinate and nicotinamide"
"Cell division"
"Transport and binding proteins"
"Degradation of proteins, peptides, and glycopeptides"
"Transport and binding proteins"
"Purine ribonucleotide biosynthesis"
"Photosystem I"
"Regulatory functions"
"Transposon-related functions"
"Transport and binding proteins"
"Biotin"
"NADH dehydrogenase"
"RNA synthesis, modification, and DNA transcription"
"Surface polysaccharides, lipopolysaccharides and antigens"
"Aminoacyl tRNA synthetases and tRNA modification"
"NADH dehydrogenase"
"Glycolysis"
"Glycolysis"
"Murein sacculus and peptidoglycan"
"NADH dehydrogenase"
"Branched chain family"
"Protein modification and translation factors"
"Chaperones"
"Cell division"
"Regulatory functions"
"Transposon-related functions"
"Degradation of proteins, peptides, and glycopeptides"
"Pyruvate dehydrogenase"
"Aspartate family"
"Transposon-related functions"
"Transposon-related functions"
"Purine ribonucleotide biosynthesis"
"Transposon-related functions"
"Murein sacculus and peptidoglycan"
"Transposon-related functions"
"Aminoacyl tRNA synthetases and tRNA modification"
"Pyruvate and acetyl-CoA metabolism"
"Transposon-related functions"
"Hydrogenase"
"Transposon-related functions"
"Transposon-related functions"
"TCA cycle"
"Transposon-related functions"
"Transport and binding proteins"
"Transposon-related functions"
"Protein and peptide secretion"
"Transposon-related functions"
"Transposon-related functions"
"Pentose phosphate pathway"
"Transposon-related functions"
"Ribosomal proteins: synthesis and modification"
"Transposon-related functions"
"Cell division"
"Transposon-related functions"
"Transposon-related functions"
"Transposon-related functions"
"Transposon-related functions"
"Transposon-related functions"
"Transposon-related functions"
"DNA replication, restriction, modification, recombination, and repair"
"Transposon-related functions"
"Transposon-related functions"
"Transposon-related functions"
"Aromatic amino acid family"
"Branched chain family"
"Amino acids and amines"
"Transposon-related functions"
"Transposon-related functions"
"Transposon-related functions"
"Other"
"Transposon-related functions"
"Transposon-related functions"
"RNA synthesis, modification, and DNA transcription"
"Transposon-related functions"
"Other"
"Protein and peptide secretion"
"Polysaccharides and glycoproteins"
"Transport and binding proteins"
"Aminoacyl tRNA synthetases and tRNA modification"
"Transport and binding proteins"
"DNA replication, restriction, modification, recombination, and repair"
"Transposon-related functions"
"Transposon-related functions"
"Glycolysis"
"Regulatory functions"
"Transposon-related functions"
"Transposon-related functions"
"Pyrimidine ribonucleotide biosynthesis"
"DNA replication, restriction, modification, recombination, and repair"
"Soluble electron carriers"
"Transposon-related functions"
"Transposon-related functions"
"Degradation of proteins, peptides, and glycopeptides"
"Transposon-related functions"
"Transposon-related functions"
"Transposon-related functions"
"Regulatory functions"
"Cobalamin, heme, phycobilin and porphyrin"
"Transposon-related functions"
"Cobalamin, heme, phycobilin and porphyrin"
"Transposon-related functions"
"Regulatory functions"
"Transposon-related functions"
"Transposon-related functions"
"Glutamate family / Nitrogen assimilation"
"DNA replication, restriction, modification, recombination, and repair"
"Surface polysaccharides, lipopolysaccharides and antigens"
"Aspartate family"
"Transposon-related functions"
"Amino acids and amines"
"Amino acids and amines"
"Transposon-related functions"
"Transposon-related functions"
"Transposon-related functions"
"Amino acids and amines"
"Other"
"Transposon-related functions"
"Aromatic amino acid family"
"Amino acids and amines"
"Transposon-related functions"
"Adaptations and atypical conditions"
"Transport and binding proteins"
"Aminoacyl tRNA synthetases and tRNA modification"
"Cytochrome oxidase"
"Transposon-related functions"
"Transposon-related functions"
"Other"
"Transposon-related functions"
"Transport and binding proteins"
"Carotenoid"
"Transposon-related functions"
"Transposon-related functions"
"Transport and binding proteins"
"Transposon-related functions"
"Carotenoid"
"Transport and binding proteins"
"Regulatory functions"
"Regulatory functions"
"Other"
"Regulatory functions"
"Transport and binding proteins"
"Cytochrome oxidase"
"Sugars"
"DNA replication, restriction, modification, recombination, and repair"
"Sugars"
"Other"
"Transposon-related functions"
"Adaptations and atypical conditions"
"Amino acids and amines"
"Regulatory functions"
"DNA replication, restriction, modification, recombination, and repair"
"Transposon-related functions"
"NADH dehydrogenase"
"Aromatic amino acid family"
"Transport and binding proteins"
"Pyruvate and acetyl-CoA metabolism"
"Transport and binding proteins"
"Transposon-related functions"
"Transport and binding proteins"
"Purine ribonucleotide biosynthesis"
"Protein modification and translation factors"
"Sugars"
"Glutamate family / Nitrogen assimilation"
"Amino acids and amines"
"Transposon-related functions"
"Murein sacculus and peptidoglycan"
"Aminoacyl tRNA synthetases and tRNA modification"
"Regulatory functions"
"Transposon-related functions"
"Transport and binding proteins"
"Transposon-related functions"
"Glycolate pathway"
"Surface polysaccharides, lipopolysaccharides and antigens"
"Transposon-related functions"
"NADH dehydrogenase"
"Regulatory functions"
"Polysaccharides and glycoproteins"
"Pyrimidine ribonucleotide biosynthesis"
"TCA cycle"
"Transposon-related functions"
"Degradation of proteins, peptides, and glycopeptides"
"Fatty acid, phospholipid and sterol metabolism"
"Transport and binding proteins"
"Transposon-related functions"
"Glycolysis"
"Aminoacyl tRNA synthetases and tRNA modification"
"DNA replication, restriction, modification, recombination, and repair"
"Surface polysaccharides, lipopolysaccharides and antigens"
"Other"
"DNA replication, restriction, modification, recombination, and repair"
"Murein sacculus and peptidoglycan"
"Fatty acid, phospholipid and sterol metabolism"
"Cobalamin, heme, phycobilin and porphyrin"
"Adaptations and atypical conditions"
"Protein modification and translation factors"
"Pyruvate and acetyl-CoA metabolism"
"Transport and binding proteins"
"Degradation of proteins, peptides, and glycopeptides"
"Other"
"Transport and binding proteins"
"Protein modification and translation factors"
"Cobalamin, heme, phycobilin and porphyrin"
"Murein sacculus and peptidoglycan"
"DNA replication, restriction, modification, recombination, and repair"
"Transposon-related functions"
"Polysaccharides and glycoproteins"
"Regulatory functions"
"Transposon-related functions"
"Pyruvate and acetyl-CoA metabolism"
"Transposon-related functions"
"Transport and binding proteins"
"DNA replication, restriction, modification, recombination, and repair"
"Regulatory functions"
"Adaptations and atypical conditions"
"TCA cycle"
"Transport and binding proteins"
"Menaquinone and ubiquinone"
"Regulatory functions"
"Fatty acid, phospholipid and sterol metabolism"
"Transport and binding proteins"
"Cobalamin, heme, phycobilin and porphyrin"
"Chemotaxis"
"Protein modification and translation factors"
"Drug and analog sensitivity"
"Transport and binding proteins"
"Regulatory functions"
"Other"
"DNA replication, restriction, modification, recombination, and repair"
"Regulatory functions"
"Degradation of proteins, peptides, and glycopeptides"
"Photosystem II"
"Transport and binding proteins"
"Regulatory functions"
"Regulatory functions"
"Other"
"Purine ribonucleotide biosynthesis"
"Transport and binding proteins"
"Aromatic amino acid family"
"Riboflavin"
"Transport and binding proteins"
"Transformation"
"Other"
"Aromatic amino acid family"
"Adaptations and atypical conditions"
"Membranes, lipoproteins, and porins"
"Other"
"Transport and binding proteins"
"Transport and binding proteins"
"Transposon-related functions"
"Regulatory functions"
"DNA replication, restriction, modification, recombination, and repair"
"Regulatory functions"
"Transport and binding proteins"
"Regulatory functions"
"Other"
"Murein sacculus and peptidoglycan"
"Transport and binding proteins"
"Glycolysis"
"Regulatory functions"
"Branched chain family"
"Other"
"Cobalamin, heme, phycobilin and porphyrin"
"Other"
"Murein sacculus and peptidoglycan"
"Hydrogenase"
"Transposon-related functions"
"DNA replication, restriction, modification, recombination, and repair"
"Transport and binding proteins"
"Transport and binding proteins"
"Degradation of proteins, peptides, and glycopeptides"
"Transposon-related functions"
"Regulatory functions"
"Membranes, lipoproteins, and porins"
"Degradation of proteins, peptides, and glycopeptides"
"Transport and binding proteins"
"DNA replication, restriction, modification, recombination, and repair"
"Aminoacyl tRNA synthetases and tRNA modification"
"Purine ribonucleotide biosynthesis"
"Other"
"Other"
"DNA replication, restriction, modification, recombination, and repair"
"Transport and binding proteins"
"Other"
"DNA replication, restriction, modification, recombination, and repair"
"Fatty acid, phospholipid and sterol metabolism"
"Soluble electron carriers"
"RNA synthesis, modification, and DNA transcription"
"Transport and binding proteins"
"Degradation of proteins, peptides, and glycopeptides"
"Transport and binding proteins"
"Regulatory functions"
"Surface polysaccharides, lipopolysaccharides and antigens"
"Other"
"Cell division"
"Regulatory functions"
"Regulatory functions"
"Cobalamin, heme, phycobilin and porphyrin"
"Murein sacculus and peptidoglycan"
"Other"
"Polysaccharides and glycoproteins"
"Lipoate"
"WD repeat proteins"
"DNA replication, restriction, modification, recombination, and repair"
"Menaquinone and ubiquinone"
"Surface polysaccharides, lipopolysaccharides and antigens"
"Transport and binding proteins"
"Degradation of proteins, peptides, and glycopeptides"
"Regulatory functions"
"Thiamin"
"Purine ribonucleotide biosynthesis"
"Glycolysis"
"Glutamate family / Nitrogen assimilation"
"Regulatory functions"
"Other"
"Other"
"Other"
"Phosphorus compounds"
"Aminoacyl tRNA synthetases and tRNA modification"
"Purine ribonucleotide biosynthesis"
"Fatty acid, phospholipid and sterol metabolism"
"Purine ribonucleotide biosynthesis"
"Cobalamin, heme, phycobilin and porphyrin"
"Adaptations and atypical conditions"
"Nucleoproteins"
"Other"
"Aromatic amino acid family"
"TCA cycle"
"Transformation"
"Thiamin"
"Other"
"DNA replication, restriction, modification, recombination, and repair"
"Transport and binding proteins"
"TCA cycle"
"Pentose phosphate pathway"
"DNA replication, restriction, modification, recombination, and repair"
"Transport and binding proteins"
"Pyruvate and acetyl-CoA metabolism"
"Purine ribonucleotide biosynthesis"
"Purine ribonucleotide biosynthesis"
"Transport and binding proteins"
"Transport and binding proteins"
"Transport and binding proteins"
"Hydrogenase"
"Transport and binding proteins"
"Regulatory functions"
"Murein sacculus and peptidoglycan"
"Glycolysis"
"Transport and binding proteins"
"Murein sacculus and peptidoglycan"
"DNA replication, restriction, modification, recombination, and repair"
"TCA cycle"
"Other"
"Carotenoid"
"Transposon-related functions"
"Sugars"
"Branched chain family"
"Other"
"Aminoacyl tRNA synthetases and tRNA modification"
"Protein modification and translation factors"
"Quinolinate"
"Regulatory functions"
"Degradation of proteins, peptides, and glycopeptides"
"Transport and binding proteins"
"Aspartate family"
"Amino acids and amines"
"Sugars"
"Regulatory functions"
"Other"
"Other"
"Pyrimidine ribonucleotide biosynthesis"
"Folic acid"
"DNA replication, restriction, modification, recombination, and repair"
"Polysaccharides and glycoproteins"
"Degradation of RNA"
"Transport and binding proteins"
"DNA replication, restriction, modification, recombination, and repair"
"RNA synthesis, modification, and DNA transcription"
"Murein sacculus and peptidoglycan"
"Transport and binding proteins"
"Other"
"DNA replication, restriction, modification, recombination, and repair"
"Transport and binding proteins"
"Murein sacculus and peptidoglycan"
"Pyruvate and acetyl-CoA metabolism"
"Thioredoxin, glutaredoxin, and glutathione"
"Polysaccharides and glycoproteins"
"Protein and peptide secretion"
"Transport and binding proteins"
"Regulatory functions"
"DNA replication, restriction, modification, recombination, and repair"
"Transport and binding proteins"
"Degradation of proteins, peptides, and glycopeptides"
"Cobalamin, heme, phycobilin and porphyrin"
"Chaperones"
"Soluble electron carriers"
"Riboflavin"
"Amino acids and amines"
"Regulatory functions"
"Pantothenate"
"Regulatory functions"
"Amino acids and amines"
"Regulatory functions"
"Pyridoxine"
"Transport and binding proteins"
"Other"
"Biotin"
"Transport and binding proteins"
"Amino acids and amines"
"DNA replication, restriction, modification, recombination, and repair"
"Aromatic amino acid family"
"Regulatory functions"
"Transport and binding proteins"
"Aminoacyl tRNA synthetases and tRNA modification"
"Regulatory functions"
"Cobalamin, heme, phycobilin and porphyrin"
"Glycolate pathway"
"Purine ribonucleotide biosynthesis"
"Aromatic amino acid family"
"Transposon-related functions"
"Adaptations and atypical conditions"
"Transport and binding proteins"
"Amino acids and amines"
"Regulatory functions"
"Degradation of proteins, peptides, and glycopeptides"
"Other"
"Transport and binding proteins"
"Cobalamin, heme, phycobilin and porphyrin"
"Pentose phosphate pathway"
"TCA cycle"
"Cell division"
"Transport and binding proteins"
"Murein sacculus and peptidoglycan"
"Aromatic amino acid family"
"Murein sacculus and peptidoglycan"
"Other"
"Other"
"Murein sacculus and peptidoglycan"
"Fatty acid, phospholipid and sterol metabolism"
"Pantothenate"
"Transposon-related functions"
"Transport and binding proteins"
"Phycobilisome"
"Degradation of proteins, peptides, and glycopeptides"
"Other"
"Other"
"Pyrimidine ribonucleotide biosynthesis"
"Transport and binding proteins"
"Murein sacculus and peptidoglycan"
"Other"
"Aminoacyl tRNA synthetases and tRNA modification"
"Interconversions and salvage of nucleosides and nucleotides"
"DNA replication, restriction, modification, recombination, and repair"
"DNA replication, restriction, modification, recombination, and repair"
"Transport and binding proteins"
"Murein sacculus and peptidoglycan"
"DNA replication, restriction, modification, recombination, and repair"
"Amino acids and amines"
"Menaquinone and ubiquinone"
"Murein sacculus and peptidoglycan"
"Polysaccharides and glycoproteins"
"Folic acid"
))	

;;; Need to replace this set with those that you want stats on (from CatB):

(setq *cc-targets* '(
"TCA cycle"
"Ribosomal proteins: synthesis and modification"
"Cobalamin, heme, phycobilin and porphyrin"
"ATP synthase"
"Adaptations and atypical conditions"
"Other"
"Aminoacyl tRNA synthetases and tRNA modification"
"Amino acids and amines"
"NADH dehydrogenase"
"Cytochrome oxidase"
"DNA replication, restriction, modification, recombination, and repair"
"CO2 fixation"
"CO2 fixation"
"NADH dehydrogenase"
"NADH dehydrogenase"
"Ribosomal proteins: synthesis and modification"
"Purine ribonucleotide biosynthesis"
"Thioredoxin, glutaredoxin, and glutathione"
"Cobalamin, heme, phycobilin and porphyrin"
"Photosystem II"
"Purine ribonucleotide biosynthesis"
"Regulatory functions"
"Cobalamin, heme, phycobilin and porphyrin"
"Fatty acid, phospholipid and sterol metabolism"
"Fatty acid, phospholipid and sterol metabolism"
"Protein modification and translation factors"
"Soluble electron carriers"
"Transport and binding proteins"
"Regulatory functions"
"Regulatory functions"
"Regulatory functions"
"Glycolysis"
"Regulatory functions"
"Regulatory functions"
"Other"
"Transport and binding proteins"
"CO2 fixation"
"Other"
"Phycobilisome"
"Photosystem II"
"WD repeat proteins"
"Transport and binding proteins"
"CO2 fixation"
"Other"
"Photosystem II"
"Murein sacculus and peptidoglycan"
"Ribosomal proteins: synthesis and modification"
"Transport and binding proteins"
"Regulatory functions"
"ATP synthase"
"Murein sacculus and peptidoglycan"
"Transport and binding proteins"
"Transport and binding proteins"
"Other"
"Ribosomal proteins: synthesis and modification"
"Protein and peptide secretion"
"Carotenoid"
"Ribosomal proteins: synthesis and modification"
"Protein modification and translation factors"
"Pentose phosphate pathway"
"DNA replication, restriction, modification, recombination, and repair"
"Regulatory functions"
"Cytochrome b6/f complex"
"CO2 fixation"
"Purine ribonucleotide biosynthesis"
"Ribosomal proteins: synthesis and modification"
"ATP synthase"
"Other"
"DNA replication, restriction, modification, recombination, and repair"
"Ribosomal proteins: synthesis and modification"
"Regulatory functions"
"Ribosomal proteins: synthesis and modification"
"Cobalamin, heme, phycobilin and porphyrin"
"Sugars"
"Phycobilisome"
"Other"
"Regulatory functions"
"Ribosomal proteins: synthesis and modification"
"Glutamate family / Nitrogen assimilation"
"NADH dehydrogenase"
"Transport and binding proteins"
"Branched chain family"
"Transport and binding proteins"
"Purine ribonucleotide biosynthesis"
"Adaptations and atypical conditions"
"Branched chain family"
"Cobalamin, heme, phycobilin and porphyrin"
"Phycobilisome"
"Transport and binding proteins"
"Thiamin"
"Ribosomal proteins: synthesis and modification"
"Phycobilisome"
"Fatty acid, phospholipid and sterol metabolism"
"Fatty acid, phospholipid and sterol metabolism"
"Soluble electron carriers"
"Protein modification and translation factors"
"Nucleoproteins"
"CO2 fixation"
"Phycobilisome"
"Cytochrome b6/f complex"
"Regulatory functions"
"CO2 fixation"
"Regulatory functions"
"Photosystem II"
))
