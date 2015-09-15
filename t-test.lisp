;;;; Functions for running statistical t-test on results of experiments
;;;; Normally use function T-TEST-FILE to evaluate the data in a result file
;;;; created by UNIVERSAL-TESTER

;;;; Copyright (c) 1991 by Raymond Joseph Mooney. This program may be freely copied, used, or
;;;; modified provided that this copyright notice is included in each copy of this code
;;;; and parts thereof.

(in-package 'user)
(provide 't-test)

(defparameter *t-one-tail-sig-values* 
   '((.0005 (636.619 31.598 12.924 8.610 6.869 5.959 5.408 5.041 4.781 4.587 4.437 4.318
	     4.221 4.140 4.073 4.015 3.965 3.922 3.883 3.850 3.819 3.792 3.767 3.745
	     3.725 3.707 3.690 3.674 3.659 3.646 3.646 3.646 3.646 3.646 3.646 3.646
	     3.646 3.646 3.646 3.551 3.551 3.551 3.551 3.551 3.551 3.551 3.551 3.551
	     3.551 3.551 3.551 3.551 3.551 3.551 3.551 3.551 3.551 3.551 3.551 3.460))
     (.005 (63.657 9.925 5.841 4.604 4.032 3.707 3.499 3.355 3.250 3.169 3.106 3.055
	    3.012 2.977 2.947 2.921 2.898 2.878 2.861 2.845 2.831 2.819 2.807 2.797
	    2.787 2.779 2.771 2.763 2.756 2.750 2.750 2.750 2.750 2.750 2.750 2.750
	    2.750 2.750 2.750 2.704 2.704 2.704 2.704 2.704 2.704 2.704 2.704 2.704
	    2.704 2.704 2.704 2.704 2.704 2.704 2.704 2.704 2.704 2.704 2.704 2.660))
     (.01 (31.821 6.965 4.541 3.747 3.365 3.143 2.998 2.896 2.821 2.764 2.718 2.681
	   2.650 2.624 2.602 2.583 2.567 2.552 2.539 2.528 2.518 2.508 2.500 2.492
	   2.485 2.479 2.473 2.467 2.462 2.457 2.457 2.457 2.457 2.457 2.457 2.457
	   2.457 2.457 2.457 2.423 2.423 2.423 2.423 2.423 2.423 2.423 2.423 2.423
	   2.423 2.423 2.423 2.423 2.423 2.423 2.423 2.423 2.423 2.423 2.423 2.390))
     (.025 (12.706 4.303 3.128 2.776 2.571 2.447 2.365 2.306 2.262 2.228 2.201 2.179
	    2.160 2.145 2.131 2.120 2.110 2.101 2.093 2.086 2.080 2.074 2.069 2.064
            2.060 2.056 2.052 2.048 2.045 2.042 2.042 2.042 2.042 2.042 2.042 2.042 
            2.042 2.042 2.042 2.021 2.021 2.021 2.021 2.021 2.021 2.021 2.021 2.021 
            2.021 2.021 2.021 2.021 2.021 2.021 2.021 2.021 2.021 2.021 2.021 2.000))
     (.05 (6.314 2.920 2.353 2.132 2.015 1.943 1.895 1.860 1.833 1.812 1.796 1.782
  	    1.771 1.761 1.753 1.746 1.740 1.734 1.729 1.725 1.721 1.717 1.714 1.711
	    1.708 1.706 1.703 1.701 1.699 1.697 1.697 1.697 1.697 1.697 1.697 1.697 
	    1.697 1.697 1.697 1.684 1.684 1.684 1.684 1.684 1.684 1.684 1.684 1.684 
            1.684 1.684 1.684 1.684 1.684 1.684 1.684 1.684 1.684 1.684 1.684 1.671)))
    "t-values ordered by df sufficient for significance at the given levels for one-tailed t-test")

;;;; -------------------------------------------------------------------------------------------------
;;;;   T-TEST for result file of UNIVERSAL-TESTER
;;;; -------------------------------------------------------------------------------------------------

(defvar *result-assoc-list* nil)
(defvar *system-list* nil)
(defvar *result-counter* nil)
(defvar *current-result-file* nil)

(defun t-test-some (input-file systems &optional (data-item 'test-accuracy) (tails 2))
  (t-test-file input-file tails data-item systems))

(defun t-test-file (input-file &optional (tails 2) (data-item 'test-accuracy) systems)
  "Runs paired t-test on all pairs of systems in a universal data file for the given data item
   (some item from *data-format*)"
   (unless (equal *current-result-file* (probe-file input-file))
     (setf *result-assoc-list* nil *system-list* nil *result-counter* 1 *current-result-file* (probe-file input-file))
     (with-open-file (stream input-file :direction :input)
       (setf *system-list* (read stream))
       (let (run)
	 (setf run (read stream))
	 (setf *result-assoc-list* (mapcar #'(lambda (point) (list (first point) (rest point)))
					   run))
	 (loop (if (null (setf run (read stream nil nil)))
		   (return nil)
		   (progn (incf *result-counter*)
			  (dolist (point run)
			    (let ((point-list (assoc (first point) *result-assoc-list*)))
			      (if point-list
				  (push (rest point) (rest point-list))
				  (error "Unknown point: ~A" (first point)))))))))))
    (format t "~%Total number of samples in data: ~D" *result-counter*)
    (dolist (pair (form-pairs (or systems *system-list*)))
      (format t "~%~%Comparing ~A to ~A:~%" (first pair) (second pair))
      (dolist (point-list *result-assoc-list*)
	(format t "~%For ~A training examples:" (first point-list))
	(paired-t-test (mapcar #'(lambda (set)
				   (list (get-field (get-field set (first pair) *system-list*)
						   data-item *data-format*) 
					 (get-field (get-field set (second pair) *system-list*)
						   data-item *data-format*)))
			       (rest point-list))
		       tails (first pair) (second pair)))))
	
(defun form-pairs (list)
  "Returns a list of all pairs of items in list"
  (if (null list)
      nil 
      (append (mapcar #'(lambda (x) (list (first list) x))
		      (rest list))
	      (form-pairs (rest list)))))

;;;; -------------------------------------------------------------------------------------------------
;;;;   Basic t-test
;;;; -------------------------------------------------------------------------------------------------

(defun paired-t-test (pairs &optional (tails 1) system1-name system2-name (confidence-interval-level .05))
  "Runs t-test for paired values.  Input of the form:
   ((system1-value system2-value) (system1-value system2-value) ...)"
  (let* ((N (length pairs))
	 (X-mean (/ (reduce #'+ (mapcar #'first pairs)) N))
	 (X-SD  (standard-deviation (mapcar #'first pairs)))
	 (Y-mean (/ (reduce #'+ (mapcar #'second pairs)) N))
	 (Y-SD  (standard-deviation (mapcar #'second pairs)))
	 (sum-D (reduce #'+ (mapcar #'(lambda (pair)
					(- (first pair) (second pair)))
				    pairs)))
	 (sum-D-squared (reduce #'+ (mapcar #'(lambda (pair)
						(expt (- (first pair) (second pair)) 2))
					    pairs)))
	 (Sd (sqrt (/ (- sum-d-squared (/ (expt sum-D 2) N)) (- N 1))))
	 (Sd-mean (/ Sd (sqrt N)))
	 (t-score (if (= X-mean Y-mean)
		      0
		      (if (zerop Sd-mean)
			  (if (> X-mean Y-mean)
			      most-positive-fixnum
			      most-negative-fixnum)
			  (/ (- X-mean Y-mean) Sd-mean))))
	 (df (- N 1))
	 (sig-level (t-significance-level t-score df tails))
         (sig-value (nth (if (<= df 50) (1- df) 49)
			 (second (assoc  (if (= tails 1)
					   confidence-interval-level
					   (/ confidence-interval-level 2))
				       *t-one-tail-sig-values*))))
         (slop (* Sd-mean sig-value)))
    (format t "~%Analysis of ~A mean = ~,3F (sd ~,3F) versus ~A mean = ~,3F (sd ~,3F).~%Difference: ~,3F~A"
	    (or system1-name "System 1") X-mean X-SD (or system2-name "System 2") Y-mean Y-SD
	    (- X-mean Y-mean) (if (and sig-level (<= sig-level .05)) "*" "" ))
    (format t "~%Paired t-test results: t = ~,3F, df = ~A" t-score df)
    (if (and sig-level (<= sig-level .05))
        (format t " (significant at the ~F level for ~D-tailed test)" sig-level tails)
	(format t " (not significant for ~D-tailed test)" tails))
    (format t "~%The ~F level confidence interval on the difference between the means: ~,3F to ~,3F~%"
            confidence-interval-level (- X-mean Y-mean slop) (- X-mean Y-mean (- slop)))))

(defun standard-deviation (numbers)
  "Compute the std dev of these numbers."
  (std-dev (reduce #'+ (mapcar #'sqr numbers)) (reduce #'+ numbers) (length numbers)))

(defun std-dev (sum-of-squares sum-of-values number-of-values)
  "Determine the standard deviation from these components."
  (sqrt (- (/ sum-of-squares number-of-values) (sqr (/ sum-of-values number-of-values)))))

(defun sqr (n) (* n n))

(defun t-significance-level (t-score df tails)
  "Returns the highest confidence level (lowest alpha) at which t-score is significant
   Returns NIL if not significant at any level in the table"
    (dolist (t-list *t-one-tail-sig-values* nil)
      (if (>= (abs t-score) (nth (if (<= df 60) (1- df) 59) (second t-list)))
	  (return (if (= tails 1) (first t-list) (* 2 (first t-list)))))))

(defun t-test-lists  (sys1 sys2)
  (paired-t-test (mapcar #'(lambda (val1 val2) (list val1 val2))
			 (eval sys1) (eval sys2))
		 2 sys1 sys2))

