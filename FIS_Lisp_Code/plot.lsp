;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; --- Uses gnuplot to report results.  Multiple lines are plotted in the
;;; most obvious way.  The data for the lines has to arrive in a list
;;; of lists, each of which represents one line.  Each line sublist
;;; contains is: (quoted-title [x-values] y-values).  [If x values aren't
;;; given then just the y's are dumped to the tmp file for gnuplot, meaning
;;; that x will be assumed to be sequential from 1.  Unfortunately, gnuplot
;;; is moronic and requires extensive stupid manipulations in order to do
;;; multiple-line plots in the same graph.  yrange, if given, is a string
;;; that will get sent directly to the set yrange command.  It should 
;;; look like: "[0:10]" or whatever.

(defun plot1 (r &optional xlabel yrange)
  ;; First push out each list 
  (with-open-file (f (/tmp "fis.plotemp")
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (dotimes (i (length (cadar r)))
      (dotimes (j (length r))
	(dotimes (k 2)
          (format f "~a " (nth i (nth (1+ k) (nth j r))))
	  ))
      (format f "~%"))
    )
  ;; now form the silly command line for gnuplot.
  (with-open-file (f (/tmp "fis.plotcmds")
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (if xlabel (format f "set xlabel \"~a\"~%" xlabel))
    (if (and yrange (not (eq 'auto yrange)))
	(format f "set yrange ~a~%" yrange))
    (format f "set data style lines~%")
    (format f "plot ")
    (dotimes (i (length r))
      (format f "\"~a\" using ~a:~a title \"~a\""
	      (/tmp "fis.plotemp")
	      (1- (* 2 (1+ i)))
	      (* 2 (1+ i))
	      (car (nth i r))
	      )
      (if (< (1+ i) (length r)) (format f ", "))
      )
    (format f "~%pause 1000")
    )
  (system! "gnuplot ~a &" (/tmp "fis.plotcmds"))
  )

;;; Makes ANOVA interaction plots given the four means: a1b1 a1b2 a2b1 and a2b2.
;;; We just use zero and 1 as the x axis level labels.

(defun anova-plot (ma1b1 ma1b2 ma2b1 ma2b2 &optional xlabel)
  ;; First push out each list 
  (with-open-file (f (/tmp "fis.plotemp1")
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (format f "0 ~a~%" ma1b1)
    (format f "1 ~a~%" ma1b2)
    )
  (with-open-file (f (/tmp "fis.plotemp2")
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (format f "0 ~a~%" ma2b1)
    (format f "1 ~a~%" ma2b2)
    )

  ;; now form the command line for gnuplot.
  (with-open-file (f (/tmp "fis.plotcmds")
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (if xlabel (format f "set xlabel \"~a\"~%" xlabel))
    (format f "plot \"~a\" with lines, \"~a\" with lines"
	    (/tmp "fis.plotemp1")
	    (/tmp "fis.plotemp2"))
    (format f "~%pause 100")
    )
  (system! "gnuplot ~a &" (/tmp "fis.plotcmds"))
  )

;;; This is a simple plot utility that packs a bunch of values for
;;; plot1.  It isn't actually called anyplace but is useful for
;;; debugging.

(defun plot* (y &aux x)
  (plot1 (list (list " " (dotimes (k (length y) (reverse x)) (push k x)) y))))

