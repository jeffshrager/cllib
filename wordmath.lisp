; (load (compile-file "wordmath.lisp"))

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

(defvar *ngram->count* (make-hash-table :test #'equal))

(defun make-ngram-table (file)
  (clrhash *ngram->count*)
  (with-open-file 
   (i file)
   (loop for n from 1 by 1
	 as word = (read-line i nil nil)
	 until (null word)
	 do (ngcount word)
	 (if (zerop (mod n 1000)) (format t "~a~%" (cons n word)))))
  (dd)
  )

(defun seedict (&optional (limit *min-score*))
  (loop for ngram being the hash-keys of *ngram->count*
	using (hash-value count)
	when (>= count limit)
	collect (cons count ngram)))

(defun ngcount (word)
  (mapcar #'(lambda (ng) (incf (gethash ng *ngram->count* 0))) (ngrams word)))

(defparameter *ngram-length* 5)

(defun ngrams (words)
  (loop for w+ on words
	as i from 1 to (1+ (- (length words) *ngram-length*))
	append (loop for j from 1 to *ngram-length*
		 collect (first-n j w+))))

 (defun dd ()
   (loop for i from 1 to 1000
	 for (k . ng) in 
	 (sort (loop for ngram being the hash-keys of *ngram->count*
		    using (hash-value count)
		    collect (cons count ngram))
	      #'> :key #'car)
	do (print (cons k ng))))

(defparameter *abc* "abcdefghijklmnopqrstuvwxyz")

(defun all-words-filtered (&optional (limit *min-score*))
  (setq *min-score* limit)
  (let (ws)
    (loop for c1 across *abc*
	  do (loop for c2 across *abc*
		   do (loop for c3 across *abc*
			    do (loop for c4 across *abc*
				     do (loop for c5 across *abc*
					      as w = (format nil "~c~c~c~c~c" c1 c2 c3 c4 c5)
					      when (normal? w)
					      do (push (print w) ws))))))
    ws))

(defun normal? (words)
  (loop for ng in (ngrams words)
	as score = (gethash ng *ngram->count*)
	when (or (null score) (< score *min-score*))
	do (return nil)
	finally (return t)))

;(untrace)
;(make-ngram-table)
(all-words-filtered 0)
