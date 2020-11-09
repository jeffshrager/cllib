;;; (load (compile-file "numbers.lisp"))

(declaim (optimize (debug 3)))

(defun concat (a b) (parse-integer (format nil "~a~a" a b)))
(defun root (a b) (expt a (/ 1 b)))
(defun /? (a b) (if (zerop b) :nan (/ a b)))
(defun fact (n) (if (< n 1) 1 (* n (fact (1- n)))))

(defparameter *2ops* '(* + - / root #\|))
(defparameter *1ops* '(identity fact -))
(defvar *paths* nil)


(defun run (l)
  (setq *paths* nil)
  (run2 l)
  *paths*)

(defun run2 (l &optional path)
  (cond
   ((null (cdr l))
    (loop for op in *1ops* do (push (cons op (car l)) *paths*)))
   (t (loop for op in *2ops*
	    do 

(push (cons (list op (car l) (cadr l)) path) *paths*)))
   ))
	    

(trace run)
(mapcar #'print (run '(1 2 3)))
