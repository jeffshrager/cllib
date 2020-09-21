;;; -*- Syntax: Common-Lisp; Base: 10; Mode: LISP -*-

;;;; Copyright (c) 1992, 1993, 1994, 1995, 1996, 1997 by Paul Eric Nielsen.
;;;; The following software was developed by Paul Eric Nielsen for his
;;;; exclusive use.  This software, or excerpts thereof, may be freely
;;;; redistributed, in any medium, provided that you conspicuously and
;;;; appropriately publish on each copy an appropriate copyright notice.
;;;; It may not be sold for profit or incorporated in commercial documents
;;;; (e.g., published for sale on CD-ROM, floppy disks, books, magazines,
;;;; or other form) without the prior written permission of the copyright
;;;; holder.  This software is provided AS IS without any expressed or
;;;; implied warranty.

(defvar *copyright* 
"Copyright (c) 1992, 1993, 1994, 1995, 1996, 1997 by Paul Eric Nielsen.
The following software was developed by Paul Eric Nielsen for his
exclusive use.  This software, or excerpts thereof, may be freely
redistributed, in any medium, provided that you conspicuously and
appropriately publish on each copy an appropriate copyright notice.
It may not be sold for profit or incorporated in commercial documents
(e.g., published for sale on CD-ROM, floppy disks, books, magazines,
or other form) without the prior written permission of the copyright
holder.  This software is provided AS IS without any expressed or
implied warranty.")


;;;------------------------------------------------------------------------
;;;
;;; Memoization
;;;
;;;------------------------------------------------------------------------

;; The ability to memoize functions is severely impared by tail
;; recursion optimization.  This macro replaces the defun, function
;; definition, and rehacks the body of the function to insert
;; memoization.

(defmacro defmemo (name (&rest args) &rest body)
  "Memoizes a function.  Successive calls to this function will first
  check to see if the requested value has already been computed.  If
  so that value is returned from storage, if not the value is computed
  and stored.  This can provide a tremendous speedup at the expense of
  additional memory."
  (flet ((declarationp (form)
            "Is form a declaration?"
            ;; I'm not sure all of these are really legal at the start
            ;;of a defun, but the compiler will catch it later.
            (member (car form)
                    '(declare declaim locally proclaim declaim)))
         (doc-stringp (form)
            "Is the form a documentation string?"
            (stringp form))
         )
    
    (let ((doc-or-decl                  ; Documentation strings and declarations
           (loop for b on body
                 while (or (doc-stringp (car b))
                           (declarationp (car b)))
                 collect (car b)         ; Collect docs & decls
                 finally (setf body b))) ; Strip docs & decls off body
          (index (car args))             ; Index to the hash table
          ;; Gensym new names to prevent some idiot from using them
          (memo (gensym))                ; Storage for previous results
          (hashindex (gensym))           ; Hash index
          )
      ;; Cons all the arguments together for the hash index
      (dolist (arg (cdr args))
        (setf index `(cons ,arg ,index)))

      ;; Template for resultant memoized form follows
      `(let ((,memo (make-hash-table :test #'equal))) ; Storage for results
         (defun ,name ,args
           ,@doc-or-decl                ; Docs & decls at function start
           (let ((,hashindex ,index))
             (multiple-value-bind (value found)
                 (gethash ,hashindex ,memo) ; Look for a previous result
               (if found
                   value                ; Return previous value
                 (setf (gethash ,hashindex ,memo) ; Save new result
                       (block ,name     ; Handle "return-from" so that it saves
                         ,@body         ; Compute new value
                         ))))))
         )                              ; End of template
      )))
                            
       

;;;------------------------------------------------------------------------
;;;
;;; Queue management
;;;
;;;------------------------------------------------------------------------

;; The queues are utilities needed by some of the combinatorial
;; functions

(defstruct queue
  (elements nil)
  last)

(defun create-queue (element)
  "Create a queue with an initial element"
  (let ((queue (make-queue)))
    (enqueue element queue)
    queue))

(declaim (inline queue-emptyp))
(defun queue-emptyp (queue)
  "Returns non-NIL if the queue is empty"
  (null (queue-elements queue)))

(defun enqueue (element queue)
  "Add an element to a queue"
  (enqueue-list (list element) queue)
  element)

(defun enqueue-list (list queue)
  "Add a list of elements to a queue"
  (cond ((null (queue-elements queue))
         (setf (queue-elements queue) list))
        (T (setf (cdr (queue-last queue)) list)))
  (setf (queue-last queue) (last list))
  list)

(defun dequeue (queue)
  "Pop the next element from the queue"
  ;; In keeping with Common Lisp paradigm, dequeueing from an empty
  ;; queue returns NIL
  (pop (queue-elements queue)))

(defun top-queue (queue)
  "Return the next element from the queue without changing the queue"
  ;; In keeping with Common Lisp paradigm, the top of an empty
  ;; queue returns NIL
  (car (queue-elements queue)))



;;;------------------------------------------------------------------------
;;;
;;; Combinations
;;;
;;;------------------------------------------------------------------------


#|
 Some timings for the combination functions on a SPARC 10 using Allegro CL
        Call                                          CPU time / Cons Cells
(with-count (comb1 '(a b c d e f g h i j k l m n o p) 8)) 950     218792
(with-count (comb2 '(a b c d e f g h i j k l m n o p) 8)) 900     205922
(with-count (comb3 '(a b c d e f g h i j k l m n o p) 8)) 984     284327
(with-count (comb5 '(a b c d e f g h i j k l m n o p) 8)) 517     115848
|#

(defun comb1 (s n)
  "Given a set of elements, S, this forms all combinations of length N
  of the elements of S 
  Example:
       (comb1 '(A B C) 2)
          ;=> ((A B) (A C) (B C))
  "
  (cond ((zerop n) (list nil))
        ((null s) nil)
        (T (nconc (mapcar #'(lambda (c)
                              (cons (car s) c))
                          (comb1 (cdr s) (1- n)))
                  (comb1 (cdr s) n)))))

(defun comb2 (s n)
  "Another version of the same function as above.  The main difference
   is that the order of the nconc is reversed.
   Given a set of elements, S, this forms all combinations of length N
   of the elements of S.
   Example:
        (comb2 '(A B C) 2)
          ;=> ((B C) (A B) (A C))
   "
  ;; Doesn't work if n is initially 0, but that would be silly
  (cond ((= n 1) (mapcar #'list s))
        ((< (length s) n) nil)
        (t (nconc (comb2 (cdr s) n)
                  (mapcar #'(lambda (c)
                              (cons (car s) c))
                          (comb2 (cdr s) (1- n)))))))

(defun comb3 (s n)
  "Yet another version of the same function as above.  This one is
  more readable and eliminates one recursive call.
  Given a set of elements, S, this forms all combinations of length N
  of the elements of S 
  Example:
       (comb3 '(A B C) 2)
          ;=> ((A B) (A C) (B C))
  "
  (cond ((zerop n) (list nil))
        (t (loop for l on s
                 nconc (loop for c in (comb3 (cdr l) (1- n))
                             collect (cons (car l) c))))))

(defun fcomb (list n function)
  "Another version of the 'comb' function.  This one avoids consing by
  accepting an additional argument which is the function to be applied
  to each of the various combinations.
  The list passed to the function is destructively modified.  If you
  want to save it, you must copy it (avoiding the savings in cons cells).
  Example:
       (fcomb '(a b c) 2 <function>)
          ;=> will call <function> three times with the following arguments:
              (A B) (A C) (B C)
  "
  (let ((choice (make-list n)))
    ;; choices are items remaining to choose from
    ;; c-length is the length of choices
    ;; tail is the tail of choice that has yet to be filled in with valid
    ;;   values
    ;; t-length is the number of items to choose from choice, and should be
    ;;   = to the length of tail.
    (labels ((fcomb-1 (choices c-length tail t-length)
               (cond 
                ((null tail) (funcall function choice))
                ((>= c-length t-length)
                 (loop for l on choices
                       for i from c-length downto t-length
                       do (setf (car tail) (car l))
                       do (fcomb-1 (cdr l) (1- i) (cdr tail) (1- t-length))))))) 
      (fcomb-1 list (length list) choice n))))

(defun comb5 (list n)
  "This passes a collector function to 'fcomb' which illustrates the
  use of 'fcomb' and yields exactly the same behavior as the other
  'comb' functions.
  Example:
       (comb5 '(A B C) 2)
          ;=> ((B C) (A C) (A B))
  "
  (let ((choices nil))
    (fcomb list n #'(lambda (choice)
                       (push (copy-list choice) choices)))
    choices))


(defun comb&left1 (list n function &optional destructive)
   "Yet another version of the 'combination' function.  This one
   avoids consing by accepting an additional argument which is the
   function to be applied to the various combinations.  The passed
   function should accept -two- arguments the first is the set of
   combinations and the second is the set-difference between this set
   and the original `list'
   Example:
        (comb&left1 '(a b c) 2 <function>)
        ;=> will call <function> three times with the following arguments:
            (A B) (C)
            (A C) (B)
            (B C) (A)
  "
  (let ((c-list (if destructive list (copy-list list)))
        (len (length list)))
    ;; c-list is a copy of the list of items.  C-list will be
    ;; destructively modified during the course of execution; however,
    ;; when the function completes c-list will be back in its original
    ;; order.  The bold user who is concerned with space economy may
    ;; dispense with the copy list by setting `destructive' to T
    ;; c-length is the length of choices remainding
    ;; t-length is the number of items to choose
    ;; current is a list, the car of which is the current position
    ;; to be filled
    (labels ((c&l1 (choices current c-length t-length)
               (cond 
                 ((zerop t-length)
                  ;; The first n elements of c-list contain the
                  ;; current combination
                  (setf (cdr (nthcdr (1- n) c-list)) NIL) ; Split list
                  (funcall function c-list current) ; <- Solution here
                  (setf (cdr (nthcdr (1- n) c-list)) current)) ; Restore split
                 (T
                  (loop for l on choices
                   for i from c-length downto t-length
                   do (rotatef (car current) (car l)) ; Swap
                   do (c&l1 (cdr l) (cdr current) (1- i) (1- t-length))
                   do (rotatef (car current) (car l)) ; Unswap
                   )))))
      (when (>= len n)
        (c&l1 c-list c-list len n)))))


(defun comb&left2 (list n function)
   "Yet another version of the 'combination' function.  This one
   avoids consing by accepting an additional argument which is the
   function to be applied to the various combinations.  The passed
   function should accept -two- arguments the first is the set of
   combinations and the second is the set-difference between this set
   and the original `list'
   Example:
        (comb&left2 '(a b c) 2 <function>)
        ;=> will call <function> three times with the following arguments:
            (A B) (C)
            (A C) (B)
            (B C) (A)
  "
   ;; This function is destructive on the list, but when it finished
   ;;the list will be restored to its original order.
   ;; If you wish to avoid destroying the list, copy it before
   ;;anything else.  The function depends on "list" and "left"
   ;;sharing elements.
   (let ((comb (subseq list 0 n))       ; Stores the combinations
         (left (nthcdr n list)))        ; Stores the remainder
     (labels ((c&l1 (fill choices)
                ;; Fill is the set of slots in the solution yet to be filled
                ;; Choices is the set of elements which may be chosen
                ;;to fill them.
                ;; Note: choices shares structure with left, so pulling
                ;;an element out of choices, also pulls it out of left
                (if (null fill)         ; No more slots to fill
                    (funcall function comb left) ; <- Solution here
                  (loop for c on choices
                        do (rotatef (car fill) (car c)) ; Swap elements
                        do (c&l1 (cdr fill) (cdr c))
                        do (rotatef (car fill) (car c))) ; Unswap
                  )))
       (c&l1 comb list))))


(defun gen-next-comb (s n &optional final-val)
  "Given a set of elements, S, this forms all combinations of length N
  of the elements of S 
  Example:
       (gen-next-comb '(A B C) 2)
          ;=> (A B) #'F
              calling F will return (A C) #'F
              calling F will return (B C) #'F
  After all combinations have been generated successive calls to the
  continuation function will return the `final-val' which is NIL by
  default"
  (let ((comb-index (loop for i below n   ;contains indices into S.
                     collect i))
        (len-s (length s)))
    (labels ((fi () (values final-val #'fi)) ;Final values
             (ci ()
               (do ((i (1- n) (1- i))
                    (max len-s (1- max)))
                   ((minusp i) (fi))    ;No more solutions
                 (when (< (incf (nth i comb-index)) max) ;Solution
                   ;;Reset following indices
                   (loop for l on (nthcdr i comb-index)
                         for amt from (nth i comb-index)
                         do (setf (car l) amt))
                   (return (values (loop for elm in comb-index
                                    collect (nth elm s))
                                   #'ci))))))
      (if (<= 0 n len-s)
          (values (loop for elm in comb-index
                     collect (nth elm s))
                  #'ci)
          (fi)))))


;;;------------------------------------------------------------------------
;;;
;;; Numeric combinations  (same as "binomial" defined below)
;;;
;;;------------------------------------------------------------------------

#|
 Some timings for the choose functions on a SPARC 10 using Allegro CL
        Call                      CPU time / Cons Cells / Other memory
(with-count (choose1 200 100))     21634       535350       2276688
(with-count (choose2 200 100))        50          371         14712
(with-count (choose3 200 100))        17          269         11544 
(with-count (choose4 200 100))        17            2          9112
For comparison, in about the same time as choose1 takes you can do:
(with-count (choose4 10000 5000))  38333            2       9304208
|#

(defmemo choose1 (n k)
  "Example:
        (choose1 10 2)
           ;=> 45
        (choose1 100 10)
           ;=> 17310309456440
  "
    (declare (fixnum n k))
    (if (or (= k 0) (= n k))
        1
      (+ (choose1 (- n 1) k)
         (choose1 (- n 1) (- k 1)))))

(defmemo fact (n)
    "Factorial (What did you expect?)
    Example:
         (factorial 5)
           ;=> 120
    "
  (if (<= n 1)
      1
    (* n (fact (1- n)))))

;; Alternative which leads to very large integer numbers
(defun choose2 (n k)
  "Binomial coefficient
  Example:
       (choose2 4 2)
         ;=> 6
  "
  (when (<= k n)
    (/ (fact n) 
       (fact k) (fact (- n k)))))

(defun sub-factorial (n k)
  "Sub-sequence of factorial, until n reaches k.
 This is equivalent to n! / k!, when n > k.
 "
  (if (<= n k)
      1
    (* n (sub-factorial (1- n) k))))

;; Alternative still using integers.  Not as large numbers.
(defun choose3 (n k)
  "Binomial coefficient
  Example:
       (choose2 4 2)
         ;=> 6
  "
  (declare (fixnum n k))
  (when (<= k n)
    (setf k (min k (- n k)))
    (/ (sub-factorial n (- n k))
       (fact k))))

;; Using rationals.  Smaller numbers.
(defun choose4 (n k)
  "Example:
        (choose4 10 2)
           ;=> 45
        (choose4 100 10)
           ;=> 17310309456440
  "
  (declare (fixnum n k))
  (setf k (min k (- n k)))
  (let ((result 1))
    (dotimes (i k result)
      (setf result (* result (/ (- n i)
                                (1+ i))))
      )))
 
(defun choose4 (n k)
  "Example:
        (choose4 10 2)
           ;=> 45
        (choose4 100 10)
           ;=> 17310309456440
  "
  (declare (fixnum n k))
  (setf k (min k (- n k)))
  (let ((result 1))
    (loop for a downfrom n
          for b upfrom 1 to k
          do (setf result (* result (/ a b))))
    result))
 


;;;------------------------------------------------------------------------
;;;
;;; Cartesian products
;;;
;;;------------------------------------------------------------------------


#|
 Some timings for the cartesian functions on a SPARC 10 using Allegro CL
        Call                         CPU time / Cons Cells
(with-count (cartesian1 *testl*))    183484       111987
(with-count (cartesian2 *testl*))       466       121311
(with-count (cartesian3 *testl*))       400       121317
(with-count (cartesian4 *testl*))       250       111980

(setf *testl* (loop for i below 6
                    collect '(a b c d e f)))
|#


(defun cartesian1 (sets)
  "Returns the Cartesian product of a list of sets
  Example:
       (CARTESIAN1 '((A B) (C D) (E F)))
         ;=> ((A C E) (B C E) (A D E) (B D E) (A C F) (B C F) (A D F) (B D F))
  "
  (cond ((null sets) (list nil))
        (t (mapcan #'(lambda (cartesian)
                       (mapcar #'(lambda (elmt)
                                   (cons elmt cartesian))
                               (car sets)))
                   (cartesian1 (cdr sets))))))

(defun cartesian2 (sets)
 "Returns the Cartesian product of a list of sets
  Example:
       (CARTESIAN2 '((A B) (C D) (E F)))
         ;=> ((A C E) (B C E) (A D E) (B D E) (A C F) (B C F) (A D F) (B D F))
  Same as cartesian1, but uses loop instead of mapping.
  "
  (cond ((null sets) (list NIL))
        (t (loop for cartesian in (cartesian2 (cdr sets))
                 nconc (loop for elmt in (car sets)
                             collect (cons elmt cartesian))))))


(defun cartesian3 (sets)
 "Returns the Cartesian product of a list of sets
  Example:
       (CARTESIAN3 '((A B) (C D) (E F)))
         ;=> ((A C E) (B C E) (A D E) (B D E) (A C F) (B C F) (A D F) (B D F))
  Non-recursive version.
  "
 (reduce #'(lambda (cartesian set)
             (loop for cart in cartesian
                   nconc (loop for elmt in set
                               collect (cons elmt cart))))
         (reverse sets)                 ; For esthetics
         :initial-value (list NIL)))

(defun cartesian4 (sets)
 "Returns the Cartesian product of a list of sets
  Example:
       (CARTESIAN4 '((A B) (C D) (E F)))
         ;=> ((B C F) (B C E) (B D F) (B D E) (A C F) (A C E) (A D F) (A D E))
  Non-recursive version.
  "
 (let ((cartesian (list nil)))
   (dolist (set (reverse sets))         ; reverse is only for esthetics
     (let ((new-cartesian nil))
       (dolist (elmt set)
         (dolist (cart cartesian)
           (push (cons elmt cart) new-cartesian)))
       (setf cartesian new-cartesian)))
   cartesian))



;;;------------------------------------------------------------------------
;;;
;;; Partition sets
;;;
;;;------------------------------------------------------------------------

#|
Some timings for the partition functions on a SPARC 10 using Allegro CL
        Call                                     CPU time / Cons Cells
(with-count (partition1 '(a b c d e f g h i)))     750       139972
(with-count (partition2 '(a b c d e f g h i)))    5700       182260
(with-count (partition3 '(a b c d e f g h i)))     467       139968
(with-count (partition4 '(a b c d e f g h i)))   35750       147772
(with-count (partition5 '(a b c d e f g h i)))    1083       188790
(with-count (partition6 '(a b c d e f g h i)))    5350       176958
(with-count (partition7 '(a b c d e f g h i)))     616       134667
|#

(defun partition1 (set)
  "Finds all the partitions of the input <set>, returning them in the
  obvious canonical format.
  Example:
       (partition1 '(a b c))
         ;=> (((A) (B) (C)) ((A B) (C)) ((A C) (B)) ((A) (B C)) ((A B C)))
  "
  (cond ((null set) (list nil))
        (t (let ((first-elem (car set)))
             (loop for part in (partition1 (cdr set))
                   nconc (cons 
                          (cons (list first-elem) part)
                          (loop for p on part
                                collect (nconc (list (cons first-elem (car p)))
                                               (ldiff part p)
                                               (cdr p)))))))))


; partition1a marginally faster and uses less consing that partition1,
; but the resulting order is strange.
;
; (defun partition1a (set)
;   "Finds all the partitions of the input <set>, returning them in the
;   obvious canonical format.
;   Example:
;        (partition1 '(a b c))
;          ;=> (((A C) (B)) ((A B) (C)) ((A) (B) (C)) ((A B C)) ((A) (B C)))
;   "
;   (cond ((null set) (list nil))
;         (t (let ((first-elem (car set))
;                  (new-partition nil))
;              (dolist (part (partition1a (cdr set)))
;                (push (cons (list first-elem) part)
;                      new-partition)
;                (do ((p part (cdr p)))
;                    ((null p))
;                  (push (nconc (list (cons first-elem (car p)))
;                               (ldiff part p)
;                               (cdr p))
;                        new-partition)))
;              new-partition))))

(defun partition2 (set)
  "Compute all partitions of a general n-element set
  Example:
       (partition2 '(a b c))
         ;=> (((A) (B) (C)) ((A B) (C)) ((B) (A C)) ((A) (B C)) ((A B C)))
  "
  (cond ((null (cdr set)) (list (list set))) ; set length is 1
        (t (let ((first-elem (car set)))
             (loop for part in (partition2 (cdr set))
                   nconc (cons
                          (cons (list first-elem) part)
                          (loop for p in part
                                collect (subst (cons first-elem p)
                                               p
                                               part))
                          ))))))


(defun partition3 (set)
  "Compute all partitions of a general n-element set
  Example:
       (partition3 '(a b c))
         ;=> (((A) (B) (C)) ((A B) (C)) ((A C) (B)) ((A) (B C)) ((A B C)))
  The only difference between this and partition2 is the explicit
  handling of the two halves of the list (before and after) rather
  than using subst, but what a difference in efficiency.
  "
  (cond ((null (cdr set)) (list (list set)))
        (t (let ((first-elem (car set)))
             (loop for part in (partition3 (cdr set))
                   nconc (cons
                          (cons (list first-elem) part)
                          (loop for before = nil then (cons p before)
                                for after = (cdr part) then (cdr after)
                                for p in part
                                collect (cons (cons first-elem p)
                                              (append before after)))
                          ))))))
             


(defun partition4 (set)
  "Compute all partitions of a general n-element set
  Example:
       (partition4 '(a b c))
         ;=> (((A) (B) (C)) ((A) (B C)) ((A B) (C)) ((B) (A C)) ((A B C)))
  "
  (labels ((amplify (first-elem subset)
                    (when subset 
                      (let ((first-sub-elem (car subset))
                            (partition (amplify first-elem (cdr subset))))
                        (cons (cons (cons first-elem first-sub-elem)
                                    (cdr subset))
                              (mapcar #'(lambda (part)
                                          (cons first-sub-elem part))
                                      partition))))))

    (cond (set (let ((first-elem (car set))
                     (partition (partition4 (cdr set))))
                 (nconc (mapcar #'(lambda (part)
                                    (cons (list first-elem) part))
                                partition)
                        (mapcan #'(lambda (part)
                                    (amplify first-elem part))
                                partition))))
          (T (list nil))
          )))


; Original form
; (defun fill-each-one (partition item)
;   "fill-each-one takes a partition and an item and returns a list of 
;    partitions. If the input partition has m sublists, a list of m partitions
;    is returned. Each partition in the returned list is the same as the
;    input partition, except that the item has been added to one of the
;    sublists.
;    Example:
;         (fill-each-one '((B C) (D)) 'A)
;           ;=> (((A B C) (D)) ((B C) (A D)))
;    "
;   (do ((partition-list (mapcar #'(lambda (subset)
;                                    (copy-tree partition)) ; Anal
;                                partition))
;        (k (1- (length partition)) (1- k))
;        focus)
;       ((minusp k) partition-list)
;     (setq focus (nth k (nth k partition-list)))
;     (push (car focus) (cdr focus))
;     (setf (car focus) item)))


(defun fill-each-one (partition item)
  "fill-each-one takes a partition and an item and returns a list of 
   partitions. If the input partition has m sublists, a list of m partitions
   is returned. Each partition in the returned list is the same as the
   input partition, except that the item has been added to one of the
   sublists.
   Example:
        (fill-each-one '((B C) (D)) 'A)
          ;=> (((A B C) (D)) ((B C) (A D)))
   "
  (loop for part on partition
        collect (nconc (ldiff partition part)
                       (list (cons item (car part)))
                       (cdr part))))

(defun partition-into-n (set how-long how-many)
  "partition-into-n returns all partitions of a set into how-many
   subsets. The length of set must be equal to how-long.
   Let A be the first element of set.
   Observe that all partitions of set into how-many subsets may be
   divided into two groups: those partitions in which A appears by itself
   in a subset, and those partitions of in which A does not appear
   by itself. The function finds those two groups and concatenates them.
   The first group is formed by finding all the partitions of the rest
   of set into (how-many minus 1) subsets and adding (A) to each such
   partition.

   The second group is formed by finding all the partitions of the rest
   of set in how-many subsets and, for each partition P, creating how-many
   new partitions by sticking A into each subset of P, one at a time
   and concatenating the resulting partitions together.
   "
  (cond ((eq how-many 1) (list (list set)))
        ((eq how-many how-long) (list (mapcar #'list set)))
        (t
         (nconc 
          (loop for partition in (partition-into-n
                                  (rest set) (1- how-long) (1- how-many))
                collect (cons (list (first set)) partition))
          (loop for partition in (partition-into-n 
                                  (rest set) (1- how-long) how-many)
                nconc (fill-each-one partition (first set)))))))
 
(defun partition5 (set)
  "Returns all partitions of a set into subsets.
  It concatenates all the partitions of set with n subsets.
  Example:
       (partition5 '(a b c))
         ;=> (((A B C)) ((A) (B) (C)) ((A) (B C)) ((A B) (C)) ((B) (A C)))
  "
  (do* ((how-long (length set))
        (how-many how-long (1- how-many))
        (answer (list (list set))))
       ((eq how-many 1) answer)
    (nconc answer (partition-into-n set how-long how-many))))


(defun partition6 (set)
  "Given a list of items, return all possible partitions, i.e. a list of
  list of lists.  Non-recursive.
  Example:
       (partition6 '(a b c))
         ;=> (((B) (C A)) ((C B) (A)) ((C) (B) (A)) ((C B A)) ((C) (B A)))
  "
  (labels ((adjust (elmt partition)
             "Adjust list of partitions to take into account new element."
             (if (null partition)
                 (list (list (list elmt)))
               (let ((new-partition nil))
                 (dolist (part partition)
                   ;; case 1: new element as singleton set
                   (push (cons (list elmt) part)
                         new-partition)
                   ;; case 2: new element as member of some subset in
                   ;; a partition
                   (dolist (p part)      
                     (push (subst (cons elmt p) p part)
                           new-partition)))
                 new-partition))))
    (let ((result nil))
      (dolist (item set)
        (setq result (adjust item result)))
      result)))


(defun partition7 (set)
  "Given a list of items, return all possible partitions, i.e. a list of
  list of lists.  Non-recursive.
  Example:
       (partition7 '(a b c))
         ;=> (((B) (C A)) ((C B) (A)) ((C) (B) (A)) ((C B A)) ((C) (B A)))
  "
  ;;Esthetic note:  To have the items within sets come out ordered
  ;;(e.g., (A B C) rather than (C B A)), reverse the initial set
  (flet ((adjust (partition elmt)
             "Adjust list of partitions to take into account new element."
             (let ((new-partition nil))
               (dolist (part partition new-partition)
                 ;; case 1: new element as singleton set
                 (push (cons (list elmt) part)
                       new-partition)
                 ;; case 2: new element as member of some subset in a
                 ;; partition
                 (do ((p part (cdr p)))
                     ((null p))
                   (push (nconc (ldiff part p)
                                (list (cons elmt (car p)))
                                (cdr p))
                         new-partition))))
             ))
    (reduce #'adjust
            set
            :initial-value (list nil))))

;; K-PARTITIONS:
;; Thomas Trenz
;; mail: tr...@dfki.uni-sb.de
;; 
;;; The function k-partition enumerates k-partitions of a given set and
;;; calls a user defined function for every partition.
;; The enumeration stops when the user defined function returns a non-nil value.
;; 
;; The user defined function gets two parameters:
;; 
;; 1. The partition as a list of lists (e.g. ((a b) (c) (d e f)) for a 3-partition
;;    of the set (a b c d e f)).
;; 2. A list of additional arguments (optional).

(defun k-partition-help (k stack partition function args)
  (when (and (>= (+ (length stack) (length partition)) k) 
             (<= (length partition) k))
    (cond ((null stack)
           (when (= (length partition) k)
             (funcall function partition args)))
          (t (let ((element (pop stack))
                   (temp-partition partition))
               (or (progn
                     (push (list element) temp-partition)
                     (k-partition-help k stack temp-partition function args))
                   (some #'(lambda (set)
                             (setq temp-partition partition)
                             (setq temp-partition
                                   (remove set temp-partition :test #'equal))
                             (push (union (list element) set) temp-partition)
                             (k-partition-help k stack temp-partition function args))
                         partition)))))))


(defun k-partition (set k function &optional (argforfunction nil))
  "Computes all k-partitions of a given set and calls for every
  k-partition a Function given by the parameter FUNCTION.  The
  enumeration of partitions stops, when FUNCTION return a non-nil
  value.
  Example:
       (k-partition '(a b c d) 3 <function>)
         ;=> will call <function> six times with the following
             arguments, or until function returns a non-nil value:
             ((D C) (B) (A)) <argforfunction>
             ((D B) (C) (A)) <argforfunction>
             ((D A) (C) (B)) <argforfunction>
             ((D) (C B) (A)) <argforfunction>
             ((D) (C A) (B)) <argforfunction>
             ((D) (C) (B A)) <argforfunction>
  "
  (k-partition-help k (cdr set) (list (list (car set))) function argforfunction))

;; Demo call:
;;
; (k-partition '(a b c d) 3 
;              #'(lambda (partition optional-args)
;                  (print partition)
;                  nil))                  ; return value nil to continue enumeration


;;;----------------------------------------------------------------------------
;;; Bell's number, the number of partitions of an n-element set
;; 
;;; Hans-Martin Adorf
;;; ST-ECF/ESO
;;; Karl-Schwarzschild-Str. 2
;;; D-85748 Garching b. Muenchen
;;; ad...@eso.org
;;;
;;; upon a suggestion by Mark McConnell <mmc...@math.okstate.edu>
;;;
;;; Hacked by Paul Nielsen
;;;----------------------------------------------------------------------------

(defun binomial (k n)
  "Binomial coefficient
  Example:
       (binomial 2 3)
         ;=> 3
  "
  (declare (fixnum n k))
  (let ((result 1))
    (dotimes (i k result)
      (setf result (* result (/ (- n i) (1+ i)))))))


#|
(defmemo factorial (n)
  "Factorial (What did you expect?)
  Example:
       (factorial 5)
         ;=> 120
  "
  (declare (fixnum n))
  (if (<= n 1)
      1
    (* n (factorial (1- n)))))


;; Alternative binomial which leads to very large integer numbers
(defun binomial (k n)
  "Binomial coefficient
  Example:
       (binomial 2 4)
         ;=> 6
  "
  (when (<= k n)
    (/ (factorial n) 
       (factorial k) (factorial (- n k)))))
|#

(defmemo bell (n)
  "Bell's number B(n), the number of partitions of an n-element set"
  (if (= n 0)
      1
    (loop for k below n
          sum (* (binomial k (1- n)) (bell k)))))

#|
(defun bell (n)
  "Bell's number B(n), the number of partitions of an n-element set"
  (if (= n 0)
      1
    (loop for k below n
          sum (* (binomial k (1- n)) (bell k)))))
|#


#|
(bell 0) => 1
(bell 1) => 1
(bell 2) => 2
(bell 3) => 5
(bell 4) => 15
(bell 5) => 52
(bell 6) => 203
(bell 20) => 51724158235372
;; And using the memoization forms, in less than a minute:
(bell 100) => 4758539127676483365879076884138720782636366968682561146661
              6334637559114497892442622672724044217756306953557882560751

 
|#


;;;------------------------------------------------------------------------
;;;
;;; Power sets
;;;
;;;------------------------------------------------------------------------

#|
 Some timings for the power set functions on a SPARC 10 using Allegro CL
        Call                                                CPU time / Cons Cells
(with-count (power-set1 '(a b c d e f g h i j k l m n o p q)))  900  262161
(with-count (power-set2 '(a b c d e f g h i j k l m n o p q)))  850  262161
(with-count (power-set3 '(a b c d e f g h i j k l m n o p q)))  900  262161
(with-count (power-set4 '(a b c d e f g h i j k l m n o p q))) 1683  393232
(with-count (power-set5 '(a b c d e f g h i j k l m n o p q))) 1733  393216
(with-count (power-set6 '(a b c d e f g h i j k l m n o p q))) 3750  262178
(with-count (power-set7 '(a b c d e f g h i j k l m n o p q))) 5466  524168
(with-count (power-set8 '(a b c d e f g h i j k l m n o p q)))17100 1376258
|#

; Without exception the f versions of these functions (which accept an
; additional argument which is a function) are substantially slower
; and cons more.  I haven't looked at them long enough to figure this out.

(defun power-set1 (s)
  "The obvious implementation of the power set function.  Recursive-version.
   Forms all possible subsets of a set of parameters.
   Example:
        (power-set1 '(a b c))
           ;=> ((A B C) (A B) (A C) (A) (B C) (B) (C) NIL)
   NIL is always a subset and always in the last position.
   The resulting order may be preferable for some purposes since
   supersets always preceed their subsets."
  (cond (s (let ((a (car s)) 
                 (b (power-set1 (cdr s))))
             (nconc (loop for x in b
                          collect (cons a x))
                    b)))
        (T (list nil)) ))

(defun power-set2 (set)
  "Forms all possible subsets of a set of parameters.  Non-recursive version.
   Example:
        (power-set2 '(a b c))
           ;=> (NIL (A) (B) (B A) (C) (C A) (C B) (C B A))
   NIL is always a subset and always in the CAR position.
   The resulting order may be preferable for some purposes since
   subsets always preceed their supersets."
  ;; The order is also natural for bit encoding.  If the elements are
  ;;represented as bits in a normal binary fashion , i.e., A ->1, B->2,
  ;;C->4, D->8 the Nth element will be the decoded bit representation of N,
  ;;i.e. the 6th element will be (B C), 7th->(A B C), 8th->(D), 9th->(A D)

  ;;Esthetic note:  To have the items within sets come out ordered
  ;;(e.g., (A B C) rather than (C B A)) use (append r (list s)) rather
  ;;than (cons s r), or reverse the original set
  (let ((res (list NIL)))
    (dolist (s set res)                                
      (nconc res (loop for r in res
                       collect (cons s r))))))

(defun power-set3 (set)
  "Similar to power-set2, but using reduce.
  Example:
        (power-set2 '(a b c))
           ;=> ((C B A) (C B) (C A) (C) (B A) (B) (A) NIL)
   The resulting order may be preferable for some purposes since
   supersets always preceed their subsets."
  (reduce #'(lambda (result new-el)
              (nconc (loop for result-elm in result
                           collect (cons new-el result-elm))
                     result))
          set
          :initial-value (list nil)))


(defun power-set4 (set &optional (maxsize (length set)))
  "Another implementations of the power set function.  This one
  allows an optional stipulation of the maximum size of a resulting
  set in the power-set.
  Example:
       (power-set4 '(a b c))
          ;=> (NIL (A) (B) (A B) (C) (A C) (B C) (A B C))
  "
  (if (null set)
      (list nil)
    (loop for entry in (power-set4 (cdr set) maxsize)
          collect entry
          when (< (length entry) maxsize)
            collect (cons (car set) entry))))

(defun power-set5 (set &optional aux-set)
  "Forms all possible subsets of a set of parameters.
   The optional argument is for subsequent recursive calls.
   Example:
        (power-set5 '(A B C)), 
           ;=> (NIL (A) (B A) (C B A) (C A) (B) (C B) (C))
   NIL is always a subset and always in the CAR position.
   To get the non-empty subsets take the CDR"
  (cons aux-set (loop for s on set
                      nconc (power-set5
                             (cdr s)
                             ;; To have the items within sets come
                             ;;out ordered (e.g., (A B C) rather
                             ;;than (C B A)) use 
                             ;;(append aux-set (list (car s)))
                             ;;rather than (cons (car s) aux-set)
                             (cons (car s) aux-set)))
        ))

(defun power-set6 (set)
  "Forms all possible subsets of a set of parameters.  Result is in
   descending order of set size.
   Example:
        (power-set6 '(A B C)), 
           ;=> ((A B C) (A B) (A C) (B C) (A) (B) (C) NIL)
  "
  (cond ((null set) (list nil))
        (T (let ((bottom (power-set6 (cdr set))))
             (merge 'list
                    (loop for subset in bottom
                          collect (cons (car set) subset))
                    bottom
                    #'(lambda (set1 set2)
                        (> (length set1) (length set2))))))))
                   

(defun power-set7 (set &optional (sort-function #'<))
  "Forms all possible subsets of a set of parameters.  Result is in
   an order of set size according to sort-function (ascending if #'<,
   descending if #'>).
   Example:
        (power-set7 '(A B C)), 
           ;=> (NIL (A) (B) (C) (B A) (C A) (C B) (C B A))
  "
  ;;Esthetic note:  To have the items within sets come out ordered
  ;;(e.g., (A B C) rather than (C B A)) use (append r (list s)) rather
  ;;than (cons s r), or reverse the original set
  (let ((res (list NIL)))
    (dolist (s set res)
      (setf res (merge 'list
                       res 
                       (loop for r in res
                             collect (cons s r))
                       #'(lambda (set1 set2)
                           (funcall sort-function (length set1) (length set2)))
                       )))))

(defun power-set8 (set)
  "This is just very clever.  It generates the power set by looking at
  the bit representations of counting;  however, there is no shared
  list substructure so it conses a bit more.
   Example:
        (power-set8 '(A B C)), 
           ;=> (NIL (A) (B) (A B) (C) (A C) (B C) (A B C))
  "
  (let ((n (length set)))
    (loop for i below (expt 2 n)
          collect (loop for j below n
                        when (logbitp j i)
                          collect (nth j set)))))

(defun fpower-set1 (set function)
  "Forms all possible subsets of the given set.  Does it in order of
  successively smaller subsets.  
  Example:
       (fpower-set1 '(a b c) <function>) 
          ;=>  will call <function> with the following order of arguments:
               (A B C) (B C) (C) NIL (B) (A C) (A) (A B)
  The order may be useful if you want to ignore subsets of an
  unsatisfied set."
  (labels ((form-subsets-1 (subset num-protect)
             (funcall function subset)
             (do ((tl (nthcdr num-protect subset) (cdr tl)))
                 ((null tl))
               (form-subsets-1 (nconc (ldiff subset tl) (cdr tl)) num-protect)
               (incf num-protect))))
    (form-subsets-1 set 0)))


(defun fpower-set2 (set function)
  "Another version of forming all possible subsets.
   This one avoids consing by accepting an additional argument
   which is the function to be applied to each of the subsets.
   Forms all possible subsets of a set of parameters.
   Example:
        (fpower-set2 '(A B C))
          ;=> will call function with the following order of arguments:
              NIL (A) (A B) (A B C) (A C) (B) (B C) (C)
   NIL is always a subset and always the first generated."
  (labels ((form-subsets-1 (set subset)
             (let ((marker (last subset)))
               (funcall function subset)
               (loop for s on set
                do (setf subset (nconc subset (cons (car s) nil)))
                do (form-subsets-1 (cdr s) subset)
                if marker
                   do (setf (cdr marker) nil)
                   else do (setf subset nil)))))
    (form-subsets-1 set nil)))


(defun fpower-set3 (set function)
  "Another version of forming all possible subsets.
   This one avoids consing by accepting an additional argument
   which is the function to be applied to each of the subsets.
   It also simplified by ignoring the order of the set.
   Forms all possible subsets of a set of parameters.
   Example:
        (fpower-set3 '(A B C) <function>)
           will call function on the following arguments:
           NIL (A) (B A) (C B A) (C A) (B) (C B) (C)

   NIL is always a subset and always the first generated."

  (declare (type list set)
           (type function function))
  (labels ((form-subsets-1 (set subset)
             (declare (type list set subset))
             (funcall function subset)
             (do ((s set (cdr s)))
                 ((endp s))
               (form-subsets-1 (cdr s) (cons (car s) subset)))
             ;;(loop for s on set        ; Alternative using loop macro
             ;; do (form-subsets-1 (cdr s) (cons (car s) subset)))
             ))

    (form-subsets-1 set nil)
    ;;(do ((s set (cdr s)))      ; Alternative for non-null subsets
    ;;    ((endp s))
    ;;  (form-subsets-1 (cdr s) (cons (car s) nil)))
    ))

(defun fpower-set4 (set function)
  "Forms all possible subsets of the given set.  Does it in order of
  descending set size.
  Example:
       (fpower-set4 '(a b c) <function>) 
          ;=>  will call <function> with the following order of arguments:
               (A B C) (A B) (A C) (B C) (A) (B) (C) NIL
  "
  (let ((result (copy-list set)))
    (labels ((ps1 (subres subset)
                  (if (null subres)
                      (funcall function result)
                    (loop for s on subset
                          do (rotatef (car subres) (car s))
                          do (ps1 (cdr subres) (cdr s))
                          do (rotatef (car subres) (car s))
                          ))))
      (loop while result
            do (ps1 result set)
            do (setf result (cdr result)))
      (funcall function result)         ; Catch the NIL set
      )))

(defun fpower-set*largest-order (set function)
  "Forms all possible subsets of the given set.  Does it in order from
  largest to smallest.
  Example:
       (fpower-set*largest-order '(a b c) <function>)
         ;=>  will call <function> with the following arguments in order:
              (A B C) (B C) (A C) (A B) (C) (B) (A) NIL
  This may be useful if you want to stop when the largest satisfying
  subset is found."
  ;; Requires QUEUES
  (do ((set-queue (create-queue set))
       (count-queue (create-queue 0))
       (sub-set nil))
      ((queue-emptyp set-queue))
    (setf sub-set (dequeue set-queue))
    (when (funcall function sub-set)
      (return-from fpower-set*largest-order))
    (do* ((pro-count (dequeue count-queue) (1+ pro-count))
          (tl (nthcdr pro-count sub-set) (cdr tl)))
         ((null tl))
      (enqueue (nconc (ldiff sub-set tl) (cdr tl)) set-queue) ; Splice out
      (enqueue pro-count count-queue))))


(defun fpower-set*smallest-order (set function)
  "Forms all possible subsets of the given set.  Does it in order from
  smallest to largest.
  Example:
       (fpower-set*smallest-order '(a b c) <function>)
         ;=> will call <function> with the following order of arguments:
             NIL (A) (B) (C) (B A) (C A) (C B) (C B A)
  This may be useful if you want to stop when the smallest satisfying
  subset is found.
  NOTE: initial NIL set may be avoided by changing location of
  function call"
  ;; Requires QUEUES
  (declare (type list set)
           (type function function))
  (do ((sub-queue (create-queue nil))
       (add-queue (create-queue set))
       (sub-set nil))
      ((or (queue-emptyp sub-queue) (queue-emptyp add-queue)))
    (setf sub-set (dequeue sub-queue))
    (funcall function sub-set)          ; FUN call here includes NIL set
    (do* ((tl (dequeue add-queue) (cdr tl))
          (new-set (cons (car tl) sub-set) (cons (car tl) sub-set)))
         ((null tl))
      ;;(funcall function new-set)        ; FUN call here avoids NIL set
      (enqueue new-set sub-queue)
      (enqueue (cdr tl) add-queue))))


;;;------------------------------------------------------------------------
;;;
;;; Cross product?
;;;
;;;------------------------------------------------------------------------

(defun comb-from-each (l)
  "Not sure of the proper term for this function.
  It forms all possible sets consisting of zero or one elements
  from each of the sets passed in.
  Example:
       (COMB-FROM-EACH '((A B) (C D) (E F)))
         ;=> (NIL (A) (B) (C) (D) (E) (F) (A C) (A D) (A E) (A F) 
             (B C) (B D) (B E) (B F) (C E) (C F) (D E) (D F) (A C E)
             (A C F) (A D E) (A D F) (B C E) (B C F) (B D E) (B D F))
   BUT NOT NECESSARILY IN THAT ORDER!
   NIL is always a set and always the first element of the result."
  (cond ((null l) (cons l NIL))
        (t (let ((sub-comb (comb-from-each (cdr l))))
             (nconc sub-comb
                    (loop for x in sub-comb
                     nconc (loop for y in (car l)
                            collect (cons y x))))))))


(defun fcomb-from-each (l -c-)
  ;; Tail recursive, cons avoiding version
  "Not sure of the proper term for this function.
  It calls a function on all possible sets consisting of zero or one
  elements from each of the sets passed in.
  Example:
       (fcomb-from-each '((A B) (C D) (E F)) <function>)
         ;=> will call <function> with each of the following arguments:
            NIL (A) (B) (C) (D) (E) (F) (A C) (A D) (A E) (A F) (B C) (B D)
            (B E) (B F) (C E) (C F) (D E) (D F) (A C E) (A C F) (A D E) (A D F)
            (B C E) (B C F) (B D E) (B D F)
   BUT NOT NECESSARILY IN THAT ORDER!
   NIL is always a set and always the first element of the result."
  (cond
    ((null l) (funcall -c- nil))
    (t (fcomb-from-each (cdr l) #'(lambda (sub)
                                    (funcall -c- sub)
                                    (loop for y in (car l)
                                          do (funcall -c- (cons y sub)))))
       )))

                   

(defun non-overlapping-sets (lists &optional previous)
  "Given a set of sets, this function will return a set of sets of
  sets such that the elements between the sets are non-overlapping
   Example:
        (non-overlapping-sets '((A B) (B C)))
           ;=> (((A) (B C)) ((A B) (C)))
  "
  (cond ((null lists) (cons NIL NIL))
        (T (let ((overlaps NIL)
                 (lst (car lists)))
             ;; Eliminate elements used previously
             (setf lst (set-difference lst previous))
             ;; Determine which elements overlap from the "yet to be used" sets
             (dolist (l (cdr lists))
               (setf overlaps (union (intersection l lst) overlaps)))
             ;; Eliminate overlapping elements in this set
             (setf lst (set-difference lst overlaps))
             ;; Recursively form non-overlapping sets
             (mapcan #'(lambda (subset)
                         (mapcar #'(lambda (extension)
                                     (cons (union subset lst) extension))
                                 (non-overlapping-sets
                                  (cdr lists) (union subset previous))))
                     (power-set2 overlaps))
             ))))

(defun max-no-sets (lists &optional previous)
  "Form maximal-non-overlapping-subsets
  Example:
       (max-no-sets '((a b c) (b c d)))
          ;=> (((C B A) (D)))
    "
  (cond ((null lists) (list NIL))
         (T (let ((overlaps NIL)
                  (lst (car lists)))
              (setf lst (set-difference lst previous))
              (dolist (l (cdr lists))
               (setf overlaps (union (intersection l lst) overlaps)))
              (setf lst (set-difference lst overlaps))
              (mapcan #'(lambda (subset)
                  (mapcar #'(lambda (extension)
                                      (cons (union subset lst) extension))
                                  (max-no-sets (cdr lists) (union subset previous))))
                      (list overlaps))
            ))))

;;;------------------------------------------------------------------------
;;;
;;; Permutations
;;;
;;;------------------------------------------------------------------------


(defun permute1 (list)
  "Computes all possible permutations of a list of elements
  Example:
       (permute1 '(a b c))
         ;=> ((A B C) (A C B) (B A C) (C A B) (B C A) (C B A))
  "
  ;; This has the advantage that it is easy to keep track of the
  ;;number of exchanges necesary to bring each permutation of the
  ;;list back to its origional form.
  (cond ((cdr list) 
         (let ((element (car list))
               (len (length (cdr list)))
               (result ())
               pc
               index)
           (dolist (perm (permute1 (cdr list)))
             (push (cons element perm) result)
             (dotimes (i len)
               (setf index (1+ i))
               (setf pc (copy-list perm))
               (push element (nthcdr index pc))        ; Splice in element
               (push pc result)))
           result))
        (T (list list))))

(defun permute2 (list)
  "Computes all possible permutations of a list of elements
   Example:
        (permute2 '(a b c))
          ;=> ((A B C) (A C B) (B A C) (C A B) (B C A) (C B A))
  "
  ;; Slightly faster, uses less consing, and can take advantage of
  ;; cdr coding compared with than permute1 
  (cond ((cdr list)
         (let ((element (car list))
               (len (length (cdr list)))
               (result ())
               c-list)
           (dolist (perm (permute2 (cdr list)))
             (push (cons element perm) result)
             (dotimes (i len)
               (setf c-list (copy-list perm))
               (psetf (nth i c-list) element        ; Exchange 1st and Ith element
                      element (nth i c-list))
               (push (cons element c-list) result)
               (setf element (nth i c-list))))
           result))
        (T (list list))))

(defun fpermute (list -c- &optional destructive)
   "Yet another version of the 'permute' function.  This one
   avoids consing by accepting an additional argument which is the
   function to be applied to the various permutations.
   Example:
        (fpermute '(a b c) <function>)
          ;=> will call function with each of the following arguments
              (A B C) (A C B) (B A C) (C A B) (B C A) (C B A)
   "
   ;; Setting the final value will destructively permute the original
   ;; list in place rather than creating a new list.  In either case,
   ;; at normal completion the list will be back in the original
   ;; order.
   (let ((perm (if destructive list (copy-list list)))
         (len (1- (length list))))
     (labels ((permute-intern (pos)
                (cond ((< pos len)
                       (permute-intern (1+ pos))
                       (dotimes (i (- len pos))
                         (rotatef (nth pos perm) (nth (+ pos i 1) perm))
                         (permute-intern (1+ pos))
                         (rotatef (nth pos perm) (nth (+ pos i 1) perm))))
                      (T (funcall -c- perm)))))
       (permute-intern 0))))



;;;------------------------------------------------------------------------
;;;
;;; Substrings
;;;
;;;------------------------------------------------------------------------


(defun max-substring (num-list &optional destructive)
  "Find the substring with the largest sum
     Example (max-substring '(-1 2 5 -4 10 20 -10 5 5 5))
     Returns (2 5 -4 10 20 -10 5 5 5)
             38
     Where the first value is the substring and the second is its sum"

  (do ((nums num-list (cdr nums))                ; Start a new substring from each position
       (max (car num-list))                        ; Value of the maximal substring
       (max-ptr num-list)                        ; The maximal substring
       (substr-len 1)                                ; The length of the maximal substring
       (accum 0 0)                                ; Substring accumulator
       (position 0 0))                                ; Current position in the substring
      ((null nums)
       (if destructive
           (setf (cdr (nthcdr (1- substr-len) max-ptr)) NIL)
           (setf max-ptr (loop for i below substr-len
                          collect (nth i max-ptr))))
       (values max-ptr max))
    (dolist (num nums)                ; Find max substring from each starting position
      (incf position)
      (when (> (incf accum num) max)
        (setf max accum)                        ; This is the best total so far
        (setf max-ptr nums)                        ; Begin the substring here
        (setf substr-len position)                ; Substring is this long
        ))))


;;;------------------------------------------------------------------------
;;;
;;; Bit hacking
;;;
;;;------------------------------------------------------------------------


(defconstant *logemptyset* 0)

(defun form-possible-bit-subsets (set &optional (aux-set *logemptyset*))
  "Finds all subsets of a set represented as a bit vector"
  (do ((s set (logset-difference s (most-sig-bit s)))
       (result NIL))
      ((logemptysetp s) (cons aux-set result))
    (setf result (nconc result (form-possible-bit-subsets
                                 (logset-difference s (most-sig-bit s))
                                 (logunion aux-set (most-sig-bit s)))))))


(defun non-overlapping-bit-sets (bit-vectors
                                 &optional (previous *logemptyset*))
  "Given a set of bit-vectors, this function will return a set of sets
  of bit-vectors such that the elements between the bit-vectors are
  non-overlapping.  For example, given (non-overlapping-bit-sets '(3 5))
  returns ((2 5) (3 4))"
  (cond ((null bit-vectors) (cons NIL NIL))
        (T (let ((overlaps *logemptyset*)
                 (bit-vector (car bit-vectors)))
             ;; Eliminate elements used previously
             (setf bit-vector (logset-difference bit-vector previous))
             ;; Determine which elements overlap from the "yet to be used" sets
             (dolist (vector (cdr bit-vectors))
               (setf overlaps (logunion (logintersection vector bit-vector) overlaps)))
             ;; Eliminate overlapping elements in this set
             (setf bit-vector (logset-difference bit-vector overlaps))
             ;; Recursively form non-overlapping sets
             (mapcan #'(lambda (subset)
                         (mapcar #'(lambda (extension)
                                     (cons (logunion subset bit-vector) extension))
                                 (non-overlapping-bit-sets (cdr bit-vectors)
                                                           (logunion subset previous))))
                     (form-possible-bit-subsets overlaps))
             ))))



;;;------------------------------------------------------------------------
;;;
;;; All variable bindings
;;;
;;;------------------------------------------------------------------------

;; Given a list of variables '(a b) and values '(1 2 3 4) generate a
;; list of all possible variable/value pairs.

(defun pc (vars vals)
  (let ((result '()))
    (labels ((f (n in-use)
               (if (= n 0)
                 (push (mapcar #'list  vars in-use) result)
                 (do ((l vals (cdr l)))
                     ((null l))
                   (unless (member (car l) in-use :test #'eq)
                     (f (1- n) (cons (car l) in-use)))))))
      (f (length vars) '()))
    result))

;; If you care about the exact order of the output, insert a few calls to
;; reverse

(defun pc (vars vals)
  (let ((result '())
        (vals (reverse vals))
        (vars (reverse vars)))
    (labels ((f (n in-use)
               (if (= n 0)
                 (push (mapcar #'list  vars in-use) result)
                 (do ((l vals (cdr l)))
                     ((null l))
                   (unless (member (car l) in-use :test #'eq)
                     (f (1- n) (cons (car l) in-use)))))))
      (f (length vars) '()))
    (mapcar #'reverse result)))

;; Finally, here is an elegant version with mapping or loops

(defun p/c (l1 l2)
  (if (null l1)
      '(())
    (mapcan #'(lambda (a)
                (mapcar #'(lambda (b) (cons (list (car l1) a) b))
                        (p/c (cdr l1) (remove a l2))))
            l2)))

(defun p/c (l1 l2)
  (if (null l1)
      '(())
    (loop for a in l2
          nconc (loop for b in (p/c (cdr l1) (remove a l2))
                      collect (cons (list (car l1) a) b)))))
