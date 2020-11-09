;;; (load (compile-file "astar.lisp"))
(declaim (optimize (debug 3)))
;;; Prof. R. Williams				Artificial Intelligence

;;;;;;;;;;;;;;;;;;;; General A* Search Pseudocode ;;;;;;;;;;;;;;;;;;;;;;

#|
Finds an optimal (minimum-distance) path from a given start state to a
given goal state.

OPEN is a list of (path-to-extend,distance-from-start) pairs
STATE-DISTANCES is a hash table that stores for each state visited its
  minimum distance from the start state found so far.  The default
  distance for any state not yet visited is infinite.  This data structure
  is not essential, but is the proper way to do repeated-node checking.


Initialize STATE-DISTANCES as empty (interpreted as initializing all
				     distances to infinity)
Initialize OPEN to list containing start node and zero
Repeat forever
  If OPEN is empty
    Report failure
  Else
    Set (PATH-TO-EXTEND,DIST-SO-FAR) to be first element in OPEN,
                                 and remove from OPEN
    Set CURRENT-NODE to be last node of PATH-TO-EXTEND
    If CURRENT-NODE is goal state
      Report answer: PATH-TO-EXTEND
    Else if DIST-SO-FAR < STATE-DISTANCES(CURRENT-NODE)
      Set STATE-DISTANCES(CURRENT-NODE) to DIST-SO-FAR
      Generate all successors of CURRENT-NODE
      Construct list of paths by extending PATH-TO-EXTEND into each successor
                                 and pair each with its distance from start
      Remove from this list all pairs with paths having length greater than the
                                 minimum distance to the last node in the path
      Place remaining pairs in OPEN, ordered by distance from start +
                                 estimated remaining distance
|#

;;;;; Here is a direct Common Lisp implementation of this pseudocode ;;;;;

;;; The function FIND-SHORTEST-PATH uses the A* algorithm to find the
;;; shortest path from the start state to the goal state in a graph.
;;; It uses a hash table, STATE-DISTANCES, that keeps track of the length of
;;; the shortest path found so far from the start to each state visited
;;; during the search.
;;;
;;; The OPEN list is maintained as a list of NODE structures.
;;; Each NODE structure contains 3 fields, one for a path (represented as
;;; a list of states in reverse order), one for the length of this path,
;;; and one for the total path length estimate if this path is extended
;;; to the goal in the shortest possible way.
;;;
;;; Just as in our earlier depth-first/breadth-first/greedy search program
;;; where we checked for closed states in 2 places, we check states against
;;; the STATE-DISTANCES hash table in the same 2 places.  In particular, we
;;; immediately discard any successor state if there is a path to it of
;;; equal or shorter length, and we also discard any path when it appears
;;; at the front of the OPEN list if there is a path of strictly shorter
;;; length ending at the same state.  If a state has no entry in this hash
;;; table, its distance from the start state is interpreted as being infinite.
;;;
;;; The application-specific functions that are to be passed to this program
;;; are: (1) SUCCESSORS, which returns a list of dotted pairs of the form
;;; ( <next state> . <cost of this arc> ), where <next state> is directly
;;; reachable from the given state in the problem graph; and
;;; (2) HEURISTIC-DIST, which takes 2 states and provides an estimate of the
;;; overall cost from the first to the second.  If this is an admissible
;;; (non-overestimating) heuristic, the A* algorithm is guaranteed to find
;;; the minimum-cost path; otherwise, there is no such guarantee.
;;;
;;; The return value from this function is a node structure.  When this prints
;;; out, the path and its length are shown.

(defstruct node
  path
  path-length
  total-length-estimate
  )

(defun find-shortest-path (start goal successors heuristic-dist &aux (steps 0))
  (do (head-node		; node at head of open list
       path-to-extend	        ; path to state currently visited
       current-state		; state currently visited
       dist-so-far		; length of this path
       extended-paths	        ; list of newly extended paths
       (open			; list of all candidate nodes
	(list (make-node :path (list start)
			 :path-length 0
			 :total-length-estimate
			 (funcall heuristic-dist start goal))))
       (state-distances (make-hash-table :test #'equalp))
       )
      ((null open) nil)	        ; if open list is empty, search fails
      (incf steps)
      (format t "~a Open: ~s~%" steps open)
      (setq head-node (pop open))       ; get node at head of open list
      (setq path-to-extend (node-path head-node)) ; get path itself
      (setq current-state (car path-to-extend)) ; get state this path ends at
      (format t "~%~%current-state: ~a, goal:~a~%" current-state goal)
      (if (equalp current-state goal) (return head-node)) ;; success: return path and length found
      (setq dist-so-far (node-path-length head-node))
      (format t "dist-so-far: ~a, (gethash current-state state-distances): ~a~%" dist-so-far (gethash current-state state-distances))
      (when (less-than dist-so-far (gethash current-state state-distances))
	 (setf (gethash current-state state-distances) dist-so-far)
	 (let (next-state
	       next-dist-so-far
	       (next-nodes nil))
	   (dolist (pair (funcall successors current-state))
	     (setq next-state (car pair))
	     (setq next-dist-so-far (+ (cdr pair) dist-so-far))
	     (if (less-than next-dist-so-far
			    (gethash next-state state-distances))
		 (setf open
		       (merge
			'list
			(list
			 (make-node
			  :path (cons next-state path-to-extend)
			  :path-length next-dist-so-far
			  :total-length-estimate
			  (+ next-dist-so-far
			     (funcall heuristic-dist next-state goal))))
			open
			#'<
			:key #'node-total-length-estimate)))
		)))
      ))

;;; Here the y argument may be nil, which is treated like infinity.

(defun less-than (x y)
  (or (null y) (< x y)))

;;; Example graph


(defparameter *states*
  (loop for c across "ABCDEFIGHIJKLMNOPQRSTUVWXYZ"
	with kp = (find-package :keyword)
	collect (intern (String c) kp)))

(defparameter *graph*
  (loop for state in *states*
	as n = (+ 3 (random 10))
	as l = (length *states*)
	collect `(,state
		  ,(loop for i below n
			 collect (cons (nth (random l) *states*)
				       (+ 2 (random 100)))))))

(defun successors (state)
  (second (assoc state *graph*)))

(defun heuristic-dist (next-state goal)
  (abs (- (position next-state *states*)
     (position goal *states*))))

(print (find-shortest-path :a :z #'successors #'heuristic-dist))
