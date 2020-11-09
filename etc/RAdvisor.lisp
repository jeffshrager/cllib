(proclaim '(optimize (compilation-speed 0) (speed 3) (safety 1)))

;;; Ignore words are used only when we come in through typed input.
;;; (Remember not to put special words like "explain" and "try" in here!)

(defparameter *ignore-words* 
  '(i want to eat how about can we me you us the a an of in on is
      today this time "let's" food some "I'm" mood for "no," have
      again but meant means there place near nearby by at tonight instead
      would be nice around look prefer go looking something anything any some
      thing "I'd" like please))

(defvar *debug*)
(setq *debug* t)

(defmacro debug (&rest forms)
  `(if *debug* (progn ,@forms)))

;;; Notes from 10/16 mtg Pat Demo:
;;;
;;; At choice, if user says "no" propose another choice.
;;; "   "      user should be able to say "no, <something else>
;;; Other things to know: ambiance
;;; Italian pizza???
;;; fill in MEAL
;;; OTHER selection just doesn't work!!
;;; I can't find X in <> but I can find it in <>...
;;; keep 4 partial matches
;;; tell what the problem is when no match

;;; !!! Use learning to give the ? examples.
;;; Bug: After disambiguation should retain selected meaning if re-searched.
;;; Bug: NIL among the options in ? list sometimes (e.g., PLACE=CAFE)
;;; Bug: Looks up OTHER (while looking up SERVICE?)
;;; Bug: Start w/city after it has a preference -> HOW ABOUT FOOD NIL???

;;; User ought to be able to say "I don't care" for a feature
;;; and the apa won't ask about that one anymore.

;;; After suggesting the place don't just ask OK or not, but permit the
;;; user to interogate features and change her mind.  Alternatively,
;;; permit the user to say "Why?" and tell her the reasons
;;; (interpretation) and where the information came from.
;;; "Describe it"
;;; "Why did you choose this one?"
;;; "Okay/No thanks"
;;; "Is it cheap?" e.g.

;;; ??? Is this upgrade needed anylonger?
;;; Keep independent dimnesions (location v. style)
;;; Toss subsummed dimensions (mandarin v. chinese)

;;; ??? Is this upgrade needed anylonger?
;;; Update *frame* prefs.

;;; !!! There is a problem with OR sets of features, esp. how they
;;; interact with rules.  Actually, they are poorly thought out in
;;; general.

;;; !!! Rules are fired in order, which is wrong, but....

(require "/wk/shrager/utils.lisp")
(require "/wk/shrager/stats.lisp")

;;; The interactor tells the IO how to behave.  It can be:
;;;    :saexec (speech acts executable)
;;;    :salisp (speech acts loaded into lisp)
;;;    :tty (or anything but the above)

(defvar *interactor* :tty)

;;;  *kb* are the instances (as from a database).
;;;    (formed from *kb-data*, which is really intended to be used to
;;;     reconstruct a hash table, so we strip off the first elt: the key)

(defvar *kb* nil)

;;;  *rules* are inferences that are applied to the knowledge base
;;;          at setup time.  They can include functions.

(defvar *rules* nil)

;;;  *lex* are rules that tell you what to look for in the knowledge
;;;        based given certain keywords.  
;;;    (Some of these are externally specific, but most come from the
;;;     kb keys collected at kb formation time)

(defvar *lex* nil)

;;; The history list is a collection of moments, each one representing
;;; an interaction.

(defvar *fullhistory* nil) ; The history of the world for this user.
                           ; (!!! Not currently re-read from the file !!!)
(defvar *histfile* "") ;...and where they are stored.
(defvar *moment* ()) ; the current interaction

;;; The knowledge base is coded into this function at the moment simply
;;; for convenience.

(defun reset-kb ()
  (setq *histfile* (format nil "~a/apa-history.lisp" 
			   (environment-variable "HOME")))
  (format t ";;; Loading knowledge base.")
  (load "/wk/shrager/database.lisp")
  (setq *kb* (mapcar #'second *kb-data*)) ; removes the hash header
  (update-lexicon)
  (format t ";;; Applying rules")
  (xform-rules) ; removes '-> and xforms to a more convenient format.
  (setq *kb* (mapcar #'complete-ku-by-rules *kb*))
  (format t ";;; Reloading History")
  (setq *fullhistory* ()) ; want to force reloading on reset
  (maybe-restore-history)
  (restore-preferences)
  )

;;; The *frame* gives us the framework for discrimination by listing
;;; the slots that have to be filled in.  It also supplies various
;;; info about these slots, esp. discrim ordering that tells us which
;;; things can be removed upon expansion.

;;; NNNFFF Some of these come from the internal rule set and should be 
;;; added instead of being hard coded here.

(defvar *frame*)
(setq *frame* 
      ;; :discrim-weight gets convolved with the information
      ;; gain during property evaluation.

      ;; :back-out-importance tells us which properties can't be 
      ;; dropped upon expansion. 

      '((:nationality :discrim-weight 1.0 :back-out-importance 4.0)
	(:place :discrim-weight 1.0 :back-out-importance 1.0)
	(:city  :discrim-weight 1.0 :back-out-importance 1.0)
	(:style :discrim-weight 1.0 :back-out-importance 1.0)
	(:service :discrim-weight 1.0 :back-out-importance 2.0)
	(:taste :discrim-weight 1.0 :back-out-importance 1.0)
	(:cost :discrim-weight 1.0 :back-out-importance 3.0)
	(:meal :discrim-weight 1.0 :back-out-importance 1.0)
	(:food :discrim-weight 1.0 :back-out-importance 1.0)
	;; County and region, even though we went through all that
	;; work to get them, aren't useful in the conversational mode.
	(:county :discrim-weight 0.0 :back-out-importance 1.0)
	(:region :discrim-weight 0.0 :back-out-importance 1.0)
	))

;;; When we form interpretations, the interp-entry is used to compose them.

(defstruct interp-entry name value set-time)
(defun copy-interp-entry (e)
  (make-interp-entry
   :name (interp-entry-name e)
   :value (copy-tree (interp-entry-value e))
   :set-time (interp-entry-set-time e)
   ))
(defun copy-interp (i)
  (mapcar #'copy-interp-entry i))

;;; Restaraunt specific pretty printing.

(defun display-restaraunt (r &key prefix)
  (let* ((name (pop r))
	 (nationality (lexget r :nationality))
	 (general-location (lexget r :general-location))
	 (place (lexget r :place))
	 (location (lexget r :city))
	 (style (lexget r :style))
	 (cost (lexget r :cost))
	 (food (lexget r :food))
	 (pref (p2 (getf r :pref)))
	 (crit (p2 (getf r :crit)))
	 )
    (speakf (format nil "~a~a is ~a ~a ~a ~a in ~a."
		    (if prefix 
			(format nil "~a: " prefix)
		      "")
		    name 
		    (a-or-an cost)
		    cost
		    (or nationality "")
		    (or place "restaraunt")
		    location
		    ))
    (if food (speakf (format nil "  It serves ~a." (sayable-list food))))
    (if style (speakf (format nil "  The style is ~a." style)))
    ))
	 
;;; Find the indicated symbol in a plist and return its lexical item for 
;;; output purposes.  This uses the *keys* list for speed, and so that
;;; we get the unedited (that is, with space instead of dashes) form of
;;; each word, althoug this entails looking in *base-lex* as well as *keys*!
;;; 'r' can be an atom or a list.  In the latter case, we grab the
;;; indicated prop from the list as well.  If we can't find a word
;;; we simply return the incoming value.

(defun lexget (r prop &optional for-example)
  (let ((keys (cdr (assoc prop *keys*))))
    (cond ((symbolp r) (lexget2 r keys prop for-example))
	  ((plist? r) (lexget (getf r prop) prop for-example))
	  (t (mapcar #'(lambda (w) (lexget2 w keys prop for-example)) r))
	  )))

(defun lexget2 (value lexlist prop for-example)
  (cond ((eq :other value) 
	 (if for-example
	     (format nil "some other sort of ~a (say OTHER)"
		     (key-phrase prop))
	   nil))
	(t 
	 ;; Look first among the keys.
	 (let ((keystring (find value lexlist 
				:test #'(lambda (value i) 
					  (eq value (cdr i))))))
	   (if keystring
	       (car keystring)
	     ;; Get it as a base-lex word, or not at all.
	     (or 
	      (car (find value *base-lex*
			 :test #'(lambda (value i) 
				   (and (eq value (third i))
					(eq prop (second i))))
			 ))
	      value ; no match -- give the value back
	      )
	     ))
	 )
	))

;;; Like lexget but traps :other specially.

#|
(defun lexget (r prop)
  (let* ((value (cond ((symbolp r) r)
		      ((plist? r) 
		       (setq r (getf r prop))
		       :recur)
		      (t :recur)))
	 (lexlist (cdr (assoc prop *keys*)))
	 )
    (cond ((eq :recur value)
	   (mapcar #'(lambda (i) (lexget i prop)) r))

	  (t 
	   ;; Look first among the keys.
	   (let ((keystring (find value lexlist 
				  :test #'(lambda (value i) 
					    (eq value (cdr i))))))
	     (if keystring
		 (car keystring)
	       ;; Get it as a base-lex word, or not at all.
	       (or 
		(car (find value *base-lex*
			   :test #'(lambda (value i) 
				     (and (eq value (third i))
					  (eq prop (second i))))
			   ))
		value ; no match -- give the value back
		)
	       ))
	   )
	  )
    )) |#

(defun plist? (l)
  (eq #\: (aref (format nil "~s" (car l)) 0)))

;;; The irules (initial rules) are run 

 (defvar *irules*)
  (setq *irules* '(
  (:place cafe -> :food (coffee dessert baked-goods) :service (sit-down take-out))
  (:nationality japanese -> :food (sushi teriyaki))
	(:food pizza -> :service (take-out delivery) :cost cheap)
  (:nationality chinese -> :taste #'(lambda (r) (nth (random 3) '(mandarin szechuan hunan))))
  (:nationality chinese -> :service #'(lambda (r) (nth (random 3) '(take-out sit-down buffet))))
  (:food fish -> :cost expensive)
  (:food sushi -> :cost expensive)
  (:city palo-alto -> :cost expensive) ; !!! Generalize once the regions are in!
  (:nationality chinese -> :cost medium)
  (:cost :unassigned -> :cost #'(lambda (r) (nth (random 3) '(cheap expensive medium))))
	))

(defvar *lex*)

;;; This is the basic lexicon.  Everything that comes in with the database
;;; gets added to this.   NOTE!!! All words that are defined only in the
;;; rules, MUST appear here!

(setq *base-lex* '(
    
     ("take-out" :service take-out)
     ("sit-down" :service sit-down)
     ("buffet" :service buffet)
     ("eat" :place (restaraunt bakery cafe))
     ("dessert" :place (cafe bakery))
     ("coffee" :place cafe)
     ("cheap" :cost cheap)
     ("medium" :cost medium)
     ("expensive" :cost expensive)
     ("hunan" :taste hunan)
     ("mandarin" :taste mandarin)
     ("szechuan" :taste szechuan)
     ))

;;; Add each key to the lexicon.  (Also change spaces to dashes in the
;;; process since otherwise the parser will do the wrong thing.  This
;;; is a limitation that the speech recognizer will have to help us
;;; with, though I'm very dubious!)

(defun update-lexicon ()
  (setq *lex* (copy-tree *base-lex*))
  (dolist (cat *keys*)
    (let* ((type (pop cat)))
      (dolist (entry cat)
	(let ((string (space- (car entry))))
	  (unless (assoc string *lex* :test #'equal)
		  (push (list string type (cdr entry)) *lex*)
	   ))))))

;;; Utilities to help set up the knowledge base.

(defun xform-rules ()
  (setq *rules* (mapcar #'xform-rule *irules*)))

(defun xform-rule (r)
  (cons (pairify (rule-lhs r))
        (pairify (rule-rhs r))
        ))

(defun rule-lhs (r)
  (prog (c)
    loop (if (eq (setq i (pop r)) '->) (return (reverse c)))
         (push i c)
         (go loop)))

(defun rule-rhs (r)
  (cdr (member '-> r)))

(defun pairify (l &aux c)
  (prog ()
    loop (if (null l) (return c))
         (let ((a (pop l))
               (b (pop l))
               )
           (push (list a b) c))
         (go loop)))

;;; This applies all the rules that apply to a knowledge unit.  It tries
;;; each rule ONLY ONCE, and anything already in the knowledge base takes
;;; precidence over anything inferred.  (This is probably wrong since 
;;; in some cases we might want to add inferences as OR cases with existing
;;; info.)  We also add the default preferences here.  Also, complete
;;; each knowledge unit by adding a default pref to it, and by ensuring
;;; that all keys are represented.  The value in the rhs can be a function.

(defconstant *default-pref* 1.0)

(defun complete-ku-by-rules (ku)
  ;; First update based upon rules.
  (setq ku
    (dolist (rule *rules* ku) 
      (if (match-rule-lhs-to-ku (first rule) (cdr ku))
	  (setq ku (cons (car ku) 
			 (apply-rule-to-ku (cdr rule) (cdr ku))
			 ))
	)))
  ;; Then add default pref and anything else that's missing based upon *frame*
  (append ku
	  (let ((postfix (list :pref *default-pref*
			       :crit (/ (+ 50 (random 50)) 10.0))))
	    (dolist (prop *frame* postfix)
		    (unless (getf (cdr ku) (first prop))
			    ;; This is a tad obscure
			    (push :other postfix)
			    (push (first prop) postfix)
			    )
		    ))
	  ) ; append
  )

(defun match-rule-lhs-to-ku (lhs ku)
  (dolist (item lhs t)
    (if (or (and (eq (cadr item) :unassigned)
		 (not (member (car item) ku)))
	    (and (member (car item) ku)
		 (let ((val (getf ku (car item))))
		   (or (eq (cadr item) :any)
		       (eq val (cadr item))
		       (and (listp val)
			    (member (cadr item) val)))))
	    )
      'okay
      (return nil))))

;;; Add each rhs pair, eval'ing fns as needed.

(defun apply-rule-to-ku (rhs ku)
  (dolist (r rhs)
    (let ((m (member (car r) ku)))
      (if (not m) ; no value already given for this property
        ;; This is arcanely done backwards.
        (progn (push (if (and (listp (second r))
			      (eq 'function (car (second r))))
                       (funcall (eval (second r)) ku) ; eval if it's a function
                       (second r)) ; else just return the constant
                     ku)
               (push (first r) ku)) ; add the key
        )
      ))
  ku
  )

;;; Code to do queries and update prefs.

(defconstant *pref-incr* 0.1)

(defvar *ambiguity-hint*) ; see locate-word lu

;;; Macros to make checking and jumping around easier inside the main loop.

;;; This is usually used around an SARead-line.

(defmacro intrap (form)
  `(let ((words ,form))
     (cond ((member (car words) '("q" "quit" "restart" "!") :test #'string-equal)
	    (go loop))
	   ((member (car words) '("explain" "how" "why") :test #'string-equal)
	    (explain-history *moment*)
	    ())
	   ((member (car words) '("test" "debug") :test #'string-equal)
	    (setq *debug* t)
	    ())
	   ((member "try" words :test #'string-equal)
	    (setq selection (cdr (member "try" words :test #'string-equal)))
	    (debug (format t "Intrap jump to refine with selection = ~a~%" selection))
	    (go refine))
	   (t words))))

(defparameter *neg-words* '("no" "no," "nope" "try"))

(defun maybe-clear-negs (words)
  (if (member (car words) *neg-words* :test #'string-equal)
      (cdr words)
    words))

;;; This is the heart of things, wherein the current interpretation
;;; is carried, formed from *frame*.

(defvar *interp*)

;;; ---------- CENTRAL INTERATION LOOP ----------

(defun demo () (apa :demo t))

(defun apa (&key (reset nil) demo)
  (setq *debug* (not demo))
  (if (or (null *kb*) reset) (reset-kb))
; (SAread-line) ; eat the call's return -- needed for some lisps only

  ;; The main loop.

  (prog (input selection current-location locations-tried original-location)

    choose-location (debug (format t "<<At choose-location>>~%"))

        (speakf "Please indicate your current location.")
        (setq current-location (car (SARead-line)))

    rechoose-for-null-random-location

	(cond ((equal "?" current-location)
	       (loop for i from (mod (get-universal-time) 100) to 100 
		     do (random 100))
	       (setq current-location (nth (random (length *citylocs*)) 
					   *citylocs*))
	       (when (null (cdr current-location))
		     (setq current-location "?")
		     (go rechoose-for-null-random-location))
	       (speakf (format nil "I chose ~a" 
			       (setq current-location (car current-location))))
	       )
	      (t (setq current-location (car (assoc current-location *citylocs*
						    :test #'string-equal))))
	      )

	(when (null current-location)
	      (speakf "That isn't a known city.  Please try again.")
	      (go choose-location)) 

    loop (debug (format t "<<At loop>>~%"))

        ;; Clear the current interpretation.

        (clear-interpretation)

        ;; Start a new history and begin interaction.  (At the moment
        ;; this writes out the history each time we get to the top
        ;; again, which is overkill, but is safe in case of crash or
        ;; failure to exit normally.)  So each entry in the history
        ;; file at the moment will consist of one and only one moment.

        (when *moment* (save-history))

	(setq *moment* (list (list (date-as-int) (time-as-int))))

	;; Location setup.

	(set-location current-location)
	(setq original-location current-location) ; for reset if interp changes
	(setq locations-tried nil)

	;; Top input.

	(speakf (rselect "What would you like to do?"
			 "What's your pleasure?"
			 "Can I help you?"
			 ))
        (setq input (SAread-line))

        (if (member (car input) '("relocate" "reloc" "restart" "!") 
		    :test #'string-equal)
	    (go choose-location))

    main-lookup (debug (format t "<<At main-lookup>>~%"))

	(when (or (member (car input) '("q" "quit" "stop" "end" "bye")
			  :test #'equal))
	      (save-history)
	      (return 'done))

        (setq *ambiguity-hint* ()) ; Ensure that there's no left-over
	(remember :main-lookup :input input)

	(setq contenders (chase-words input))
	(if (eq :quit contenders) (go loop))

    check-contenders (debug (format t "<<At check-contenders>>~%"))

	(if contenders
	    (go discrim)
	    (progn
	      (remember :nothing-matches-at-main-lookup)
	      (go expand-geographically)))

    expand-geographically (debug (format t "<<At expand-geographically>>~%"))

        ;; We only get here when nothing has matched someplace else in
        ;; the system.  Before giving up, we expand to a nearby
        ;; geographically nearby city, with permission.

        (speakf (format nil (rselect "I can't think of anything in ~a.  Want to look nearby?"
				     "There's nothing in ~a.  How about something in the next town?"
				     "Nothing's showing ~a.  Can we go someplace nearby?")
			(lexget (current-location) :city)))
	(let ((response (intrap (saread-line))))
	  (cond ((confirm-input? response)
		 (go try-new-location))
		((cdr response) ; something after the no,... (the actual "no" is dropped on input)
		 (setq input (cdr response))
		 (go main-lookup))
		(t (go back-out-interp-entry))))

    try-new-location (debug (format t "<<At try-new-location>>~%"))

       (let* ((interp-loc-entry (find-interp-entry-by-name :city *interp*))
	      (now-at (interp-entry-value interp-loc-entry))
	      (nearby-city (find-a-nearby-city now-at 
					       (pushnew current-location
							locations-tried))))
	 (push nearby-city locations-tried)
	 (set-location nearby-city)
	 (setq contenders (constrain-kb-by-interpretation *interp* *kb*))
	 (go check-contenders)
	 )

    discrim (debug (format t "<<At discrim>>~%"))

	(setq discrim (best-discriminator (discriminate contenders)))
	(if (null discrim)
	    (go select))

	;; Choose the value on this dimension of the preferred contender.
        ;; OR, if there are none, ask for an element on that dimension.

	(let ((topcv (top-contender-val (car discrim) contenders)))
	  (if (null topcv) ; nil if the prefs were balanced.
	      (go ask-dim)
	    (progn
	      (setq topcv (lexget `(,(car discrim) ,topcv)
				   (car discrim) t))
	      (speak-compact-interp (length contenders))
	      (speakf (format nil (rselect "How about ~a ~a one? "
					   "~a ~a one perhaps? "
					   "Is ~a ~a one okay? ")
			      (a-or-an topcv)
			      topcv))
	      (setq *ambiguity-hint* (car discrim))
	      (remember :how-about 
			:n (length contenders) 
			:att (car discrim) 
			:value topcv)
	      (let ((response (intrap (SAread-line))))
		(cond ((confirm-input? response)
		       (setq selection (list (space- topcv))))
		      ((member (car response)
			       '("no" "n" "nope")
			       :test #'equal)
		       (when (not (null (cdr response)))
			     (setq selection response)
			     (go refine))
		       (speakf (rselect "Okay, so tell me more... "
					"Okay, so give me some more info..."
					"Alright, what else?"
					))
		       (remember :ask-for-more-detailed-info)
		       (setq selection (intrap (SAread-line)))
                       )
		      (t (setq selection response))
		      ))
	      ) ; progn
	    ))

    refine (debug (format t "<<At refine>>~%"))

        ;; With a new selection, refine the contenders list.

        (remember :refine :selection selection)
	(let ((subset (chase-words selection contenders)))
	  (cond ((eq :quit subset) (go loop))
		(subset 
		 (setq contenders subset)
		 (go discrim))))
	
	;; Nothing found, expand the search using the current
	;; interpretation on the whole kb, and then by backing out the
	;; oldest entered terms first. In theory as will never get to
	;; backing them ALL out because in order for the last term to
	;; have worked at all, it must have matched at least one entry.
	
    re-search (debug (format t "<<At re-search>>~%"))

        (debug (show-interp))

	;; !!! The order of using the whole kb v. the contender set
	;; could be wrong here in some cases.  I'm not clear on what to do.

 	(let ((subset (constrain-kb-by-interpretation *interp* *kb*)))
	  (when subset
		(setq contenders subset)
		(go discrim)))

    ;; Backing out an entry (by whatever critereon) is used when
    ;; there aren't any matches to the current interpretation.
    ;; Geo-expansion is handled specially, not here.  In fact, if
    ;; we run out of things to back out here, and we've already
    ;; done geo-expansion, we hard fail and start over.

    back-out-interp-entry 
        (debug (format t "<<At back-out-interp-entry>>~%"))
   
	(setq interp-has-changed nil)

    back-out-inner
        (debug (format t "<<At back-out-inner>>~%"))


        ;; The entry that has to be backed out might be the ONLY one 
        ;; which happens when there's only one filled item -- Break out!
	;; If we've already looked nearby, don't bother with that again.

        (when (= 1 (count-non-nil-interp-entries *interp*))
	      (cond ((and ; already-geo-expanded
			  (not interp-has-changed))
		     (speakf (rselect "There's nothing left to try."
				      "Nothing's left."
				      "I'm out of ideas."
				      ))
		     (go loop))
		    (t (go expand-geographically))))

        ;; Otherwise choose and entry to back out.

	(let ((backout-entry (find-interp-entry-to-back-out)))
	  (debug (format t "<<Selected ~a to back out>>~%" backout-entry))
	  (cond ((null backout-entry)
		 (break "Backout entry was nil."))
		((null (interp-entry-value backout-entry))
		 (break "Tried to back out an entry with no value: ~a" backout-entry))
		(t 
		 (debug (format t "<<Backing out ~a>>~%" backout-entry))

		 ;; Since the interp has changed, permit re-searching
		 (setq interp-has-changed t)
		 ;(setq current-location original-location) ;???
		 ;(set-location current-location) ; ???
		 (setq locations-tried nil)

		 (speakf (format nil (rselect "Let's ignore ~a."
					      "I'm dropping ~a."
					      "Let's leave out ~a."
					      )
				 (lexget (interp-entry-value backout-entry)
					 (interp-entry-name backout-entry))))
		 (set-interp-entry-value backout-entry nil)
		 )
		)
	  )
	
	(let ((subset (constrain-kb-by-interpretation *interp* 
			 (or contenders *kb*))))
	  (when subset 
		(setq contenders subset)
		(go discrim)
		))

	(go back-out-inner)

    ask-dim (debug (format t "<<At ask-dim>>~%"))

        ;; The contenders aren't distinguished by preference.
        ;; Use overall prefs to choose which one if possible.
        ;; (!!! Perhaps this should use more specific context when
        ;;  a history mechanism has been introduced?)

        (let ((dimpref (overall-dim-preference (car discrim)
					       (cdr discrim))))
	  (when dimpref
		(if (eq :other dimpref) (go no-dim-pref)) ; !!! Probably wrong!
		(speak-compact-interp (length contenders))
		(speakf (format nil (rselect "How about ~a ~a one? "
					   "~a ~a one perhaps? "
					   "Is ~a ~a one okay? ")
			      (a-or-an (lexget dimpref (car discrim))) ; redundant computation!!!
			      (lexget dimpref (car discrim))))
		(setq *ambiguity-hint* (car discrim))
		(remember :how-about-in-dim-pref
			  :n (length contenders) 
			  :att (car discrim) 
			  :value dimpref)
		(let ((response (intrap (SAread-line))))
		  (cond ((confirm-input? response)
			 (setq *ambiguity-hint* (car discrim))
			 (setq selection (list (format nil "~a" dimpref))))
			((member (car response)
				 '("no" "n" "nope")
				 :test #'equal)
			 (when (not (null (cdr response)))
			       (setq selection response)
			       (debug (format t "Going to REFINE!~%"))
			       (go refine))
			 (speakf (rselect "Okay, so tell me more... "
					"Okay, so give me some more info..."
					"Alright, what else?"
					))
			 (remember :ask-for-more-detailed-info)
			 (setq selection (intrap (SAread-line)))
			 )
			(t (setq selection response))
			))
		(go refine)))

    no-dim-pref (debug (format t "<<At no-dim-pref>>~%"))

        ;; When there's no other way to make choices, we end up
        ;; here and have to ask the user to disciminate for us.
        ;; By getting her input on the discrim dimension.

        ;; This is just an optimization to cache the example info.
        (setq *dim-options* (mapcar #'car (cdr discrim)))

    no-dim-pref-internal (debug (format t "<<At no-dim-pref-internal>>~%"))

        (speak-compact-interp (length contenders))
        (speakf (format nil (rselect "Which ~a do you prefer? " 
				     "Which ~a do you want?"
				     "Choose a ~a to reduce the options."
				     )
			(key-phrase (car discrim))))
        (remember :ask-preference 
		  :n (length contenders)
		  :att (car discrim))
        (setq *ambiguity-hint* (car discrim)) ; prefer the current prop
        (setq selection (intrap (SAread-line)))

	;; Might be asking for more info.  Give two examples and
	;; as again.

	(when (confused-input? selection)
	      (let ((examples (n-random 2 *dim-options*)))
		(speakf (format nil (rselect "For example ~a or ~a"
					     "Maybe ~a or ~a"
					     "Perhaps ~a or ~a"
					     )
				(lexget (first examples) (car discrim) t)
				(lexget (second examples) (car discrim) t)
				)))
	      (go no-dim-pref-internal)
	      )

	;; If s/he chose something not on the list, need to
	;; confirm before going on.
	
	(when (not (member (car selection) *dim-options*
			   :test #'string-equal*))
	      (speakf (rselect "That wasn't an option and so might take us off track."
			       "I don't know that.  It might off track us."
			       "That's not one I know and might take us off track."
			       ))
	      (speakf (rselect "Is that okay?"
			       "Okay?"
			       "Alright"
			       ))
	      (if (confirm-input? (intrap (saread-line)))
		  (go refine)
		(progn (speakf "You can type a ? for examples.")
		       (go no-dim-pref-internal)
		       )))
        (go refine)

    select (debug (format t "<<At select>>~%"))

        (cond ((zerop (length contenders))
	       (speakf (rselect "Nothing left!"
				"No more items."
				"I'm out of ideas."
		       ))
             (remember :nothing-left-at-select)
	       (go expand-geographically))
	      ((= 1 (length contenders))
	       (setq selection (pop contenders))
	       (go onlyone))
	      )
	(setq contenders (sort contenders 
			       #'(lambda (a b)
				   (> (getf (cdr a) :pref)
				      (getf (cdr b) :pref)))))

	;; !!! Eventually use geographic info, or ask you what to do if there's
	;; too many to list through the speech channel.

	(speakf (format nil (rselect "I'm chooing randomly from the ~a similar choices."
				     "Let's pick one from the ~a left."
				     "There are ~a things that I can't distinguish between so I'm just choosing one randomly."
				     )
			(length contenders)))
	(setq selection (car (n-random 1 contenders)))
	(setq contenders (remove selection contenders)) ; so it isn't selected again
	(remember :randomly-selected 
		  :from (length contenders) 
		  :selection selection)
	(go onlyone)
	  
    onlyone (debug (format t "<<At onlyone>>~%"))
	  
      ;; When it's unique, come here to actually accept the selection.
      ;; User can accept or reject it.

      (remember :final-selection :selection selection)
      (display-restaraunt selection)
      (speakf (rselect "Okay?" "Alright?" "Does that work for you?"))
      (let ((result (intrap (saread-line))))
	(when (not (confirm-input? result))
	      (setq result (maybe-clear-negs result))
	      (remember :rejected-selection)
	      (cond (result ; user gave more details
		     (debug (format t "Going to refine with ~a~%" result))
		     (setq selection result)
		     (go refine))
		    (t ; some contenders left
		     (go select)))))
      (remember :accepted-selection)
       (incf (getf (cdr selection) :pref) *pref-incr*)
      (remember :new-preference
		:item-name (car selection)
		:pref (getf (cdr selection) :pref))
      (go loop)
	  ) ; prog
  )

;;; This matches words to either words or lists of words (by membership)

(defun string-equal* (w1 worl2)
  (cond ((listp worl2)
	 (member w1 worl2 :test #'string-equal))
	(t (string-equal w1 worl2))))

;;; Identify a confirmation (or not).  Has to be the first word.

(defun confirm-input? (input)
  (member (car input)
	  '("y" "yes" "okay" "ok" "sure")
	  :test #'equal))

;;; Identify the various ways that the user can tell us that s/he's 
;;; confused. 

(defun confused-input? (input)
  (dolist (word input nil)
    (if (member word '("?" "huh" "huh?" "what?" "what" "help")
		:test #'equal)
	(return t))
    ))

;;; Make a choice along a specific dimension by using global
;;; preference information if possible.  We only care if we can
;;; discriminate between the given values, so only consider ku's that
;;; have these values on the dimension in the analysis.  This works by
;;; computing a mean preference for each value.  That is, the mean of
;;; the preference of KU's that indicate this value on this dim.

(defun overall-dim-preference (dim values &aux sums maxitem maxval)
  (let ((sums (loop for value in values
		                             ;sum of prefs and n items
		    collect (list (car value) 0.0 0)
		    )))
    (loop for ku in *kb*
	  do (let ((sumentry (assoc (getf (cdr ku) dim) sums)))
	       (when sumentry
		     (incf (second sumentry) 
			   (getf (cdr ku) :pref))
		     (incf (third sumentry)))))
    (setq maxitem (caar sums) 
	  maxval (/ (second (car sums))
		    (third (car sums))))
    (dolist (sum (cdr sums))
      (let ((mean (/ (second sum) (third sum))))
	(when (> mean maxval)
	      (setq maxval mean maxitem (first sum))
	      )))
    (if (> maxval 1.0)
	maxitem
      ())
    ))

;;; Choose the value of the indicated property that goes with the contender
;;; that has the highest pref.  (Don't sort -- too slow -- just scan!)
;;; Return nil if the prefs are all the same.

(defun top-contender-val (prop contenders &aux topku toppref switches)
  (setq topku (cdar contenders))
  (setq toppref (getf topku :pref))
  (dolist (ku (cdr contenders))
    (setq ku (cdr ku)) ; remove name	  
    (if (not (= (getf ku :pref) toppref))
	(setq switches t)) ; indicate that prefs matter!
    (when (> (getf ku :pref) toppref)
	  (setq toppref (getf ku :pref))
	  (setq topku ku)))
  ;; If there were switches, then the prefs mattered.
  (if (null switches)
      () ; no pref use, return nil
    (getf topku prop) ; return the relevant prop from the top ku.
    ))

;;; Look up from lexical entries, effectively ANDing them together.
;;; Lookup the symbolic "semantics" of the words.  Compose (type
;;; . symbol) pairs from the symbols, and drop words that don't match
;;; any lexical item.  Use the current interpretation (*interp*) to
;;; fill in missing info, and change it as appropriate.

(defun chase-words (words &optional (kb *kb*) &aux old-interp)
  (setq old-interp (copy-interp *interp*)) ; for later testing
  (let ((r (catch :quit (update-interpretation words))))
    ;; If the new interpretation has replaced a term in the old one,
    ;; then we needn't bother the result will be nil.  Just return
    ;; nil and let the dialog manager deal with it.... unless it's the
    ;; whole DB being searched (which sometimes happens on a fresh feed)
    ;; in which case do it even if the interpretation is inconsistent.
    (if (eq :quit r) :quit
      (if 
	  (and words ; anything at all was found
	       (or (eq *kb* kb) ; doesn't have to be consistent if its complete
		   (consistent-interps? old-interp *interp*)))
	  (constrain-kb-by-interpretation *interp* kb))
      )))

(defun constrain-kb-by-interpretation (interp kb &aux nr)
  (debug (format t "<<Reducing ~a contenders>>~%" (length kb)))
  (debug (show-interp interp))
  (dolist (k kb) ; for each item in the database...
    (if (dolist (e interp t) ; for each interpretation entry...
	  (if (interp-entry-value e) ; if there's any value for this entry...
	      ;; Check it.
	      (let* ((prop (interp-entry-name e))
		     (ival (interp-entry-value e)) ; atomic or a list ???!!!
		     (kuval (getf (cdr k) prop)) ; the k.u.'s binding for this prop
		     )
		;; If the ku's value for this prop doesn't match,
		;; flush it, else keep it.  First ensure that each is
		;; a list.
		(if (atom ival) (setq ival (list ival))) ; could be done outside once
		(if (atom kuval) (setq kuval (list kuval)))
		;; Now see if there's any overlap.
		(if (not (any-overlap ival kuval))
		    (return nil)) ; pop out of dolist with nil -- no good entry
		)))
	;; Else, it's okay, keep it.
	(push k nr) ; otherwise, save this entry
      ) ; close if for push or not
    ) ; close dolist for all kus
  nr) ; the result is in nr

;;; Sees if any of the atoms in the lists are the same.

(defun any-overlap (a b)
  (dolist (ai a nil)
    (if (member ai b) (return t))))
                   
;;; Get the lexical item that a word refers to.  NOTE: Through the
;;; total magic of string-equal equating atoms with the same pname as
;;; their strings, this actually works whether you give it an atom or
;;; a string!  This is also where ambiguity conflicts are resolved.
;;; If there is an ambiguity hint, that it used (if it helps) otherwise,
;;; we ask.  

(defun locate-word-lu (word)
  (let ((le* (find-all-lentries word)))
    (cond ((null le*)
	   (speakf (format nil (rselect "I don't know what ~a means." 
					"~a???"
					"~a isn't a word I know."
					)
					word))
	   (let ((similar-words (find-similar-words word)))
	     (when similar-words
		   (speakf (format nil (rselect "You might mean ~a." 
						"Maybe you mean ~a."
						"Could you mean ~a?")
			   (sayable-list (mapcar #'car similar-words))))))
	   ())
	  ((null (cdr le*)) ; unambiguous
	   (copy-list (cdar le*)))
	  (t ; ambiguous
	   (if *ambiguity-hint*
	       (let ((which (find *ambiguity-hint* le*
			       :test #'(lambda (a b) (equal a (second b))))))
		 (or (copy-list (cdr which))
		     (disambiguate le*)))
	     (disambiguate le*)))
	  )))

;;; Turns (a b c) in "a, b, or c"  Atoms are left as-is.

(defun sayable-list (l)
  (cond ((or (stringp l) (atom l)) l)
	((null (cdr l)) (format nil "~a" (car l)))
	(t (sayable-list-2 l))))
(defun sayable-list-2 (l)
  (cond ((null (cdr l))
	 (format nil "or ~a" (car l)))
	(t (format nil "~a, ~a" (car l)
		   (sayable-list-2 (cdr l))))))

(defun disambiguate (le*)
  (speakf (format nil "~a is ambiguous. " (caar le*)))
  (dolist (le le*)
    (speakf (format nil "If you mean the ~a (say ~a), " 
		    (key-phrase (second le))
		    (second le)
		    ))
    )
  (speakf (format nil "or none of these? (say NONE or OTHER) "))
  (let* ((in (car (SAread-line)))
	 (which (find in le* :test #'(lambda (a b)
				       (string-equal a (second b)))))
	 )
    (cond ((member in '("none" "no" "other" "n") :test #'string-equal)
	   ())
	  ((member in '("quit" "stop" "!") :test #'string-equal)
	   (throw :quit :quit))
	  ((null which)
	   (speakf "Come again?")
	   (disambiguate le*))
	  (t (copy-list (cdr which)))
	  ))
  )

;;; !!! Mishandles list "words" here !!!

(defun find-all-lentries (word)
  (if (listp word) ; !!! THIS IS WRONG !!!
      (setq word (car word)))
  (loop for entry in *lex*
        if (string-equal word (car entry))
	collect entry))

;;; Finds words in the lexicon that are likely to be mis-spellings
;;; of the target word.

(defun find-similar-words (w)
  (loop for entry in *lex*
        if (string-similar w (car entry))
	collect entry))

(defun string-similar (w1 w2)
  (if (not (stringp w1)) (setq w1 (format nil "~a" w1)))
  (if (not (stringp w2)) (setq w2 (format nil "~a" w2)))
  (and (char-equal (aref w1 0) (aref w2 0))
       (>= 2 (abs (- (length w1) (length w2))))
       (< (/ (length w1) 2)
	  (count-similar-letters w1 w2))))

(defun count-similar-letters (w1 w2)
  (loop for i from 0 to (1- (min (length w1) (length w2)))
	sum (if (char-equal (aref w1 i) (aref w2 i)) 1 0)))
  
(defun kbanyof (l1 l2)
  (dolist (e1 l1 nil)
    (if (member e1 l2) (return t))))

;;; Interpretation management.

;;; Counts non-nil and non-location entries.

(defun count-non-nil-interp-entries (interp &aux (k 0))
  (dolist (e interp k)
    (if (and (interp-entry-value e)
	     (not (eq :city (interp-entry-name e))))
	(incf k))))

(defun copy-interp (i)
  (mapcar #'copy-interp-entry i))

;;; The interpretation is an alist formed from the *frame*.

(defun clear-interpretation ()
  (setq *interp* ())
  (dolist (i *frame*)
    (push (make-interp-entry :name (car i) 
			     :value nil
			     :set-time (get-universal-time)
			     )
	  *interp*))
  )

;;; To update the interpretation, take (slot value)* and replace all
;;; of those in the *frame*.    

(defun update-interpretation (words)
  (dolist (item (pairify (mapcan #'locate-word-lu words)))
    (let ((frame-entry (find-interp-entry-by-name (car item) *interp*)))
      (cond ((null frame-entry)
	     (break "~a isn't a valid slot/filler." item))
	    ((interp-entry-value frame-entry) ; just for debugging
	     (debug (format t "<<Replacing ~a with ~a.>>~%" frame-entry item)))
	    )
      (setf (interp-entry-value frame-entry) (cadr item))
      (setf (interp-entry-set-time frame-entry) (get-universal-time))
      ))
  (remember :update-interp :words words :new-interp (copy-interp *interp*))
  )

(defun find-interp-entry-by-name (name interp)
  (find name interp :test #'(lambda (a b) (eq a (interp-entry-name b)))))

;;; This isn't done by setf so that we can update the set-time.

(defun set-interp-entry-value (e newval)
  (remember :set-interp-entry-value
	    :entry (copy-interp-entry e)
	    :newval newval)
  (debug (format t "<<Reset ~a to ~a>>~%" e newval))
  (setf (interp-entry-value e) newval)
  (setf (interp-entry-set-time e) (get-universal-time))
  )

;;; Back out selection combines the age of the entry with the
;;; :back-out-importance to choose an entry to remove.  It should
;;; always return an interp-entry that can validly be backed out.

(defun find-interp-entry-to-back-out ()
  (let ((l (sort (copy-list *interp*)
		 #'(lambda (a b)
		     ;; Reverse sort so that important ones aren't selected.
		     (< (interp-entry-importance a)
			(interp-entry-importance b))))))
    ;; l is now sorted to put the most imporant toward the front.
    ;; filter out the :city and things with nil values.
    (setq l (remove t l 
		    :test #'(lambda (ignore e)
			      (or (eq :city (interp-entry-name e))
				  (null (interp-entry-value e))))))
    ;; Okay, no we have to choose.
    (cond ((null l) (break "Nothing left to remove.  Shouldn't happen!"))
	  ((null (cdr l)) (car l)) ; only one, must be the one!
	  ;; If the top two have the same values, choose the oldest
	  ;; otherwise, choose at random.
	  ((= (interp-entry-importance (first l))
	      (interp-entry-importance (second l)))
	   (debug (format t "~a and ~a have the same importance~%" (first l) (second l)))
	   (cond ((> (interp-entry-set-time (first l))
		     (interp-entry-set-time (second l)))
		  (second l))
		 ((< (interp-entry-set-time (first l))
		     (interp-entry-set-time (second l)))
		  (first l))
		 (t (if (zerop (random 2)) (first l) (second l)))
		 ))
	  ;; If they aren't equal, the reverse sort will have made the least the car
	  (t (car l))
	  )))

(defun interp-entry-importance (e)
  (algetf :back-out-importance (interp-entry-name e) *frame*))

#| dead code 971030 related to back out based upon age.

;;; Use the set times to find the oldest set NON-NIL entry.  Note that 
;;; this may return nil if there aren't any with values.  Also skip
;;; cities, since they are never backed out.

(defun find-oldest-interp-entry (&optional (interp *interp*) &aux e0)
  ;; First locate a non-null entry that isn't the :city
  (dolist (e interp)
     (when (and (interp-entry-value e)
	      (not (eq :city (interp-entry-name e))))
	   (setq e0 e)
	   (return t)))
  ;; Now if there's anything there at all,  try to beat it.
  (if e0
      (dolist (e interp)
	(if (and (not (eq :city (interp-entry-name e)))
		 (< (interp-entry-set-time e)
		    (interp-entry-set-time e0)))
	    (setq e0 e))))
  e0)

|#

;;; Interpretations are consistent when they have no particular elements
;;; that differ.  Nil in one and not the other is okay.

(defun consistent-interps? (i1 i2)
  (dolist (e1 i1 t)
    (let* ((v1 (interp-entry-value e1))
	   (e2 (find-interp-entry-by-name (interp-entry-name e1) i2))
	   (v2 (interp-entry-value e2))
	   )
      (cond ((null v1) t)
	    ((null v2) t)
	    ((not (equal v1 v2))
	     (return nil))
	    )
      )))

;;; What at/vals differentiate the given knowledge units?  It returns an alist
;;; of alists, as: (...(attribute ...(value name name name name ...)...)...)

(defun discriminate (kus &aux r)
  (dolist (ku kus)
    (prog (att val old name)
           (setq name (pop ku))
      loop (if (null ku) (return nil))
           (setq att (pop ku))
           (setq val (pop ku))
	   ;; Old is the current discrimination set for this attribute.
           (setq old (assoc att r))
           (if (not old)
	       ;; Nothing yet, simply put the current value on the list.
	       (progn
		 (push (setq old (list att (list val name))) r)
		 (go loop)))
           (let ((vl (assoc val (cdr old) :test #'discrim-val-equal)))
             (if vl
               (push name (cdr vl))
               (push (list val name) (cdr old))
               ))
           (go loop)
           ))
  r)

;;; Accepts set equality. (Maybe should take subset equality as well?)

(defun discrim-val-equal (a b)
  (or (equal a b)
      (if (and (listp a) (listp b))
	  (set-equal a b)
	)))

;;; Select a best discriminator based upon user prefs and the *frame*
;;; which serves as the default ordering list.  

;;; Filter out anything that doesn't discriminate, and report a
;;; discrimination level for each one that does.  The value cons'ed
;;; onto each tells us both how good it discrimnates, COMBINED with
;;; its pref order in the frame, used as a discrimination ordering
;;; list.  This will return the first if there's only one, or nil if
;;; there aren't any left.

(defun best-discriminator (discrims)
  (cdar (sort (mapcan #'evaluate discrims)
	     #'(lambda (a b) (> (car a) (car b))))))

;;; Here's the heart of the discrimination algorithm.  If there's no
;;; discrimination at all made, return nil and the mapcan will loose
;;; it.  Otherwise, cons on a value that is combined from the *frame*
;;; value for the property (if any 1.0 otherwise) and the
;;; discrimination level.  The evaluation tells us 

(defconstant *default-frame-order* 1.0)

(defun evaluate (d)
  (let ((prop (pop d)))
   (if (not (member prop '(:pref :crit)))
    (if (not (cdr d)) ; only one value
	nil ; junk this one.
      ;; Combine the *frame* value for the prop with the discrim utility.
      (list (cons
	     (* (or (algetf :discrim-weight prop *frame*)
		    *default-frame-order*)
		(ninfo (mapcar #'length d))
		)
	     (cons prop d)))
      ))
    ))

;;; Utils

(defun algetf (prop key alist)
  (getf (assocdr key alist) prop))

(defun assocdr (key alist)
  (cdr (assoc key alist)))

;;; Quinlan's gain ratio analysis, from the C4.5 book (pp. 20-22).
;;; S is the set of cases.
;;; Ti are the n subsets into which S is partitioned.
;;; Ci is the ith class.

;;; Given a set (S) and a number of class-based subsets:
;;;  (nth i C) -> (class-name item item item...), return the
;;; information as per Quinlan p.21 bottom equation.

(defun info (S C &aux (info 0.0))
  (- (dotimes (i (length C) info)
       (let* ((Ci (nth i C))
	      (nCi (length Ci))
	      (nS (length S))
	      )
	 (incf info
	       (* (/ nCi nS)
		  (log2 (/ nCi nS))))
	 ))
     ))

(defun log2 (n)
  (/ (log n)
     (log 2)))

;;; Same thing but this time just from a list of lengths.

(defun ninfo (C &aux (info 0.0) (nS (sumup C)))
  (- (dotimes (i (length C) info)
       (let* ((Ci (nth i C))
	      (nCi Ci)
	      )
	 (incf info
	       (* (/ nCi nS)
		  (log2 (/ nCi nS))))
	 ))
     ))

(defun sumup (l* &aux (sum 0.0))
  (dolist (l l* sum)
    (incf sum l)))

;;; Random utils.

;;; Xlate spaces to dashes in a string

(defun space- (string) (substitute #\- #\space string))

;;; Programmer convenience.  (It'd be even more so if I could find the
;;; latest version automagically!!!)

(defun cl ()
  (compile-file (format nil "RAdvisor.lisp"))
  (load "RAdvisor"))

;;; Interface code for SpeechActs

(defun xlate-SA-string-to-expr (string)
  (read-from-string (format nil "~a" (replace-illegal-and-unclean-chars string))))

(defun replace-illegal-and-unclean-chars (string)
  (nsubstitute #\- #\: string)
  (nsubstitute #\( #\[ string)
  (nsubstitute #\) #\] string)
  (nsubstitute #\. #\, string)
  (nsubstitute #\- #\_ string)
  string
)

(defun speakf (s)
  (remember :speakf :sentence s)
  (case *interactor*
     (:saexec (format t "(]MSG \"\" (SPEAK \"~a\"))~%" s))
     (:salisp (speakf-for-lisp s))
     (t (when (not *debug*) (sleep 1) (format t "~c" #\bell))
	(format t "---~%--- ~a~%---~%" s)
	)
     ))

;;; Speakf for direct lisp loading into speechacts.

(defun speakf-for-lisp (stringexpr &rest args)
  "lets you use an expression in the string portion of speak ..
      replace speak with this when it's good and tidy"
  (let ((qargs (loop for a in args collect (list 'quote a))))
    (send-pcommand *tts*
                   (string-append "SPEAK \""
                                  (eval `(format nil ,stringexpr .,  qargs))
                                  "\""))))

(defun SAread-line ()
  (let ((input
          (case *interactor*
              (:salisp (break)) ; maybe like: (pop *sa-input-stack*))
              (:saexec (let ((in1 (parse-sa-input (xlate-SA-string-to-expr ([read-lines])))))
			 (speakf in1)
			 in1))
              (t (format t "Awaiting input: ")
		 (strip-ignore-words (parse-string (read-line))))
           )
         ))
       (remember :input :input input)
       (if (member *interactor* '(:salisp :saexec)) (speakf "Okay."))
       input
     ))

(defun strip-ignore-words (l)
  (loop for w in l
	if (not (member w *ignore-words* :test #'string-equal))
	collect w))

;;; This replaces read for speechacts inputs.  It balances
;;; brackets, and then returns the whole input as a long string.

(defun [read-lines] (&aux (collected-string "") ([k] 0))
  (prog (newline)
    get-another-line
      (setq newline (read-line))
      (setq collected-string (format nil "~a ~a" collected-string newline))
      (incf [k] (count #\[ newline))
      (decf [k] (count #\] newline))
      (if (< [k] 0) (setq [k] 0))
      (if (zerop [k]) (return collected-string))
      (go get-another-line)))

;;; Create an image for SpeechActs.

(defun make-apa ()
  (compile-file "RAdvisor")
  (load "RAdvisor")   
  (setq *interactor* :saexec)
  (if (probe-file "apa")
      (delete-file "apa"))
  (disksave "apa" :restart-function #'apa)
  (quit)
  )

;;; Parse the SpeechActs input form.  We are looking for the bindings
;;; for certain specific items, either at top level or embedded within
;;; other items.  We getf down the line to find the required items and
;;; then return an alist with the values bound to the head of each
;;; locator list.

(setq *sa-locators* 
  '(  
      ;; General:

    (COMMAND COMMAND-)
    (WORD WORD-)
    (SEM SEM-)
    (CONFIRM_RESULT CONFIRM-RESULT-) 

    ;; Simple specific:
    
    (LOCATION LOCATION-)
    (LOCATION-NAME LOCATION-NAME-)
    (FOODCAT FOODCAT-)
    (FOODCAT-NAME FOODCAT-NAME-)
    (PLACE-TYPE PLACE-TYPE-)
    (PLACE-TYPE-NAME PLACE-TYPE-NAME-)
    (SERVICE-TYPE SERVICE-TYPE-)

    ;; Within REST-TYPE specific:
    
    (RESTCAT REST-TYPE- RESTCAT-)
    (RESTCAT-NAME REST-TYPE- RESTCAT-NAME-)
    (PLACE-TYPE REST-TYPE- PLACE-TYPE-)
    (PLACE-TYPE-NAME REST-TYPE- PLACE-TYPE-NAME-)
    ))

(defun parse-sa-input (l)
  (mapcar #'(lambda (locator) 
              (cons (car locator)
                    (recursively-extract (cdr locator) l)))
          *sa-locators*))

(defun recursively-extract (loc l)
  (let ((entry (getf l (car loc))))
    (cond ((null loc) l)
          (entry (recursively-extract (cdr loc) entry))
          (t nil)
     )))

;;; Simple Geogragphical locator uses the Alhpa/Numeric keys on the 
;;; AAA California Map (96-11).  

(defun current-location ()
  (interp-entry-value (find-interp-entry-by-name :city *interp*)))

;;; There are two maps at different scales: The larger (numerics >=
;;; 30) and the smaller (numerics < 30).  We indicate their square
;;; measures here in miles.

(defvar *large-scale* 25) ; 25 miles per block
(defvar *small-scale* 9)

;;; Estimate the milage between two locations using the right scale.
;;; There are a couple of problems here.  First, if the locs aren't on
;;; compatible scales, we can't compare them, whereas maybe what
;;; really should happen is to xlate the smaller scale into it's
;;; approx. loc on the larger, or vv.  Also, there's a problem
;;; interpreting the Bay Area small scale from the LA Area small scale.
;;; At the moment, none of these is a show stopper.

(defun distance-between-cities (c1 c2)
  (let ((c1large (extract-location c1 #'>=30))
	(c1small (extract-location c1 #'<30))
	(c2large (extract-location c2 #'>=30))
	(c2small (extract-location c2 #'<30))
	)
    ;; Small scale is more accurate; always prefer it.
    (if (and c1small c2small)
	(city-distance c1small c2small *small-scale*)
      (if (and c1large c2large)
	  (city-distance c1large c2large *large-scale*)
	))
    ))

(defun extract-location (c test)
  (let ((entry (cdr (assoc c *citylocs*))))
    (if entry
	(let ((x1 (pop entry))
	      (y1 (pop entry))
	      (x2 (pop entry))
	      (y2 (pop entry)))
	  ;; There's always a first one -- see if it's the one we want.
	  (if (funcall test y1)
	      (cons (alpha-to-num x1) y1)
	    ;; Else, it *might* be the other, or maybe not -- in which
	    ;; case we return nil.
	    (if (and y2 (funcall test y2))
		(cons (alpha-to-num x2) y2)
	      nil)
	    )
	  ))))
	  
(defun >=30 (l) (>= l 30))
(defun <30 (l) (< l 30))

(defun alpha-to-num (letter &aux result)
  (loop for l in '(a b c d e f g h i j k l m n o p)
	as n from 1
	if (equal l letter)
	do (setq result n)
	)
  result)

;;; The structures here were formed above, and are: (x . y); conversion
;;; to numeric has already been done.

(defun city-distance (c1 c2 scale-in-miles)
  (* scale-in-miles 
     (sqrt (+ (expt (- (car c1) (car c2)) 2)
	      (expt (- (cdr c1) (cdr c2)) 2)))))

;;; Sorts places by their distance to the target ; exclude ourselves
;;; from the list.

(defun nearby-cities (c)
  (let* ((c1 (assoc c *citylocs*))
	 (cities (loop for c2 in *citylocs*
		              ;; UUU This is doing unneccessary assocs.
		       as d = (distance-between-cities (car c1) (car c2))
		       if (and d (not (eq (car c1) (car c2))))
		       collect (cons (car c2) d)
		       )))
    (sort cities #'(lambda (a b) (< (cdr a) (cdr b))))
    ))

;;; Choose randomly from among the three closest cities.

(defun find-a-nearby-city (now-at exclude)
  (let ((c* (nearby-cities now-at)))
    (loop for c in exclude
	  do (setq c* (remove c c* :test #'(lambda (a b) (eq a (car b))))))
    (caar (n-random 1 (first-n 3 c*)))))

;;;

(defun show-interp (&optional (i *interp*))
  (format t "~%~%<<Interpretation:~%")
  (dolist (e i)
    (let ((n (interp-entry-name e))
	  (v (interp-entry-value e))
	  (tm (interp-entry-set-time e)))
      (when v ; skip null entries
	 (format t "  ~a = ~a (@~a)~%" n v tm))
      ))
  (format t "  >>~%")
  )

;;; History list handling.  Note that the fullhistory appears in
;;; REVERSE chron order even though each entry is in FORWARD order.

(defun remember (indicator &rest values)
   (setq *moment* (append *moment* (list (cons indicator (copy-tree values)))))
   "Someone tried to use the results of a call to Remember!")

;;; Save the current history info in the permanent database, and
;;; update the internal complete history as well.  Note that these are
;;; saved as PUSH exprs to the fullhistory, and will appear in CORRECT
;;; order in the file, since they are appended meaning that they'll
;;; come back in REVERSE order care of the pushes in fullhistory.
;;; Anyhow, they all have time stamps.

(defun save-history ()
  (let ((*print-length* ()))
    (with-open-file (f *histfile* :direction :output 
		       :if-exists :append
		       :if-does-not-exist :create)
      (format f "~%~%;;;-----~%~%(push '(~%")
      (dolist (event *moment*)
	      (format f "     ~s~%" event))
      (format f ") *fullhistory*)~%")
      )
    (push *moment* *fullhistory*)
    (setq *moment* nil)
    ))

;;; In principle there ought to be special-purpose information stored
;;; for these instead of reading through the history all the time.
;;; But at the moment, since this is exploratory, we do it the hard
;;; way, and a little bit of caching of serch results helps with
;;; speed. !!!

(defun maybe-restore-history ()
  (if (not (boundp '*fullhistory*)) 
      (setq *fullhistory* ()))
  (when (and (null *fullhistory*) (probe-file *histfile*))
	(load *histfile*)
	(format t ";;; The history contains ~a entries. ~%"
			(length *fullhistory*))
	))

;;; One of the most important functions of the history is to restore
;;; the user's selected preferences for database items.  There will
;;; eventually be a nicer way to do this than having to scan the whole
;;; damned history stack, but for the moment, this will do.

(defun restore-preferences ()
  (loop for story in *fullhistory*
	do
	(loop for event in story
	      do 
	      (case (car event)
		 (:new-preference
		  (let ((item-name (getf (cdr event) :item-name))
			(new-pref (getf (cdr event) :pref)))
		    (debug (format t ";;; Reseting preference for ~s to ~a~%"
			    item-name new-pref))
		    (setf (getf (cdr (assoc item-name *kb* :test #'equal)) :pref)
			  new-pref)
		    )))
	      )))

;;; Tell stories from the history.

(setq y nil) ; in case the user types a whole series of "Y"....

(defun stories (&optional long)
  (setq *fullhistory* ())
  (maybe-restore-history)
  (loop for story in *fullhistory*
	do
	(tell story long)
	(format t "~%~%")
	(if (not (y-or-n-p "Next previous dialog? "))
	    (return 'done)))
  'done)

(defun tell (story &optional long)
  (if (numberp (caar story))
      (format t "~%~%----- Dialog of ~a (~a) -----~%~%" 
	      (caar story) (pretty-time (cadar story)))
    (format t "~%~%----- (fragment) -----~%~%")
    )
  (loop for event in (cdr story)
	do 
	(if long
	    (print event)
	  (case (car event)
		(:set-location (format t "      (loc = ~a)~%" 
				       (third event)))
		(:input (format t "user: ~a~%" (third event)))
		(:speakf (format t " apa: ~a~%" (third event)))
		))))

;;; Explaining is more complicated since it's intended to be
;;; given to the user when they type "how did you get that?" etc.

(defun explain-history (moment)
  (do ((events moment (cdr events)))
      ((null events))
    (let* ((event (car events))
	   (key (car event))
	   (info (cdr event)))
      (case key
	(:refine
	 (let ((words (getf info :selection)))
	   (if words
	       (speakf (format nil "We refined the set with these terms: ~a" words)))))
	(:input 
	 (speakf (format nil "You said: ~a" (getf info :input))))
	(:randomly-selected 
	 (speakf (format nil "and I randomly selected from among ~a items."
		 (getf info :from))))
	((:how-about :how-about-in-dim-pref)
	 (speakf (format nil "With ~a possible choices, I suggested the ~a ~a."
			 (getf info :n)
			 (key-phrase (getf info :att))
			 (getf info :value)
			 )))
	(:set-interp-entry-value
	 (let* ((e (getf info :entry))
		(name (interp-entry-name e))
		(oldval (interp-entry-value e))
		(newval (getf info :newval)))
	   (cond ((null oldval)
		  (speakf (format nil "We set the ~a to ~a"
			   (key-phrase name)
			   (lexget newval name))))
		 ((null newval)
		  (speakf (format nil "We drop the ~a, which was formerly ~a" 
			   (key-phrase name)
			   (lexget oldval name))))
		 (t 
		  (speakf (format nil "We changed the ~a from ~a to ~a"
			   (key-phrase name)
			   (lexget oldval name)
			   (lexget newval name)))))
	   ))
	(:nothing-matches-at-main-lookup
	 (speakf "I couldn't find anything to match the request."))
	(:ask-preference
	 (speakf (format nil "With ~a possible choices, I asked which ~a you'd like."
			 (getf info :n)
			 (cadr (assoc (getf info :att) *key-phrases*))
			 )))
	)))
  (speakf "In the current interpretation,")
  (dolist (e *interp*)
      (let ((name (interp-entry-name e))
	    (value (interp-entry-value e)))
	(when value
	      (speakf (format nil " the ~a is ~a,"
			      (key-phrase name)
			      (lexget value name))))))
  )

;;; 

(defun key-phrase (key)
  (second (assoc key *key-phrases*)))

;;;

(defun set-location (location)
  (set-interp-entry-value (find-interp-entry-by-name :city *interp*) location)
  (remember :set-location :location location))

;;; Chooses randomly among the args, used mainly to vary conversational text.

(defun rselect (&rest l)
  (car (n-random 1 l)))

;;; Used to contextualize choices.

(defun liget (att &optional (i *interp*))
  (let ((v (interp-entry-value (find-interp-entry-by-name att i))))
    (if v (lexget v att))))

(defun speak-compact-interp (n) 
  (let* ((city (liget :city))
	 (cost (liget :cost))
	 (nationality (liget :nationality))
	 (service (liget :service))
	 )
    (cond 
     ((and city cost nationality service)
      (speakf (format nil "~a ~a ~aly priced ~a ~a places in ~a..."
		      (rselect "Of the" "Among" "Of") n cost nationality service city)))
     ((and city nationality service)
      (speakf (format nil "~a ~a ~a ~a places in ~a..."
		      (rselect "Of the" "Among" "Of") n nationality service city)))
     ((and city cost service)
      (speakf (format nil "~a ~a ~aly priced ~a places in ~a..."
		      (rselect "Of the" "Among" "Of") n cost service city)))
     ((and city cost nationality)
      (speakf (format nil "~a ~a ~aly priced ~a places in ~a..."
		      (rselect "Of the" "Among" "Of") n cost nationality city)))
     ((and city nationality)
      (speakf (format nil "~a ~a ~a places in ~a..."
		      (rselect "Of the" "Among" "Of") n nationality city)))
     ((and city cost)
      (speakf (format nil "~a ~a ~aly priced places in ~a..."
		      (rselect "Of the" "Among" "Of") n cost city)))
     ((and city service)
      (speakf (format nil "~a ~a ~a places in ~a..."
		      (rselect "Of the" "Among" "Of") n service city)))
     ((and city)
      (speakf (format nil "~a ~a places in ~a..."
		      (rselect "Of the" "Among" "Of") n city)))
     )))

(defun a-or-an (word)
  (if (member (aref word 0) '(#\a #\e #\i #\o #\u #\y #\A #\E #\I #\O #\U #\Y)
	      :test #'char-equal)
      "an" "a"))