;;; (load (compile-file "leocrontab.lisp"))

(defstruct event hour min day msg btn)
(defparameter *events*
  (list
   ;; WWW Can't use apostrophies in messages or buttons!
   (make-event :min 00 :hour 15 :day 'mon  :msg "Its 15:00 on Monday!" :btn "Yeah, I know!")
   (make-event :min 10 :hour 15 :day 'mon  :msg "Its 15:10 on Monday!" :btn "Gotcha!")
   ))

(defun run ()
  (with-open-file
   (o "/tmp/leo.crons" :direction :output :if-exists :supersede)
   (loop for event in *events*
	 ;; Minute Hour Day(month) Month Day(of week)
	 ;; Day of week can be: mon tue wed thu fri sat sun
	 do (format o "~a ~a * * * ~a osascript -e 'tell application (path to frontmost application as text) to display dialog ~s buttons {~s} with icon stop'~%"
		    (event-min event) 
		    (event-hour event) 
		    (event-day event) 
		    (event-msg event) 
		    (event-btn event)
		    ))))

(run)
		    
