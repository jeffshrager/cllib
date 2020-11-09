;;; (load (compile-file "wifi.lisp"))

(defun run ()
  (loop for i below 10
	do (print (pingem))))

(defun pingem ()
  (list
   (pingit "ping -c 1 1.1.1.1" :onesx4)
   (pingit "ping -c 1 www.google.com" :google)
   (pingit "ping -c 1 192.168.1.254" :router)
   (pingit "ping -c 1 facebook.com" :fb)
   (pingit "ping -c 1 www.apple.com" :apple)
   ))

(defun pingit (pingcmd key)
  (let* ((string (ignore-errors (uiop:run-program pingcmd :output :string))))
    (when string
      (let* ((a (+ 6 (search " time=" string)))
	     (b (search " " string :start2 a)))
	(read-from-string (subseq string a b))))))

(run)

