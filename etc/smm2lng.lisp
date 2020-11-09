(defparameter *structs*
  '((:number :adj :enemies "in" "the" :location)
    (:number :enemies "in" "the" :location)
    (:number :adj :enemies)
    (:enemy :location)
    (:adj :location)))

(defparameter *words*
  '((:adj "Big" "Small" "Creepy" "Dark" "Scary")
    (:enemy "Boo" "Koopa" "Buzzy Beetle" "Yoshi" "Red Coin" "Platform")
    (:enemies "Boos" "Koopas" "Buzzy Beetles" "Yoshis" "Red Coins" "Platforms")
    (:number "Two" "Three" "Four" "Five" "Six" "Seven")
    (:location "Cavern" "Castle" "Field" "Summit")))

(defun gen-name ()
  (loop for element in (nth (random (length *structs*)) *structs*)
	as vals = (rest (assoc element *words*))
	collect
	(if (null vals) element
	  (nth (random (length vals)) vals))))

(defun gen-names (&optional (n 10))
  (loop for i below n
	do (format t "~{~a~^ ~}~%" (gen-name))))
