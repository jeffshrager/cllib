;;; Copyright (c) 1996 by The University of Pittsburgh
;;;                       Psychological Software Tools, Inc.
;;;                       Jeff Shrager

;;; --- This module controls all aspects of palettes, including the
;;; creation of palettes and the management of palette space.  Each
;;; context has a local palette directory (called pals).
;;; Unsurprisingly, this contains palettes.  When you first create a
;;; context, a set of default palettes are copied in for you.  After
;;; that, you need to use the pal command to include, exclude, edit,
;;; or create additional palettes.

(defun include-default-palettes (where)
  (system! "cp ~a ~a" (homefile "defpals/*") where)
  )

(defun reload-palettes ()
  (format t "Recopying all default palettes into current result set will overwrite any changes you've made to palettes there.~%")
  (if (y-or-n-p "Do you really want to do this?")
      (system! "cp ~a ~a" (homefile "defpals/*") 
	       (pal-dir-or-default)))
  )

;;; --- The pal command distributor.

(defun pal (args)
  (case (pop args)
    ((l list)
     (list-pals))
    ((d des desc describe)
     (describe-palette (car args)))
    ((r reset recopy)
     (reload-palettes))
    ((c create m make n new)
     (create-new-pal args))
    (t (abort-command "Unknown palette command."))
    ))

;;; Currently the only sort of palette that can be created is a rainbow
;;; palette at 128 some even submultiple of 128.  It is created by 
;;; chopping up the *r128pal* var.

(defun create-new-pal (args)
  (let ((low (first args))
	(high (second args))
	(ncols (third args)))
    (if (or (not (numberp high))
	    (not (numberp low))
	    (not (numberp ncols)))
	(abort-command "Palette low, high, and number of colors must all be given and must all be numerical."))
    (if (not (zerop (mod 128 ncols)))
	(abort-command "The number of colors must be an even divisor of 128 (e.g., 128, 64, 32, etc.)"))
    (let* ((pname (format nil "rainbow~a.~ato~a.pal" ncols low high))
	   (skip (/ 128 ncols))
	   (colorset (copy-list *r128pal*))
	   (val low)
	   (val-delta (/ (- high low) (1- ncols)))
	   )
      (format t "Creating ~a in the current result's palette directory.~%" pname)
      (with-open-file (f (pfn (pal-dir-or-default) pname) :direction :output
			 :if-exists :supersede)
	(format f "color~%")
	(dotimes (i ncols)
	  (format f "~a ~a ~a ~a 0 0 0~%" 
		  (round val)
		  (first (car colorset))
		  (second (car colorset))
		  (third (car colorset)))
	  (incf val val-delta)
	  (dotimes (i skip) (pop colorset))
	  ) ;dotimes
	) ;with-open-file
      ));let*let
  )

;;; --- List the files in the palette directory.

(defun list-pals ()
  (system! "ls ~a" (pfn (pal-dir-or-default) "*")))

;;; --- Describe a palette; this is useful in logging what the colors
;;; in a printed picture mean.  I'm not actually sure of how this
;;; ought to work.  There must be some theoretical way of doing this,
;;; but I've simply done it empirically.  Note that the name of the
;;; palette has to be given as a quoted string.

(defun describe-palette (palname &aux curcol curlim)
  (let ((pal (find-and-load-palette palname)))
    (format t "Description of ~a:~%" palname)
    (dolist (l pal)
      (let ((items (mapcar #'read-from-string (parse-string l))))
	(if (all-are-numeric? items)
	    (let ((limit (car items))
		  (color (get-color-name (second items) 
					 (third items) 
					 (fourth items))))
	      (if (not (equal curcol color))
		  (progn (print (list limit color))
			 (setq curcol color curlim limit))
		)
	      )
	  )))))


(defun all-are-numeric? (l)
  (dolist (i l t)
    (if (not (numberp i)) (return nil))))

(defun find-and-load-palette (pal)
  (if (not (stringp pal))
      (abort-command "The palette name must be quoted to preserve case."))
  (let ((palfile (pfn (pal-dir-or-default)
		      (format nil "~a.pal" pal))))
    (if (file-exists? palfile)
	(read-lines-from-file palfile)
        (abort-command "No such palette!")
	)))

;;; Palette description isn't too easy, actually.  We figure out the
;;; combination and then just look it up on a chart.

(defvar *coltrans* ; these are the red, green, and blue respectively
   '(((- - -) black) ; - means under 126, + means over.
     ((+ - -) red)
     ((- + -) green)
     ((- - +) blue)
     ((+ + -) yellow)
     ((- + +) Light-Blue)
     ((+ - +) Purple)
     ((+ + +) White)))

(defun get-color-name (r g b)
  (cadr (assoc (list (cdt r) (cdt g) (cdt b)) *coltrans* :test #'equal)))

(defun cdt (cv) ; color density transform (to + -)
  (if (> cv 126) '+ '-))

;;; Definitions for the 128 palette. Other palettes are computed from
;;; this one.

(defvar *r128pal*
  '(
    (255 255 255)
    (132 0   255)
    (127 0   255)
    (122 0   255)
    (116 0   254)
    (111 0   254)
    (102 0   254)
    (91  0   254)
    (77  0   254)
    (62  0   254)
    (45  0   254)
    (27  0   254)
    (9   0   254)
    (0   6   254)
    (0   20  254)
    (0   33  254)
    (0   44  254)
    (0   55  254)
    (0   65  254)
    (0   75  255)
    (0   86  255)
    (0   97  255)
    (0   108 255)
    (0   121 255)
    (0   133 254)
    (0   144 251)
    (0   153 247)
    (0   163 242)
    (0   174 236)
    (0   188 230)
    (0   203 223)
    (0   216 216)
    (0   227 207)
    (0   236 197)
    (0   244 187)
    (0   251 175)
    (0   254 160)
    (0   254 144)
    (0   254 131)
    (0   254 119)
    (0   254 108)
    (0   254 98 )
    (0   254 90 )
    (0   254 82 )
    (0   254 74 )
    (0   254 67 )
    (0   254 59 )
    (0   254 51 )
    (0   254 44 )
    (0   254 36 )
    (0   254 27 )
    (0   254 18 )
    (0   254 9  )
    (0   254 1  )
    (11  254 0  )
    (23  254 0  )
    (36  254 0  )
    (50  254 0  )
    (65  254 0  )
    (80  254 1  )
    (94  254 1  )
    (106 254 0  )
    (120 254 0  )
    (133 254 0  )
    (146 254 0  )
    (158 254 0  )
    (170 254 0  )
    (180 254 0  )
    (187 254 0  )
    (192 254 0  )
    (198 254 0  )
    (203 254 0  )
    (209 254 0  )
    (215 254 0  )
    (220 254 0  )
    (225 254 0  )
    (231 254 0  )
    (236 254 0  )
    (241 253 0  )
    (246 251 0  )
    (250 245 0  )
    (253 241 0  )
    (254 235 0  )
    (254 231 0  )
    (254 229 0  )
    (254 227 0  )
    (254 221 0  )
    (254 217 0  )
    (254 213 0  )
    (254 207 0  )
    (254 201 0  )
    (254 197 0  )
    (254 191 0  )
    (254 185 0  )
    (254 179 0  )
    (254 171 0  )
    (254 165 0  )
    (254 159 0  )
    (254 155 0  )
    (254 147 0  )
    (254 141 0  )
    (254 138 0  )
    (254 135 0  )
    (254 131 0  )
    (254 124 0  )
    (254 122 0  )
    (254 118 0  )
    (254 114 0  )
    (254 108 0  )
    (254 102 0  )
    (254 98  0  )
    (254 90  0  )
    (254 86  0  )
    (254 82  0  )
    (254 78  0  )
    (254 72  0  )
    (254 62  0  )
    (254 56  0  )
    (254 50  0  )
    (254 44  0  )
    (254 38  0  )
    (254 32  0  )
    (254 26  0  )
    (254 22  0  )
    (254 16  0  )
    (254 10  0  )
    (254 4   0  )
    (254 0   0  )
    ))
  
